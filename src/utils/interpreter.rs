//
// EPITECH PROJECT, 2023
// nebulang [WSL : Ubuntu]
// File description:
// interpreter
//

use std::{collections::HashMap, fmt::{Display, Debug}};

use super::{parser::{AstNode, BinaryOperator, TypeAnnotation}, Parser, Tokenizer};

#[derive(Debug, Clone, PartialEq)]

pub enum StackValue {
    Int(i32),
    Char(char),
    String(String),
    Bool(bool),
    Sequence(Vec<StackValue>),
    Void
}

impl StackValue {
    fn get_type_annotation(&self) -> TypeAnnotation {
        match self {
            StackValue::Int(_) => TypeAnnotation::Int,
            StackValue::Char(_) => TypeAnnotation::Char,
            StackValue::String(_) => TypeAnnotation::String,
            StackValue::Bool(_) => TypeAnnotation::Bool,
            StackValue::Sequence(values) => {
                if let Some(value) = values.first() {
                    TypeAnnotation::Sequence(Box::new(value.get_type_annotation()))
                } else {
                    TypeAnnotation::Sequence(Box::new(TypeAnnotation::Int))
                }
            },
            StackValue::Void => TypeAnnotation::Void
        }
    }

    fn match_type(&self, type_ant: &TypeAnnotation) -> bool {
        match (self, type_ant) {
            (StackValue::Int(_), TypeAnnotation::Int) => true,
            (StackValue::Char(_), TypeAnnotation::Char) => true,
            (StackValue::String(_), TypeAnnotation::String) => true,
            (StackValue::Bool(_), TypeAnnotation::Bool) => true,
            (StackValue::Sequence(values), TypeAnnotation::Sequence(inner_type)) => {
                values.iter().all(|value| value.match_type(inner_type))
            },
            _ => false
        }
    }
}

impl Display for StackValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StackValue::Int(value) => write!(f, "{}", value),
            StackValue::Char(value) => write!(f, "{}", value),
            StackValue::String(value) => write!(f, "{}", value),
            StackValue::Bool(value) => write!(f, "{}", value),
            StackValue::Sequence(values) => {
                write!(f, "[")?;
                for (i, value) in values.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", value)?;
                }
                write!(f, "]")
            },
            StackValue::Void => write!(f, "void")
        }
    }
}

#[derive(Clone)]
pub struct CallFrame {
    pub return_value: Option<StackValue>,
    return_flag: bool
}

impl CallFrame {
    pub fn new() -> Self {
        Self {
            return_value: None,
            return_flag: false
        }
    }
}

#[derive(Clone)]
pub struct ExperimentEnv {
    parameters: Vec<(String, TypeAnnotation)>,
    body: Option<Box<AstNode>>,
    builtin_exp: Option<fn(Vec<StackValue>) -> Result<StackValue, String>>
}

impl ExperimentEnv {
    pub fn new_builtin(exp: fn(Vec<StackValue>) -> Result<StackValue, String>) -> Self {
        Self {
            parameters: Vec::new(),
            body: None,
            builtin_exp: Some(exp)
        }
    }

    pub fn is_builtin(&self) -> bool {
        self.builtin_exp.is_some()
    }
}

#[derive(Clone)]
pub struct RuntimeEnv {
    stack: Vec<StackValue>,
    variables: HashMap<String, StackValue>,
    experiments: HashMap<String, ExperimentEnv>,
    pub call_stack: Vec<CallFrame>,
}

fn print_builtin(args: Vec<StackValue>) -> Result<StackValue, String> {
    for arg in args {
        println!("{:?}", arg);
    }
    Ok(StackValue::Void)
}

fn write_builtin(args: Vec<StackValue>) -> Result<StackValue, String> {
    for arg in args {
        if let StackValue::String(arg) = arg {
            print!("{}", arg);
        } else if let StackValue::Char(arg) = arg {
            print!("{}", arg);
        } else {
            return Err("Invalid argument type for Write experiment".to_string());
        }
    }
    Ok(StackValue::Void)
}

fn read_builtin(_: Vec<StackValue>) -> Result<StackValue, String> {
    let mut input = String::new();

    if let Err(e) = std::io::stdin().read_line(&mut input) {
        return Err(format!("Failed to read from stdin: {}", e));
    }
    Ok(StackValue::String(input.trim_end().to_string()))
}

fn len_builtin(args: Vec<StackValue>) -> Result<StackValue, String> {
    if args.len() != 1 {
        return Err("Len experiment expects exactly one argument".to_string());
    }

    match &args[0] {
        StackValue::String(s) => Ok(StackValue::Int(s.len() as i32)),
        StackValue::Sequence(seq) => Ok(StackValue::Int(seq.len() as i32)),
        _ => Err("Invalid argument type for Len experiment".to_string()),
    }
}

fn int_builtin(args: Vec<StackValue>) -> Result<StackValue, String> {
    if args.len() != 1 {
        return Err("Int experiment expects exactly one argument".to_string());
    }

    match &args[0] {
        StackValue::Char(c) => Ok(StackValue::Int(*c as i32)),
        StackValue::Bool(b) => Ok(StackValue::Int(*b as i32)),
        StackValue::String(s) => s.parse::<i32>().map(StackValue::Int).map_err(|_| "Invalid string for conversion to Int".to_string()),
        _ => Err("Invalid argument type for Int experiment".to_string()),
    }
}

fn char_builtin(args: Vec<StackValue>) -> Result<StackValue, String> {
    if args.len() != 1 {
        return Err("Char experiment expects exactly one argument".to_string());
    }

    match &args[0] {
        StackValue::Int(i) => {
            if let Some(c) = std::char::from_u32(*i as u32) {
                Ok(StackValue::Char(c))
            } else {
                Err("Invalid int for conversion to Char".to_string())
            }
        },
        _ => Err("Invalid argument type for Char experiment".to_string()),
    }
}

fn string_builtin(args: Vec<StackValue>) -> Result<StackValue, String> {
    if args.len() != 1 {
        return Err("String experiment expects exactly one argument".to_string());
    }

    match &args[0] {
        StackValue::Int(i) => Ok(StackValue::String(i.to_string())),
        StackValue::Char(c) => Ok(StackValue::String(c.to_string())),
        _ => Err("Invalid argument type for String experiment".to_string()),
    }
}

fn bool_builtin(args: Vec<StackValue>) -> Result<StackValue, String> {
    if args.len() != 1 {
        return Err("Bool experiment expects exactly one argument".to_string());
    }

    match &args[0] {
        StackValue::Int(i) => Ok(StackValue::Bool(*i != 0)),
        StackValue::Char(c) => Ok(StackValue::Bool(*c != '\0')),
        StackValue::String(s) => Ok(StackValue::Bool(!s.is_empty())),
        _ => Err("Invalid argument type for Bool experiment".to_string()),
    }
}

impl RuntimeEnv {
    pub fn new() -> Self {
        let mut env = Self {
            stack: Vec::new(),
            variables: HashMap::new(),
            experiments: HashMap::new(),
            call_stack: Vec::new()
        };

        env.add_builtins_experiments();
        env
    }

    fn add_builtins_experiments(&mut self) {
        self.experiments.insert("Print".to_string(), ExperimentEnv::new_builtin(print_builtin));
        self.experiments.insert("Write".to_string(), ExperimentEnv::new_builtin(write_builtin));
        self.experiments.insert("Read".to_string(), ExperimentEnv::new_builtin(read_builtin));
        self.experiments.insert("Len".to_string(), ExperimentEnv::new_builtin(len_builtin));
        self.experiments.insert("Int".to_string(), ExperimentEnv::new_builtin(int_builtin));
        self.experiments.insert("Char".to_string(), ExperimentEnv::new_builtin(char_builtin));
        self.experiments.insert("String".to_string(), ExperimentEnv::new_builtin(string_builtin));
        self.experiments.insert("Bool".to_string(), ExperimentEnv::new_builtin(bool_builtin));
    }

    pub fn set_experiment(&mut self, name: String, experiment: ExperimentEnv) {
        self.experiments.insert(name, experiment);
    }

    pub fn get_experiment(&self, name: &str) -> Option<ExperimentEnv> {
        self.experiments.get(name).cloned()
    }

    pub fn set_variable(&mut self, name: String, value: StackValue) {
        self.variables.insert(name, value);
    }

    pub fn get_variable(&self, name: &str) -> Option<StackValue> {
        self.variables.get(name).cloned()
    }

    pub fn is_truthy(&self) -> bool {
        match self.stack.last().cloned() {
            Some(StackValue::Int(value)) => value != 0,
            Some(StackValue::Char(value)) => value != '\0',
            Some(StackValue::String(value)) => !value.is_empty(),
            Some(StackValue::Bool(value)) => value,
            Some(StackValue::Sequence(value)) => !value.is_empty(),
            Some(StackValue::Void) => false,
            None => false
        }
    }

    pub fn get_return_value(&self) -> Option<StackValue> {
        self.call_stack.last().and_then(|frame| frame.return_value.clone())
    }

    pub fn push_frame(&mut self) {
        self.call_stack.push(CallFrame::new());
    }

    pub fn pop_frame(&mut self) -> Option<CallFrame> {
        self.call_stack.pop()
    }

    pub fn current_frame(&mut self) -> &mut CallFrame {
        self.call_stack.last_mut().expect("Call stack underflow")
    }
}

pub struct Interpreter;

impl Interpreter {

    fn check_type(&self, type_ant: &TypeAnnotation, value: &StackValue) -> bool {
        match (type_ant, value) {
            (TypeAnnotation::Int, StackValue::Int(_)) => true,
            (TypeAnnotation::Char, StackValue::Char(_)) => true,
            (TypeAnnotation::String, StackValue::String(_)) => true,
            (TypeAnnotation::Bool, StackValue::Bool(_)) => true,
            (TypeAnnotation::Sequence(inner_type), StackValue::Sequence(seq)) => {
                for item in seq {
                    if !self.check_type(inner_type, item) {
                        return false;
                    }
                }
                true
            },
            _ => false,
        }
    }

    pub fn interpret(&mut self, node: &AstNode, env: &mut RuntimeEnv) -> Result<(), String> {
        if let Some(frame) = env.call_stack.last() {
            if frame.return_flag {
                return Ok(());
            }
        }
        match node {
            AstNode::Program(stmts) => {
                for stmt in stmts {
                    self.interpret(stmt, env)?;
                }
            },
            AstNode::EmitStmt(expr) => {
                self.interpret(expr, env)?;
                if let Some(value) = env.stack.pop() {
                    let frame = env.current_frame();

                    frame.return_value = Some(value);
                } else {
                    return Err("Stack underflow when emitting".to_string());
                }
            },
            AstNode::IntLiteral(value) => {
                env.stack.push(StackValue::Int(*value));
            },
            AstNode::CharLiteral(value) => {
                env.stack.push(StackValue::Char(*value));
            },
            AstNode::StringLiteral(value) => {
                env.stack.push(StackValue::String(value.clone()));
            },
            AstNode::BoolLiteral(value) => {
                env.stack.push(StackValue::Bool(*value));
            },
            AstNode::SequenceLiteral(values) => {
                let mut array = Vec::new();
                let mut element_type: Option<TypeAnnotation> = None;

                for value in values {
                    self.interpret(value, env)?;
                    let val = env.stack.pop().ok_or("Stack underflow")?;

                    if let Some(ref expected_type) = element_type {
                        if !&val.match_type(expected_type) {
                            return Err(format!("Type mismatch in sequence literal: expected {:?}, got {:?}", expected_type, val.get_type_annotation()));
                        }
                    } else {
                        element_type = Some(val.get_type_annotation());
                    }

                    array.push(val);
                }

                env.stack.push(StackValue::Sequence(array));
            },
            AstNode::BinaryExpr(lhs, op, rhs) => {
                self.interpret(lhs, env)?;
                self.interpret(rhs, env)?;
                let rhs = env.stack.pop().ok_or("Stack underflow")?;
                let lhs = env.stack.pop().ok_or("Stack underflow")?;
                let result = match (lhs, op, rhs) {
                    (StackValue::Int(lhs), BinaryOperator::Add, StackValue::Int(rhs)) => StackValue::Int(lhs + rhs),
                    (StackValue::Int(lhs), BinaryOperator::Sub, StackValue::Int(rhs)) => StackValue::Int(lhs - rhs),
                    (StackValue::Int(lhs), BinaryOperator::Mul, StackValue::Int(rhs)) => StackValue::Int(lhs * rhs),
                    (StackValue::Int(lhs), BinaryOperator::Div, StackValue::Int(rhs)) => StackValue::Int(lhs / rhs),
                    (StackValue::Int(lhs), BinaryOperator::Mod, StackValue::Int(rhs)) => StackValue::Int(lhs % rhs),
                    (StackValue::Int(lhs), BinaryOperator::Pow, StackValue::Int(rhs)) => StackValue::Int(lhs.pow(rhs as u32)),
                    (StackValue::Int(lhs), BinaryOperator::Is, StackValue::Int(rhs)) => StackValue::Int((lhs == rhs) as i32),
                    (StackValue::Int(lhs), BinaryOperator::Not, StackValue::Int(rhs)) => StackValue::Int((lhs != rhs) as i32),
                    (StackValue::Int(lhs), BinaryOperator::Inf, StackValue::Int(rhs)) => StackValue::Int((lhs < rhs) as i32),
                    (StackValue::Int(lhs), BinaryOperator::Leq, StackValue::Int(rhs)) => StackValue::Int((lhs <= rhs) as i32),
                    (StackValue::Int(lhs), BinaryOperator::Sup, StackValue::Int(rhs)) => StackValue::Int((lhs > rhs) as i32),
                    (StackValue::Int(lhs), BinaryOperator::Geq, StackValue::Int(rhs)) => StackValue::Int((lhs >= rhs) as i32),
                    (StackValue::String(lhs), BinaryOperator::Add, StackValue::String(rhs)) => StackValue::String(lhs + rhs.as_str()),
                    (StackValue::String(lhs), BinaryOperator::Is, StackValue::String(rhs)) => StackValue::Int((lhs == rhs) as i32),
                    (StackValue::String(lhs), BinaryOperator::Not, StackValue::String(rhs)) => StackValue::Int((lhs != rhs) as i32),
                    (StackValue::Sequence(lhs), BinaryOperator::Add, StackValue::Sequence(rhs)) => StackValue::Sequence([lhs, rhs].concat()),
                    (StackValue::Sequence(lhs), BinaryOperator::Is, StackValue::Sequence(rhs)) => StackValue::Int((lhs == rhs) as i32),
                    (StackValue::Sequence(lhs), BinaryOperator::Not, StackValue::Sequence(rhs)) => StackValue::Int((lhs != rhs) as i32),
                    (StackValue::String(lhs), BinaryOperator::Mul, StackValue::Int(rhs)) => StackValue::String(lhs.repeat(rhs as usize)),
                    (StackValue::String(lhs), BinaryOperator::Add, StackValue::Char(rhs)) => StackValue::String(lhs + &rhs.to_string()),
                    _ => return Err("Invalid binary operation".to_string())
                };

                env.stack.push(result);
            },
            AstNode::AssignmentStmt(variable, expr) => {
                self.interpret(expr, env)?;
                if let AstNode::Identifier(name) = &**variable {
                    env.get_variable(name)
                        .ok_or(format!("Undefined variable: {}", name))?;
                    let value = env.stack.pop().ok_or("Stack underflow")?;
                    env.set_variable(name.clone(), value);
                } else {
                    return Err("Invalid left-hand side of assignment".to_string());
                }
            },
            AstNode::SetStmt(variable, type_ant, expr) => {
                self.interpret(expr, env)?;
                let value = env.stack.pop().ok_or("Stack underflow")?;

                if self.check_type(&type_ant, &value) {
                    env.set_variable(variable.clone(), value);
                } else {
                    return Err(format!("Type mismatch in variable assignment for '{}': expected {:?}, got {:?}", variable, type_ant, value.get_type_annotation()));
                }
            },
            AstNode::ScopeStmt(stmts) => {
                for stmt in stmts {
                    self.interpret(stmt, env)?;
                }
            },
            AstNode::IfStmt(cond, then_stmt, else_stmt) => {
                self.interpret(cond, env)?;
                let cond = env.stack.pop().ok_or("Stack underflow")?;

                if self.is_truthy(cond) {
                    self.interpret(then_stmt, env)?;
                } else if let Some(else_stmt) = else_stmt {
                    self.interpret(else_stmt, env)?;
                }
            },
            AstNode::UnlessStmt(cond, then_stmt, else_stmt) => {
                self.interpret(cond, env)?;
                let cond = env.stack.pop().ok_or("Stack underflow")?;

                if !self.is_truthy(cond) {
                    self.interpret(then_stmt, env)?;
                } else if let Some(else_stmt) = else_stmt {
                    self.interpret(else_stmt, env)?;
                }
            },
            AstNode::WhileStmt(cond, body) => {
                while {
                    self.interpret(cond, env)?;
                    env.is_truthy()
                } {
                    self.interpret(body, env)?;
                }
            },
            AstNode::IterStmt { variable, sequence, body } => {
                self.interpret(sequence, env)?;
                let sequence = match env.stack.pop().ok_or("Stack underflow")? {
                    StackValue::Sequence(sequence) => sequence,
                    StackValue::String(sequence) => sequence.chars().map(|c| StackValue::String(c.to_string())).collect(),
                    _ => return Err("Invalid sequence".to_string())
                };

                for item in sequence {
                    env.push_frame();
                    env.set_variable(variable.0.clone(), item);
                    self.interpret(body, env)?;
                    env.pop_frame();
                }
            },
            AstNode::ExperimentDecl { name, parameters, exp_type: _, body } => {
                let experiment = ExperimentEnv {
                    parameters: parameters.clone(),
                    body: Some(body.clone()),
                    builtin_exp: None
                };

                env.set_experiment(name.clone(), experiment);
            },
            AstNode::ExperimentCall { name, arguments } => {
                if let Some(experiment) = env.get_experiment(name) {
                    if experiment.is_builtin() {
                        let mut args = Vec::new();

                        for arg in arguments {
                            self.interpret(arg, env)?;
                            let arg = env.stack.pop().ok_or("Stack underflow")?;

                            args.push(arg);
                        }

                        let result = (experiment.builtin_exp.unwrap())(args)?;

                        env.stack.push(result);
                    } else {
                        if arguments.len() != experiment.parameters.len() {
                            return Err(format!("Invalid number of arguments for experiment '{}'", name));
                        }

                        env.push_frame();

                        for ((param_name, param_type), arg) in experiment.parameters.iter().zip(arguments.iter()) {
                            self.interpret(arg, env)?;
                            let arg = env.stack.pop().ok_or("Stack underflow")?;

                            if !arg.match_type(param_type) {
                                return Err(format!("Type mismatch for parameter '{}' in experiment '{}': expected {:?}, got {:?}", param_name, name, param_type, arg.get_type_annotation()));
                            }

                            env.set_variable(param_name.clone(), arg);
                        }

                        let body = experiment.body.clone().unwrap();

                        self.interpret(&body, env)?;

                        if let Some(frame) = env.pop_frame() {
                            if let Some(return_value) = frame.return_value {
                                env.stack.push(return_value);
                            }
                        }
                    }
                } else {
                    return Err(format!("Undefined experiment '{}'", name));
                }
            },
            AstNode::InjectStmt { experiments, file } => {
                let module_code = std::fs::read_to_string(format!("{}.nbl", file))
                    .map_err(|err| format!("Failed to load module '{}': {}", file, err))?;

                let mut tokenizer = Tokenizer::new(module_code.as_str());
                let mut tokens = Vec::new();

                while let Some(token) = tokenizer.next_token() {
                    match token {
                        Ok(token) => tokens.push(token),
                        Err(err) => {
                            return Err(format!("{}", err));
                        }
                    }
                }

                let module_ast = Parser::new(tokens)
                    .parse()
                    .map_err(|err| format!("{}", err))?;

                let mut found_experiments = Vec::new();

                if let AstNode::Program(stmts) = module_ast {
                    for stmt in stmts {
                        if let AstNode::ExperimentDecl { name, parameters, exp_type: _, body } = stmt {
                            if experiments.contains(&name) {
                                let experiment = ExperimentEnv {
                                    parameters,
                                    body: Some(body),
                                    builtin_exp: None
                                };
                                let name = &name;

                                env.set_experiment(name.to_string(), experiment);
                                found_experiments.push(name.to_string());
                            }
                        }
                    }
                } else {
                    return Err("Module did not parse into a program".to_string());
                }

                for experiment in experiments {
                    if !found_experiments.contains(&experiment) {
                        return Err(format!("Cannot find experiment '{}' in '{}'", experiment, file));
                    }
                }
            },
            AstNode::Identifier(name) => {
                let val = env.get_variable(name)
                    .ok_or(format!("Undefined variable: {}", name))?;
                env.stack.push(val);
            }
        }
        Ok(())
    }

    pub fn is_truthy(&self, value: StackValue) -> bool {
        match value {
            StackValue::Int(value) => value != 0,
            StackValue::Char(value) => value != '\0',
            StackValue::String(value) => !value.is_empty(),
            StackValue::Bool(value) => value,
            StackValue::Sequence(value) => !value.is_empty(),
            StackValue::Void => false
        }
    }

    pub fn load_module(&mut self, module_name: &str, env: &mut RuntimeEnv) -> Result<(), String> {
        let module_code = std::fs::read_to_string(format!("{}.nbl", module_name))
            .map_err(|err| format!("Failed to load module '{}': {}", module_name, err))?;

        let mut tokenizer = Tokenizer::new(module_code.as_str());
        let mut tokens = Vec::new();

        while let Some(token) = tokenizer.next_token() {
            match token {
                Ok(token) => tokens.push(token),
                Err(err) => {
                    return Err(format!("Failed to tokenize module '{}': {}", module_name, err));
                }
            }
        }

        let module_ast = Parser::new(tokens)
            .parse()
            .map_err(|err| format!("Failed to parse module '{}': {}", module_name, err))?;

        env.push_frame();

        let result = self.interpret(&module_ast, env);

        env.pop_frame();

        result
    }
}
