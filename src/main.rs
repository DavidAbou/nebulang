//
// EPITECH PROJECT, 2023
// nebulang [WSL : Ubuntu]
// File description:
// main
//

use std::{io, process::exit};

use nebulang::utils::{Tokenizer, Parser, Interpreter, RuntimeEnv, StackValue};

fn main() -> io::Result<()> {
    let file = match std::env::args().nth(1) {
        Some(file) => file,
        None => {
            eprintln!("Usage: nebulang <file>");
            std::process::exit(84);
        }
    };

    let program = std::fs::read_to_string(file)?;

    let mut tokenizer = Tokenizer::new(program.as_str());
    let mut tokens = Vec::new();

    while let Some(token) = tokenizer.next_token() {
        match token {
            Ok(token) => tokens.push(token),
            Err(err) => {
                eprintln!("{}", err);
                exit(84)
            }
        }
    }

    let mut parser = Parser::new(tokens);

    match parser.parse() {
        Ok(ast) => {
            let mut interpreter = Interpreter;
            let mut env = RuntimeEnv::new();

            env.push_frame();

            match interpreter.interpret(&ast, &mut env) {
                Ok(_) => {
                    if let Some(frame) = env.call_stack.last() {
                        match frame.return_value {
                            Some(StackValue::Int(i)) => {
                                exit(i)
                            },
                            None => exit(0),
                            _ => {
                                eprintln!("Invalid return value");
                                exit(84)
                            }
                        }
                    } else {
                        exit(0)
                    }
                },
                Err(err) => {
                    eprintln!("{}", err);
                    exit(84)
                }
            }
        },
        Err(err) => {
            eprintln!("{}", err);
            exit(84)
        }
    }
}
