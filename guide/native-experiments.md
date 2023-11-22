# 🎛 Native Experiments

NebuLang introduces a unique concept known as "Native Experiments" to expand its functionality without the need for traditional functions. These experiments provide powerful capabilities while maintaining the language's minimalistic approach. Here are the native experiments available in NebuLang:

### Print&#x20;

`exp Print(value: Any): Void`&#x20;

The `Print` experiment allows you to display output to the console. It is a versatile experiment that can print values of various data types, making it a handy tool for debugging and displaying information.

```javascript
Print("Hello, World!");       # Print a string
Print(42);                    # Print a integer
Print(true);                  # Print a boolean
```

### Write

`exp Write(str: String): Void`&#x20;

The `Write` experiment is specifically designed for printing strings on standard output.

```javascript
Write("This is a string.");    # Write a string
```

### Read

`exp Read(Void): String`&#x20;

The `Read` experiment enables you to read input from the standard input and return it as a string.

```javascript
set userInput: String = Read();  # Read user input
Print("You entered: " + userInput);
```

### Len

`exp Len(value: String | Seq[Any]): Int`

The `Len` experiment calculates and returns the length of a string or a sequence.

```javascript
set text: String = "NebuLang";
set numbers: Sequence[Int] = [1, 2, 3, 4, 5];

Print("Length of text: " + String(Len(text)));        # Print the length of the string
Print("Length of numbers: " + String(Len(numbers)));  # Print the length of the sequence
```

### Int

`exp Int(value: Char | String | Bool): Int`

The `Int` experiment allows you to convert data types such as characters, booleans, or strings into integers.

```javascript
set charToInt: Int = Int('A');            # Convert a character to an integer
set boolToInt: Int = Int(true);           # Convert a boolean to an integer
set stringToInt: Int = Int("42");         # Convert a string to an integer

Print(charToInt);
Print(boolToInt);
Print(stringToInt);
```

### Char

`exp Char(value: Int): Char`

The `Char` experiment enables you to convert integers into characters.

```javascript
set intToChar: Char = Char(65);  # Convert an integer to a character ('A')
Print(intToChar);
```

### String

`exp String(value: Int | Char): String`

The `String` experiment provides the ability to convert integers or characters into strings.

```javascript
set intToString: String = String(42);    # Convert an integer to a string
set charToString: String = String('X');  # Convert a character to a string

Write(intToString);
Write(charToString);
```

### Bool

`exp Bool(value: Int | Char | String): Bool`&#x20;

The `Bool` experiment facilitates the conversion of integers, characters, or strings into boolean values.

```javascript
set intToBool: Bool = Bool(0);            # Convert an integer to a boolean
set charToBool: Bool = Bool('A');         # Convert a character to a boolean
set stringToBool: Bool = Bool("true");    # Convert a string to a boolean

Print(intToBool);
Print(charToBool);
Print(stringToBool);
```

These examples demonstrate how to use each of the native experiments in NebuLang to perform various operations and conversions with different data types. You can use these experiments to simplify your code and work efficiently with data.
