# Python Interpreter

## Overview
This project implements a Python interpreter in *Racket* programming language. The interpreter supports ***lazy evaluation*** for every assignment in the language, as well as lazy evaluation for multiple expressions.

## Features
- Supports lazy evaluation for every assignment: The interpreter evaluates the right-hand side of an assignment expression lazily, meaning that the value is only computed when it is actually needed.
- Supports lazy evaluation for multiple expressions: The interpreter allows multiple expressions to be evaluated lazily. This means that each expression is evaluated only when it is required, rather than evaluating all expressions at once.

## Prerequisites
- Racket programming language: The interpreter is written in Racket, so you need to have Racket installed on your machine to run the interpreter.

## Usage
1. Clone the repository to your local machine.
2. Open a terminal or command prompt.
3. Navigate to the directory where the repository is cloned.
4. Place your python code in test.py 
5. Run the following command to start the interpreter:

```bash   
   racket main.rkt
```   

5. The interpreter will start running, and you can start entering Python expressions to be evaluated. you can also import *evaluate* function and pass your python file locatoin to it.

## Examples
Here are some examples of Python expressions that can be evaluated using this interpreter:

1. Lazy evaluation for assignment:
   
```python
   x = 10;
   y = x + 5;
   print(y);  # Output: 15
```

In this example, the value of `x` is computed lazily when it is used in the assignment expression for `y`. The value of `y` is then printed.

2. Lazy evaluation for multiple expressions:
   
```python
   x = 10;
   y = 20;
   z = x + y;
   print(z);  # Output: 30
```

In this example, each assignment expression is evaluated lazily. The value of `x` and `y` is computed when they are used in the assignment expression for `z`. The value of `z` is then printed.

## Limitations
- The interpreter may not handle all Python language features and syntax. It is a simplified implementation and may not support advanced Python features.
- Lazy evaluation may introduce some overhead in terms of performance, compared to eager evaluation. This trade-off should be considered when using the interpreter.

## License
This project is licensed under the [MIT License](LICENSE).
