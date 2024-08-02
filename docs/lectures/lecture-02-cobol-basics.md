# Lecture 02: COBOL Basics

## Overview

In this lecture, we will cover the basic concepts and syntax of COBOL. This includes understanding the structure of a COBOL program, data types, variables, and basic operations.

## Structure of a COBOL Program

A COBOL program is divided into four main divisions:

1. **Identification Division**: Provides metadata about the program.
2. **Environment Division**: Specifies the environment in which the program will run.
3. **Data Division**: Defines the variables and data structures.
4. **Procedure Division**: Contains the code and logic of the program.

### Example

```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. BasicExample.

ENVIRONMENT DIVISION.

DATA DIVISION.

PROCEDURE DIVISION.
    DISPLAY 'This is a basic COBOL program'.
    STOP RUN.
```

## Data Types and Variables

COBOL supports various data types, including:

- **Numeric**: Used for numeric values.
- **Alphabetic**: Used for alphabetic characters.
- **Alphanumeric**: Used for a combination of alphabetic and numeric characters.
- **Databases**: Used for working with databases.

Variables in COBOL are defined using the `PIC` clause, which specifies the picture or format of the variable. For example:

```cobol
01 EMPLOYEE-ID PIC 9(5).
02 EMPLOYEE-NAME PIC X(20).
03 EMPLOYEE-SALARY PIC 9(6)V99.
```

In the above example:
- `EMPLOYEE-ID` is a numeric variable with a length of 5 digits.
- `EMPLOYEE-NAME` is an alphabetic variable with a length of 20 characters.
- `EMPLOYEE-SALARY` is a numeric variable with a length of 6 digits before the decimal point and 2 digits after the decimal point.

## Basic Operations

COBOL supports various arithmetic and logical operations, including:

- **ADD**: Adds two or more values.
- **SUBTRACT**: Subtracts one value from another.
- **MULTIPLY**: Multiplies two values.
- **DIVIDE**: Divides one value by another.
- **IF-ELSE**: Conditional statements for branching logic.

### Example

```cobol
01 NUM1 PIC 9(2) VALUE 10.
01 NUM2 PIC 9(2) VALUE 20.
01 RESULT PIC 9(3).

PROCEDURE DIVISION.
    ADD NUM1, NUM2 GIVING RESULT.
    DISPLAY 'Result: ', RESULT.
    STOP RUN.
```

In the above example, the `ADD` operation adds the values of `NUM1` and `NUM2` and stores the result in the `RESULT` variable.

## Conclusion

Understanding the basic concepts and syntax of COBOL is essential for developing COBOL programs. By mastering these fundamentals, you will be able to write and maintain COBOL code effectively.
