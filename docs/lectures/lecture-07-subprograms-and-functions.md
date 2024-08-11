# Lecture 07: Subprograms and Functions

## Introduction

In this lecture, we'll explore subprograms and functions in COBOL, which allow you to break down large programs into smaller, reusable components. Subprograms and functions promote modularity, improve code organization, and enhance code reusability.

## Subprograms

A subprogram is a self-contained block of code that performs a specific task. It can be called from other parts of the program to execute its functionality.

### Declaring Subprograms

Subprograms are declared using the `PROCEDURE DIVISION USING` clause.

**Syntax:**

```cobol
PROCEDURE DIVISION USING parameter-list.
    ...
    ...
    ...
    EXIT PROGRAM.
```

In this syntax:

- `parameter-list` is a list of parameters passed to the subprogram.

## Calling Subprograms

Subprograms are called using the `CALL` statement.

**Syntax:**

```cobol
CALL subprogram-name USING argument-list.
```

In this syntax:

- `subprogram-name` is the name of the subprogram to call.
- `argument-list` is a list of arguments passed to the subprogram.

### Example (Subprogram Declaration and Call)

```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. MAIN-PROGRAM.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 NUM1 PIC 9(2) VALUE 10.
    01 NUM2 PIC 9(2) VALUE 20.
    01 RESULT PIC 9(3).

PROCEDURE DIVISION.
    CALL 'ADD-SUBPROGRAM' USING NUM1, NUM2, RESULT.
    DISPLAY 'Result: ', RESULT.
    STOP RUN.

PROCEDURE DIVISION USING NUM1-IN, NUM2-IN, RESULT-OUT.
ADD-SUBPROGRAM.
    ADD NUM1-IN, NUM2-IN GIVING RESULT-OUT.
    EXIT PROGRAM.
```

In this example, the `ADD-SUBPROGRAM` subprogram adds two numbers and stores the result in the `RESULT-OUT` parameter. The main program calls the `ADD-SUBPROGRAM` subprogram and displays the result.

## Functions

Functions are subprograms that return a value. They are used to perform a specific computation and return the result to the calling program.

### Declaring Functions

Functions are declared using the `FUNCTION` clause.

**Syntax:**

```cobol
FUNCTION function-name RETURNING data-type.
    ...
    ...
    ...
    EXIT FUNCTION.
```

In this syntax:

- `function-name` is the name of the function.
- `data-type` is the data type of the value returned by the function.

## Calling Functions

Functions are called using the `EVALUATE` statement.

**Syntax:**

```cobol
EVALUATE function-name(parameter-list)
    WHEN value-1
        statements-1
    WHEN value-2
        statements-2
    ...
    WHEN OTHER
        default-statements
END-EVALUATE.
```

In this syntax:

- `function-name` is the name of the function to call.
- `parameter-list` is a list of parameters passed to the function.
- `value-1`, `value-2`, etc., are the possible return values of the function.

### Example (Function Declaration and Call)

```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. MAIN-PROGRAM.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 NUM1 PIC 9(2) VALUE 10.
    01 NUM2 PIC 9(2) VALUE 20.
    01 RESULT PIC 9(3).

PROCEDURE DIVISION.
    EVALUATE GET-SUM(NUM1, NUM2)
        WHEN 30
            DISPLAY 'Sum is 30'.
        WHEN OTHER
            DISPLAY 'Sum is not 30'.
    END-EVALUATE.
    STOP RUN.

FUNCTION GET-SUM RETURNING PIC 9(3).
    ADD NUM1, NUM2 GIVING RESULT.
    EXIT FUNCTION.
```

In this example, the `GET-SUM` function adds two numbers and returns the result. The main program calls the `GET-SUM` function and displays a message based on the return value.

## Conclusion

In this lecture, we explored subprograms and functions in COBOL. Subprograms and functions are essential programming constructs that promote code modularity and reusability. By breaking down large programs into smaller, manageable components, you can improve code organization and maintainability. Subprograms and functions allow you to encapsulate specific functionality and reuse it across different parts of your program.

### Next Steps

- Practice declaring and calling subprograms and functions in your COBOL programs.
- Experiment with passing parameters to subprograms and functions.
- Explore more advanced subprogram and function constructs in COBOL to enhance your programming skills.
