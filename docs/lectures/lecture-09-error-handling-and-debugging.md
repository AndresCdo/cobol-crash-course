# Lecture 09: Error Handling and Debugging

## Introduction

In this lecture, we'll explore error handling and debugging techniques in COBOL. These are essential skills for any COBOL programmer, as they allow you to identify and fix errors in your programs, ensuring they run smoothly and produce accurate results.

## Error Handling

Error handling is the process of anticipating and managing errors that may occur during program execution. COBOL provides various mechanisms for handling errors, including:

- **The `ON SIZE ERROR` Clause:** This clause is used to handle errors that occur when a numeric calculation results in an overflow or underflow.
- **The `INVALID KEY` Clause:** This clause is used to handle errors that occur when accessing a record in an indexed file that does not exist.
- **The `AT END` Clause:** This clause is used to handle errors that occur when reading a sequential file and reaching the end of the file.
- **The `USE AFTER ERROR` Clause:** This clause is used to execute a specific block of code when an error occurs during a file operation.

### Example (ON SIZE ERROR)

```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. ERROR-HANDLING-EXAMPLE.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 NUM1 PIC 9(4) VALUE 9999.
    01 NUM2 PIC 9(4) VALUE 1000.
    01 RESULT PIC 9(5).

PROCEDURE DIVISION.
    COMPUTE RESULT = NUM1 + NUM2 ON SIZE ERROR
        DISPLAY 'Error: Overflow occurred'.

    DISPLAY 'Result: ', RESULT.
    STOP RUN.
```

In this example, the `COMPUTE` statement adds two numbers (`NUM1` and `NUM2`) and stores the result in `RESULT`. If an overflow occurs during the calculation, the message `'Error: Overflow occurred'` is displayed.

## Debugging

Debugging is the process of identifying and fixing errors in a program. COBOL provides several debugging techniques to help you troubleshoot and resolve issues in your code, including:

- **Compiler Error Messages:** The COBOL compiler generates error messages when it encounters syntax errors or other issues in your program. These messages provide information about the error and its location in the code.
- **Debug Mode:** Some COBOL compilers offer a debug mode that allows you to step through your program line by line, inspecting variable values and program flow.
- **Display Statements:** You can use `DISPLAY` statements to output variable values and messages to the console, helping you track the program's execution and identify issues.

### Example (Debugging with DISPLAY Statements)

```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. DEBUGGING-EXAMPLE.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 NUM1 PIC 9(4) VALUE 10.
    01 NUM2 PIC 9(4) VALUE 20.
    01 RESULT PIC 9(5).

PROCEDURE DIVISION.
    ADD NUM1, NUM2 GIVING RESULT.
    DISPLAY 'Result: ', RESULT.
    STOP RUN.
```

In this example, we add two numbers (`NUM1` and `NUM2`) and store the result in `RESULT`. We then display the result using a `DISPLAY` statement to verify the calculation.

## Conclusion

Error handling and debugging are essential skills for COBOL programmers, allowing you to identify and fix issues in your programs effectively. By using error handling techniques and debugging tools, you can ensure your programs run smoothly and produce accurate results.

### Next Steps

- Practice error handling and debugging techniques in your COBOL programs.
- Experiment with different error scenarios to understand how to handle them effectively.
- Explore advanced debugging tools and techniques to improve your debugging skills.
- Compare this snippet from [Lecture 05: Loops and Iterations](lecture-05-loops-and-iterations.md):
  - `integer` is the number of times the loop should be executed.
  - `condition` is the expression that is evaluated to determine if the loop should continue.
