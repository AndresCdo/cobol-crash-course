# Lecture 05: Loops and Iteration

## Introduction

In this lecture, we'll explore loops in COBOL, which allow you to repeat a block of code multiple times. Loops are essential for processing data, performing calculations, and iterating over arrays or tables.

## The PERFORM Statement

The `PERFORM` statement is the primary way to create loops in COBOL. It allows you to execute a block of code a specified number of times or until a certain condition is met.

**Syntax:**

```cobol
PERFORM paragraph-name [TIMES integer] [UNTIL condition].
```

In this syntax:

- `paragraph-name` is the name of the paragraph containing the statements to be executed.
- `integer` is the number of times the loop should be executed.
- `condition` is the expression that is evaluated to determine if the loop should continue.

### Example (PERFORM Statement)

```cobol
PROCEDURE DIVISION.
    PERFORM DISPLAY-NUMBERS TIMES 5.

DISPLAY-NUMBERS.
    MOVE 1 TO COUNTER.
    DISPLAY COUNTER.
    ADD 1 TO COUNTER.
```

In this example, the `DISPLAY-NUMBERS` paragraph is executed 5 times, displaying the numbers 1 to 5.

## The PERFORM UNTIL Statement

The `PERFORM UNTIL` statement is used to create loops that continue until a certain condition is met. It allows you to repeat a block of code until the specified condition becomes true.

**Syntax:**

```cobol
PROCEDURE DIVISION.
    MOVE 1 TO COUNTER.
    PERFORM DISPLAY-NUMBERS UNTIL COUNTER > 10.

DISPLAY-NUMBERS.
    DISPLAY COUNTER.
    ADD 1 TO COUNTER.
```

In this example, the `DISPLAY-NUMBERS` paragraph is executed until the `COUNTER` variable is greater than 10.

## The PERFORM VARYING Statement

The `PERFORM VARYING` statement is used to create loops that iterate over a range of values. It allows you to specify a loop index variable and define the range of values that the variable should take.

**Syntax:**

```cobol
PROCEDURE DIVISION.
    PERFORM VARYING COUNTER FROM 1 BY 1 UNTIL COUNTER > 10
        DISPLAY COUNTER
    END-PERFORM.
```

In this example, the `COUNTER` variable is initialized to 1 and incremented by 1 in each iteration until it reaches 10.

## Conclusion

Loops are a fundamental concept in COBOL programming and are essential for processing data efficiently. By using loops, you can perform repetitive tasks, iterate over arrays or tables, and implement complex algorithms in your COBOL programs.

### Next Steps

- Practice using `PERFORM` and `PERFORM VARYING` statements in your COBOL programs.
- Experiment with nested loops to create more complex iteration patterns.
- Explore the COBOL documentation for more advanced loop constructs and techniques.
