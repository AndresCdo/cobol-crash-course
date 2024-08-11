# Lecture 04: Conditional Statements

## Introduction

In this lecture, we'll explore conditional statements in COBOL, which allow your programs to make decisions based on specific conditions. Conditional statements are essential for creating dynamic and flexible programs that can handle different scenarios.

## The IF Statement

The `IF` statement is the most fundamental conditional statement in COBOL. It allows you to execute a block of code only if a certain condition is true.

**Syntax:**

```cobol
IF condition THEN
    statements
END-IF.
```

In this syntax:

- `condition` is the expression that is evaluated to determine if the statements should be executed.
- `statements` are the COBOL statements that are executed if the condition is true.

### Example (IF Statement)

```cobol
IF EMPLOYEE-AGE > 18 THEN
    DISPLAY "Employee is an adult."
END-IF.
```

In this example, the `DISPLAY` statement is executed only if the `EMPLOYEE-AGE` is greater than 18.

## The ELSE Statement

The `ELSE` statement can be used in conjunction with the `IF` statement to provide an alternative block of code to execute when the condition is false.

**Syntax:**

```cobol
IF condition THEN
    statements
ELSE
    else-statements
END-IF.
```

In this syntax:

- `condition` is the expression that is evaluated to determine which block of statements should be executed.
- `statements` are the COBOL statements that are executed if the condition is true.
- `else-statements` are the COBOL statements that are executed if the condition is false.

### Example (IF-ELSE Statement)

```cobol
IF EMPLOYEE-AGE > 18 THEN
    DISPLAY "Employee is an adult."
ELSE
    DISPLAY "Employee is a minor."
END-IF.
```

In this example, the `DISPLAY` statement inside the `ELSE` block is executed if the `EMPLOYEE-AGE` is not greater than 18.

## The EVALUATE Statement

The `EVALUATE` statement is a powerful conditional statement that allows you to evaluate multiple conditions and execute different blocks of code based on the results.

**Syntax:**

```cobol
EVALUATE expression
    WHEN condition-1
        statements-1
    WHEN condition-2
        statements-2
    ...
    WHEN OTHER
        default-statements
END-EVALUATE.
```

In this syntax:

- `expression` is the value that is evaluated to determine which block of statements should be executed.
- `condition-1`, `condition-2`, etc., are the conditions that are evaluated against the expression.
- `statements-1`, `statements-2`, etc., are the COBOL statements that are executed if the corresponding condition is true.
- `default-statements` are the COBOL statements that are executed if none of the conditions are true.

### Example (EVALUATE Statement)

```cobol
EVALUATE EMPLOYEE-AGE
    WHEN 18
        DISPLAY "Employee is 18 years old."
    WHEN 21
        DISPLAY "Employee is 21 years old."
    WHEN OTHER
        DISPLAY "Employee is neither 18 nor 21 years old."
END-EVALUATE.
```

In this example, the `DISPLAY` statement inside the `WHEN 18` block is executed if the `EMPLOYEE-AGE` is 18, and so on.

## Logical Operators

COBOL provides several logical operators that can be used to create complex conditions in conditional statements. The most common logical operators are:

- `AND`: Logical AND operator.
- `OR`: Logical OR operator.
- `NOT`: Logical NOT operator.

### Example (AND Operator)

```cobol
IF EMPLOYEE-AGE > 18 AND EMPLOYEE-SALARY < 50000 THEN
    DISPLAY "Employee is an adult with a salary less than $50,000."
END-IF.
```

In this example, the `DISPLAY` statement is executed only if the `EMPLOYEE-AGE` is greater than 18 and the `EMPLOYEE-SALARY` is less than $50,000.

## Nested Conditional Statements

You can nest conditional statements within each other to create more complex conditions and scenarios.

### Example (Nested IF Statements)

```cobol
IF EMPLOYEE-AGE > 18 THEN
    IF EMPLOYEE-SALARY < 50000 THEN
        DISPLAY "Employee is an adult with a salary less than $50,000."
    END-IF.
END-IF.
```

In this example, the `DISPLAY` statement is executed only if the `EMPLOYEE-AGE` is greater than 18 and the `EMPLOYEE-SALARY` is less than $50,000.

## Conclusion

Conditional statements are essential for controlling the flow of your COBOL programs based on specific conditions. By using `IF`, `ELSE`, and `EVALUATE` statements, you can create dynamic and flexible programs that can handle different scenarios effectively.

In the next lecture, we'll explore loops and iteration in COBOL, which allow you to repeat a block of code multiple times. Loops are useful for processing data, performing calculations, and iterating over arrays or tables.
