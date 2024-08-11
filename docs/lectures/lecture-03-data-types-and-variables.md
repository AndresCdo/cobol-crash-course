# Lecture 03: Data Types and Variables

## Introduction

In this lecture, we'll delve deeper into the fundamental building blocks of COBOL programs: data types and variables. Understanding how to define and manipulate data is crucial for writing effective and efficient COBOL code.

## Data Types

COBOL provides a rich set of data types to represent different kinds of information. These data types can be broadly classified into the following categories:

1. **Numeric**: Used for representing numeric values, such as integers and decimals.
2. **Alphabetic**: Used for representing alphabetic characters.
3. **Alphanumeric**: Used for representing a combination of alphabetic and numeric characters.
4. **Databases**: Used for working with databases.

### Numeric Data Types

Numeric data types in COBOL are used to store numeric values, including integers and decimals. The most common numeric data types in COBOL are:

- **Integer**: Used for whole numbers without decimal points.
- **Decimal**: Used for numbers with decimal points.

#### Integer Data Type

The `INTEGER` data type in COBOL is used to represent whole numbers without decimal points. It can be defined using the `PIC` clause with the format `9( n )`, where `n` is the number of digits in the integer.

For example:

```cobol
01 EMPLOYEE-ID PIC 9(5).
```

In this example, `EMPLOYEE-ID` is an integer variable with a length of 5 digits.

#### Decimal Data Type

The `DECIMAL` data type in COBOL is used to represent numbers with decimal points. It can be defined using the `PIC` clause with the format `9( n )V9( m )`, where `n` is the number of digits before the decimal point and `m` is the number of digits after the decimal point.

For example:

```cobol
01 EMPLOYEE-SALARY PIC 9(6)V99.
```

In this example, `EMPLOYEE-SALARY` is a decimal variable with a length of 6 digits before the decimal point and 2 digits after the decimal point.

### Alphabetic Data Type

Alphabetic data types in COBOL are used to store alphabetic characters. The most common alphabetic data type in COBOL is:

- **Alphabetic**: Used for single alphabetic characters.

Alphabetic variables can be defined using the `PIC` clause with the format `X( n )`, where `n` is the length of the variable.

For example:

```cobol
01 EMPLOYEE-NAME PIC X(20).
```

In this example, `EMPLOYEE-NAME` is an alphabetic variable with a length of 20 characters.

### Alphanumeric Data Type

Alphanumeric data types in COBOL are used to store a combination of alphabetic and numeric characters. The most common alphanumeric data type in COBOL is:

- **Alphanumeric**: Used for a combination of alphabetic and numeric characters.

Alphanumeric variables can be defined using the `PIC` clause with the format `A( n )`, where `n` is the length of the variable.

For example:

```cobol
01 EMPLOYEE-ADDRESS PIC A(50).
```

In this example, `EMPLOYEE-ADDRESS` is an alphanumeric variable with a length of 50 characters.

## Variables

Variables in COBOL are used to store and manipulate data within a program. They are defined using the `PIC` clause, which specifies the picture or format of the variable. Variables can be defined at different levels of the program hierarchy, such as at the `PROGRAM-ID` level or within individual data records.

### Variable Declaration

Variables in COBOL are declared using the `01` level number followed by the variable name and the `PIC` clause that defines the data type and length of the variable.

For example:

```cobol
01 EMPLOYEE-ID PIC 9(5).
01 EMPLOYEE-NAME PIC X(20).
01 EMPLOYEE-SALARY PIC 9(6)V99.
```

In this example, we have declared three variables:

- `EMPLOYEE-ID`: An integer variable with a length of 5 digits.
- `EMPLOYEE-NAME`: An alphabetic variable with a length of 20 characters.
- `EMPLOYEE-SALARY`: A decimal variable with a length of 6 digits before the decimal point and 2 digits after the decimal point.

### Variable Initialization

Variables in COBOL can be initialized with default values using the `VALUE` clause. This clause allows you to assign an initial value to a variable when it is declared.

For example:

```cobol
01 NUM1 PIC 9(2) VALUE 10.
01 NUM2 PIC 9(2) VALUE 20.
```

In this example, `NUM1` is initialized with the value `10`, and `NUM2` is initialized with the value `20`.

## Assigning Values to Variables

Values can be assigned to variables in COBOL using the `MOVE` statement. The `MOVE` statement copies the value of one variable to another variable.

For example:

```cobol
MOVE 100 TO NUM1.
```

In this example, the value `100` is assigned to the variable `NUM1`.

## Performing Arithmetic Operations

COBOL provides a set of arithmetic operations that can be used to perform calculations on numeric variables. These operations include:

- **ADD**: Adds two or more values.
- **SUBTRACT**: Subtracts one value from another.
- **MULTIPLY**: Multiplies two values.
- **DIVIDE**: Divides one value by another.

### Example
  
  ```cobol
  IDENTIFICATION DIVISION.
  PROGRAM-ID.  DATA-TYPES-EXAMPLE.

  DATA DIVISION.
  WORKING-STORAGE SECTION.
      01  NUMBER-1 PIC 9(4).
      01  NUMBER-2 PIC 9(4).
      01  SUM PIC 9(5).

  PROCEDURE DIVISION.
      MOVE 1234 TO NUMBER-1.
      MOVE 5678 TO NUMBER-2.

      COMPUTE SUM = NUMBER-1 + NUMBER-2.

      DISPLAY 'The sum of ' NUMBER-1 ' and ' NUMBER-2 ' is ' SUM.

      STOP RUN.
  ```

## Conclusion

Understanding data types and variables is essential for writing any COBOL program. By mastering these concepts, you can effectively represent and manipulate data to create powerful and efficient business applications.

### Next Steps

- Practice declaring variables and assigning values to them.
- Experiment with different data types and arithmetic operations.
- Explore the COBOL documentation for more advanced data types and operations.
