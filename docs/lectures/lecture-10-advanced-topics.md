# Lecture 10: Advanced Topics in COBOL

## Introduction

In this lecture, we'll explore some advanced topics in COBOL, delving into features and techniques that can enhance your programming capabilities and allow you to tackle more complex tasks.

## String Manipulation

COBOL provides a set of built-in functions for manipulating strings, including:

- **INSPECT:** Counts occurrences of specific characters or substrings within a string.
- **STRING:** Concatenates multiple strings into a single string.
- **UNSTRING:** Splits a string into multiple substrings based on delimiters.
- **REPLACING:** Replaces occurrences of a substring with another substring.

### Example (INSPECT)

```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. STRING-MANIPULATION-EXAMPLE.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 TEXT PIC X(20) VALUE 'Hello, World!'.
    01 COUNT PIC 9(2).

PROCEDURE DIVISION.
    INSPECT TEXT TALLYING COUNT FOR ALL 'l'.
    DISPLAY 'Count of "l": ', COUNT.
    STOP RUN.
```

In this example, the `INSPECT` statement counts the occurrences of the character `'l'` in the string `TEXT` and stores the count in the variable `COUNT`.

## Working with Databases

COBOL supports database operations through the use of embedded SQL statements. You can interact with relational databases, such as IBM Db2 or Oracle, directly from your COBOL programs. Database operations include:

- **SELECT:** Retrieves data from a database table.
- **INSERT:** Adds new records to a database table.
- **UPDATE:** Modifies existing records in a database table.
- **DELETE:** Removes records from a database table.

### Example (SELECT Statement)

```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. DATABASE-EXAMPLE.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
    FILE-CONTROL.
        SELECT EMPLOYEE-FILE ASSIGN TO 'employees.dat'
            ORGANIZATION IS INDEXED
            ACCESS MODE IS RANDOM
            RECORD KEY IS EMPLOYEE-ID.

DATA DIVISION.
FILE SECTION.
    FD EMPLOYEE-FILE.
        01 EMPLOYEE-RECORD.
            02 EMPLOYEE-ID PIC 9(5).
            02 EMPLOYEE-NAME PIC X(20).
            02 EMPLOYEE-SALARY PIC 9(6)V99.

WORKING-STORAGE SECTION.
    01 WS-EMPLOYEE-RECORD.
        02 WS-EMPLOYEE-ID PIC 9(5).
        02 WS-EMPLOYEE-NAME PIC X(20).
        02 WS-EMPLOYEE-SALARY PIC 9(6)V99.

PROCEDURE DIVISION.
    OPEN I-O EMPLOYEE-FILE.

    MOVE 12345 TO WS-EMPLOYEE-ID.
    READ EMPLOYEE-FILE INTO WS-EMPLOYEE-RECORD
        INVALID KEY DISPLAY 'Record not found'.

    IF NOT INVALID-KEY THEN
        DISPLAY 'Employee Name: ', WS-EMPLOYEE-NAME
        DISPLAY 'Employee Salary: ', WS-EMPLOYEE-SALARY
    END-IF.

    CLOSE EMPLOYEE-FILE.
    STOP RUN.
```

In this example, we open a random access file named `employees.dat` and retrieve a record with the employee ID `12345`. If the record is found, we display the employee's name and salary.

## Object-Oriented COBOL

While COBOL is traditionally a procedural language, some implementations support object-oriented programming (OOP) features. This allows you to:

- Define classes and objects.
- Encapsulate data and behavior within objects.
- Inherit properties and methods from parent classes.
- Implement interfaces and abstract classes.

### Example (Class Definition)

```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. OBJECT-ORIENTED-EXAMPLE.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 EMPLOYEE OBJECT.
        02 EMPLOYEE-ID PIC 9(5).
        02 EMPLOYEE-NAME PIC X(20).
        02 EMPLOYEE-SALARY PIC 9(6)V99.

PROCEDURE DIVISION.

    SET EMPLOYEE::EMPLOYEE-ID TO 12345.
    SET EMPLOYEE::EMPLOYEE-NAME TO 'John Doe'.
    SET EMPLOYEE::EMPLOYEE-SALARY TO 50000.00.

    DISPLAY 'Employee ID: ', EMPLOYEE::EMPLOYEE-ID.
    DISPLAY 'Employee Name: ', EMPLOYEE::EMPLOYEE-NAME.
    DISPLAY 'Employee Salary: ', EMPLOYEE::EMPLOYEE-SALARY.

    STOP RUN.
```

In this example, we define an `EMPLOYEE` class with data members for the employee ID, name, and salary. We then create an instance of the `EMPLOYEE` class and set its properties. Finally, we display the employee's information.

## COBOL in Modern Environments

COBOL continues to be widely used in various industries, including banking, insurance, and government. To adapt to modern computing environments, COBOL has evolved to support:

- Web services and APIs.
- Integration with cloud platforms.
- Mobile and web application development.
- Interoperability with other programming languages.

### Example (Web Service Integration)

```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. WEB-SERVICE-EXAMPLE.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 REQUEST PIC X(100).
    01 RESPONSE PIC X(100).

PROCEDURE DIVISION.
    MOVE 'https://api.example.com/data' TO REQUEST.
    CALL 'HTTP-GET' USING REQUEST, RESPONSE.
    DISPLAY 'Response: ', RESPONSE.
    STOP RUN.
```

In this example, we make an HTTP GET request to a web service endpoint and display the response.

## Conclusion

COBOL's versatility and adaptability make it a powerful language for a wide range of applications. By mastering advanced topics such as string manipulation, database operations, object-oriented programming, and modern integrations, you can leverage COBOL's strengths to build robust and scalable systems in today's digital landscape.

### Next Steps

- Explore more advanced COBOL features and techniques.
- Practice implementing complex algorithms and data structures in COBOL.
- Experiment with integrating COBOL applications with modern technologies and platforms.
- Stay up-to-date with the latest developments in the COBOL ecosystem.
