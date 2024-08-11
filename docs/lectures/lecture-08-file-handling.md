# Lecture 08: File Handling in COBOL

## Introduction

In this lecture, we'll explore file handling in COBOL, which allows you to interact with external data files. File handling is essential for storing, retrieving, and manipulating data in COBOL programs.

## File Organization

COBOL supports various file organizations, each with its own characteristics and advantages:

- **Sequential:** Data is stored in a linear sequence, accessed sequentially from the beginning.
- **Indexed Sequential:** Data is stored sequentially but can be accessed directly using an index.
- **Relative:** Data is stored in a table-like structure, accessed by record number.

## File Access Modes

COBOL provides different access modes for reading and writing data to files:

- **Sequential Access:** Data is read or written sequentially from the beginning of the file.
- **Random Access:** Data can be accessed directly by record number or key.

## File Operations

COBOL provides a set of statements for performing file operations, including:

- **OPEN:** Opens a file for reading or writing.
- **CLOSE:** Closes a file.
- **READ:** Reads data from a file.
- **WRITE:** Writes data to a file.
- **REWRITE:** Replaces an existing record in a file.
- **DELETE:** Deletes a record from a file.

## Example (Sequential File Access)

```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. FILE-HANDLING-EXAMPLE.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
    FILE-CONTROL.
        SELECT EMPLOYEE-FILE ASSIGN TO 'employees.dat'
            ORGANIZATION IS SEQUENTIAL
            ACCESS MODE IS SEQUENTIAL.

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
    OPEN INPUT EMPLOYEE-FILE.

    PERFORM READ-EMPLOYEE-RECORD UNTIL EOF.

    CLOSE EMPLOYEE-FILE.
    STOP RUN.

READ-EMPLOYEE-RECORD.
    READ EMPLOYEE-FILE
        AT END MOVE 'Y' TO EOF.

    IF NOT EOF THEN
        MOVE EMPLOYEE-ID TO WS-EMPLOYEE-ID
        MOVE EMPLOYEE-NAME TO WS-EMPLOYEE-NAME
        MOVE EMPLOYEE-SALARY TO WS-EMPLOYEE-SALARY
        DISPLAY 'Employee ID: ', WS-EMPLOYEE-ID
        DISPLAY 'Employee Name: ', WS-EMPLOYEE-NAME
        DISPLAY 'Employee Salary: ', WS-EMPLOYEE-SALARY
    END-IF.
```

In this example, we open a sequential file named `employees.dat` and read records from it until the end of the file is reached. Each record contains an employee ID, name, and salary.

## Example (Random File Access)

```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. FILE-HANDLING-EXAMPLE.

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

In this example, we open an indexed file named `employees.dat` and read a specific record with an employee ID of `12345`. If the record is found, we display the employee's name and salary.

## Conclusion

File handling is an essential aspect of COBOL programming, allowing you to interact with external data files efficiently. By understanding the different file organizations, access modes, and operations, you can effectively manage data in your COBOL programs.

### Next Steps

- Practice reading and writing data to different types of files in COBOL.
- Experiment with different file organizations and access modes to understand their advantages and limitations.
