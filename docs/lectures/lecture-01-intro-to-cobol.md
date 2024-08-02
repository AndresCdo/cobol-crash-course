# Lecture 01: Introduction to COBOL

## Overview

COBOL (Common Business-Oriented Language) is a high-level programming language designed for business applications. It was first developed in the 1950s and has been widely used in various industries, particularly in finance and government sectors.

## History

- **1959**: COBOL was developed by CODASYL (Conference on Data Systems Languages).
- **1960s**: COBOL became widely adopted in business, finance, and administrative systems.
- **1970s-1980s**: Continued evolution with new standards and features.
- **1990s-Present**: COBOL remains in use, particularly in legacy systems.

## Key Features

- **English-like Syntax**: COBOL's syntax is designed to be readable and understandable.
- **Business-Oriented**: Optimized for business data processing.
- **Portability**: Can run on various hardware and operating systems.
- **Legacy Support**: Extensive use in legacy systems, particularly in financial institutions.

## Basic Structure of a COBOL Program

A COBOL program is divided into four divisions:

1. **Identification Division**: Provides metadata about the program.
2. **Environment Division**: Specifies the environment in which the program will run.
3. **Data Division**: Defines the variables and data structures.
4. **Procedure Division**: Contains the code and logic of the program.

### Example

```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. HelloWorld.

ENVIRONMENT DIVISION.

DATA DIVISION.

PROCEDURE DIVISION.
    DISPLAY 'Hello, World!'.
    STOP RUN.
```

## Conclusion

COBOL is a powerful and widely used language in the business world, particularly in legacy systems. Understanding COBOL can be valuable for developers working in industries that rely on this language.
