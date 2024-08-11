# Lecture 06: Arrays and Tables

## Introduction

In this lecture, we'll explore arrays and tables in COBOL, which allow you to store and manipulate collections of data. Arrays and tables are essential for organizing and processing large amounts of data efficiently.

## Arrays

An array is a collection of elements of the same data type, stored in contiguous memory locations. Each element in an array is identified by its index, which is a numeric value starting from 1.

### Declaring Arrays

To declare an array in COBOL, you use the `OCCURS` clause in the `PIC` clause.

**Syntax:**

```cobol
01 ARRAY-NAME PIC data-type OCCURS n TIMES.
```

In this syntax:

- `ARRAY-NAME` is the name of the array.
- `data-type` is the data type of the elements in the array.
- `n` is the number of elements in the array.

### Example (Array Declaration)

```cobol
01 EMPLOYEE-NAMES PIC X(20) OCCURS 10 TIMES.
```

In this example, `EMPLOYEE-NAMES` is an array of 10 elements, each of which can store a string of up to 20 characters.

### Accessing Array Elements

You can access individual elements of an array using the index notation.

**Syntax:**

```cobol
ARRAY-NAME(index)
```

In this syntax:

- `ARRAY-NAME` is the name of the array.
- `index` is the numeric index of the element to access.

### Example (Array Access)

```cobol
MOVE 'John Doe' TO EMPLOYEE-NAMES(1).
MOVE 'Jane Doe' TO EMPLOYEE-NAMES(2).

DISPLAY EMPLOYEE-NAMES(1).
DISPLAY EMPLOYEE-NAMES(2).
```

In this example, we assign values to the first and second elements of the `EMPLOYEE-NAMES` array and then display their contents.

## Tables

A table is a collection of elements of different data types, stored in contiguous memory locations. Each element in a table is identified by its index, which is a numeric value starting from 1.

### Declaring Tables

To declare a table in COBOL, you use the `OCCURS` clause with the `DEPENDING ON` clause.

**Syntax:**

```cobol
01 TABLE-NAME.
    05 TABLE-ELEMENT PIC data-type OCCURS n TIMES DEPENDING ON table-size.
```

In this syntax:

- `TABLE-NAME` is the name of the table.
- `TABLE-ELEMENT` is the name of the element in the table.
- `data-type` is the data type of the elements in the table.
- `n` is the number of elements in the table.
- `table-size` is the variable that determines the size of the table at runtime.

### Example (Table Declaration)

```cobol
01 TABLE-SIZE PIC 9(2) VALUE 5.
01 EMPLOYEE-DATA.
    02 EMPLOYEE-ID PIC 9(5) OCCURS TABLE-SIZE TIMES DEPENDING ON TABLE-SIZE.
    02 EMPLOYEE-NAME PIC X(20) OCCURS TABLE-SIZE TIMES DEPENDING ON TABLE-SIZE.
```

In this example, `EMPLOYEE-DATA` is a table with two elements (`EMPLOYEE-ID` and `EMPLOYEE-NAME`), each occurring 5 times.

### Accessing Table Elements

You can access individual elements of a table using the index notation.

**Syntax:**

```cobol
TABLE-NAME(index)
```

In this syntax:

- `TABLE-NAME` is the name of the table.
- `index` is the numeric index of the element to access.

### Example (Table Access)

```cobol
MOVE 12345 TO EMPLOYEE-ID(1).
MOVE 'John Doe' TO EMPLOYEE-NAME(1).

DISPLAY EMPLOYEE-ID(1).
DISPLAY EMPLOYEE-NAME(1).
```

In this example, we assign values to the first element of the `EMPLOYEE-DATA` table and then display its contents.

## Conclusion

Arrays and tables are powerful data structures in COBOL that allow you to store and manipulate collections of data efficiently. By using arrays and tables, you can organize and process large amounts of data in your COBOL programs effectively.

### Next Steps

- Practice declaring and accessing arrays and tables in your COBOL programs.
- Experiment with different data types and sizes for arrays and tables to understand their behavior and limitations.
- Explore the COBOL documentation for more advanced array and table constructs and techniques.
