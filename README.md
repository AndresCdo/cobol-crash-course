# COBOL Learning Resources

This repository contains various resources for learning COBOL, including lectures, examples, and tools.

## Directory Structure

- **docs/**: Contains documentation and lecture notes.
  - **lectures/**: Lecture notes for different topics.
    - [Lecture 01: Introduction to COBOL](docs/lectures/lecture-01-intro-to-cobol.md)
    - [Lecture 02: COBOL Basics](docs/lectures/lecture-02-cobol-basics.md)
  - **notes/**: Additional notes and references.
  - **slides/**: Presentation slides for lectures.

- **examples/**: Contains example COBOL programs.
  - [hello-world.cbl](examples/hello-world.cbl): A simple "Hello, World!" program in COBOL.

- **resources/**: Additional resources for learning COBOL.

- **tools/**: Tools and scripts for working with COBOL.
  - **cobol-compiler/**: Scripts for compiling COBOL programs.
    - [build-cobol-binary.sh](tools/cobol-compiler/build-cobol-binary.sh): Script to build COBOL binaries.

## Getting Started

To get started with COBOL, you can begin by reading the lecture notes in the `docs/lectures/` directory. The first lecture provides an introduction to COBOL, its history, and its key features.

### Running the Hello World Example

1. Navigate to the `examples/` directory:

    ```sh
    cd examples
    ```

2. Compile the `hello-world.cbl` program using the COBOL compiler script:

    ```sh
    ../tools/cobol-compiler/build-cobol-binary.sh hello-world.cbl
    ```

3. Run the compiled binary:

    ```sh
    ./hello-world
    ```

You should see the output:
  
Hello, World!

## Contributing

If you would like to contribute to this repository, feel free to submit a pull request with your changes. You can contribute by adding new lectures, examples, tools, or resources related to COBOL.