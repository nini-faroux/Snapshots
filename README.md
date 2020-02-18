<h2>Snapshots</h2>

Interpreter for a toy imperative language

<h4>Features</h4>

\* Step forward through the program's evaluation

\* Step backwards through the program's evaluation

\* View all previously evaluated Instructions

\* Inspect variable values at each previous step in the program

\* View the current environment (in-scope variables and their values)

\* Inspect a specific in-scope variable and its value

<h4>Build and Run</h4>

```bash
> stack setup

> stack build

> stack exec interpreter-exe
```

This will run with the default file. To specify another file run the following

```bash
> stack exec interpreter-exe <path-to-file>
```
