# SPICY Language

## ⚙️ How to Run the Test File

### ✅ Step 1: Ensure You Have OCaml Installed
Make sure you have `opam` and `OCaml` installed. Then install required packages:

```bash
opam install ocamlbuild menhir
```

---

### ✅ Step 2: Clean Build Directory

```bash
ocamlbuild -clean
```

---

### ✅ Step 3: Build the Project

```bash
ocamlbuild -use-menhir main.native
```

This compiles the parser, lexer, and main interpreter into an executable named `main.native`.

---

### ✅ Step 4: Run the SPICY Program

```bash
./main.native test2.spicy
```

This will parse and print a pretty-formatted representation of the program statements.

---

## 🗂 File Overview

- `ast.ml` – Abstract Syntax Tree definitions  
- `lexer.mll` – Lexical analyzer (tokenizes SPICY code)  
- `parser.mly` – Menhir parser for SPICY syntax  
- `main.ml` – Parses and prints the AST  
- `test2.spicy` – Sample test program in SPICY language  

---

## 🛠 TODO

- Add interpreter/evaluator for executing parsed programs  
- Support more advanced language features (types, scoping, etc.)
