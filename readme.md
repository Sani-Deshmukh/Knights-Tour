# ♞ Knight's Tour in OCaml

**A personal project to implement and explore Knight’s Tour puzzles, using Warnsdorff's Rule and OCaml modules.**

---

## 📌 Overview

The **Knight’s Tour** is a classic chess puzzle where a knight must visit every square on a chessboard (or an \( n \times m \) board) **exactly once**. In some versions (closed tours), the knight ends on the same square where it started, though that isn’t strictly required in an open tour.

This project provides:

1. **Board Representations** (imperative and functional)
2. **Warnsdorff’s Rule** – a simple heuristic that chooses the next move with the fewest subsequent moves, often leading to a complete tour
3. **Automated Solver** (`knights_tour`) to generate and validate tours
4. **Optional Interactive Mode** (`interact`) for manual exploration and debugging

---

## 🚀 Features

- **Warnsdorff’s Rule Implementation** for efficient Knight’s Tour solutions
- **Multiple Board Sizes** – from small boards up to larger grids
- **Imperative and Functional Board Options** – choose your preferred approach
- **Interactive Command-Line Interface** for manually placing knights
- **Unit Tests** for core modules (`storage`, `board`, `search`)

---

## ⚙️ Setup and Compilation

1. **Clone the Repository**  
   ```bash
   git clone https://github.com/your-username/ocaml-knights-tour.git
   cd ocaml-knights-tour

🎮 Running the Knight’s Tour Program
After compiling (make), run the knights_tour executable:

```
./knights_tour nrows ncols [options]
```

```
nrows ncols: Dimensions of the board (required).
-start m n: Start the knight at (m, n) (row m, column n) instead of (0,0).
-f: Use the functional board representation (default is imperative).
-q: Quiet mode – suppress the printed solution grid and only display success/failure.
```

Some example boards are given for testing and demo purposes. 

Interactive mode: 
```
make interact
./interact
```
