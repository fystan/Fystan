<h1><img src="fystan.ico" width="40" height="40" align="absmiddle"> Fystan</h1>

**Fystan** is a compiled programming language that blends the intuitive syntax of **Python** with the raw performance of **Rust** and **C**.  
It features **safe, efficient memory management** via **CTMM** (Compile Time Memory Management) and generates **native binaries** for multiple architectures.

> ⚡ Currently in **active development** – new features are added regularly.

---

## ✨ Features

- 🔨 **Compiled Language** – Generates native assembly for `amd64` and `arm64`, enabling **cross-compilation** to **Windows, Linux, Android, and WebAssembly (Wasm)**.
- 📝 **Simple, Modern Syntax** – Clean and beginner-friendly (perfectly same with Python)
- 🛡 **Safe Memory Management** – Powered by **CTMM**
- 📦 **Optimized Binaries** – Small, release-optimized output

---

## 📦 Installation

<h3>Download it on [Fystan Website](https://fystan.qzz.io).</h3>
---

## 🖥 Usage

To compile a Fystan file:

```bash
fystan build --target OS:architecture example.fys
```

**Example:**

```bash
fystan build --target windows:amd64 main.fys
```

The output will be a **native executable file** for your selected target architecture.
All compiler output and errors are in **English**.

### Supported Targets

The `fystan build` command supports cross-compilation to the following `OS:architecture` combinations:

*   `windows:amd64`
*   `windows:arm64`
*   `linux:amd64`
*   `linux:arm64`
*   `android:arm64`
*   `wasm:wasm32`
*   `wasm:wasm64`

---

## 🔍 CTMM – Compile Time Memory Management

**CTMM** is a memory management system that shifts all garbage collection decisions to **compile-time**, eliminating the need for runtime garbage collection.  
It enables developers to write code with automatic memory management comparable to Python or JavaScript without manual intervention.

### Key Features

- 🛡 **Compile-Time Determinism**  
  All object lifetimes are analyzed and deallocation points are inserted at compile time.  
  Prevents dangling pointers **before the code runs**.

- ⚡ **Zero Runtime GC**  
  The compiled binary contains only pure native code with direct deallocation calls, ensuring zero garbage collection pauses in 99% of cases.

- 👨‍💻 **Developer-Friendly**  
  Combines the ease of Python/JavaScript with the performance and predictability of C++.

---

## ⚙ How CTMM Works

The core of CTMM is **Compile-time Garbage Collection Elimination**:

1. **At compile-time**, the compiler performs static liveness and escape analysis to determine when each object is no longer needed.
2. **Deallocation instructions** (`free()` or arena resets) are inserted directly into the generated native code.
3. **Most short-lived objects** are placed on the stack or in arenas, avoiding heap allocation altogether.
4. **At runtime**, your program executes purely native code — no GC loop, no background threads.

✅ **No runtime garbage collector**  
✅ **No manual `free()` calls**  
✅ **Near-zero runtime overhead**

In rare cases where an object’s lifetime cannot be fully resolved at compile-time (such as dynamic cyclic references), CTMM triggers a **mini-GC**:  
It runs briefly to clean up unreachable cycles, then exits immediately, causing minimal pause times (microseconds to milliseconds).

---
