## llox = LLVM + lox
Lox language is a language from Crafting Interpreters however this is it's modification (we have no classes and mb smth more). Now it's static language.

### Examples:
```
fun read_int(): int;
fun print_int(v: int): nil;

fun gcd(a: int, b: int) : int {
   var temp: int;
   while ((a % b) > 0) {
      temp = a % b;
      a = b;
      b = temp;
   }
   return b;
}

fun main(): int {
   var n: int = read_int();
   print_int(gcd(228, n));
   return 0;
}
```

```
fun print_double(v: double) : nil;
fun sqrt(v: double) : double;

struct Point {
   var x: double;
   var y: double;
};

fun get_distance(p1: Point, p2: Point): double {
   var dx: double = p1.x - p2.x;
   var dy: double = p1.y - p2.y;
   return sqrt(dx*dx + dy*dy);
}

fun main() : int {
   var origin: Point;
   origin.x = 0;
   origin.y = 0;

   var p: Point;
   p.x = 20;
   p.y = 20;

   print_double(get_distance(origin, p));
   return 0;
}
```



## Speed comparison
```
Benchmark 1: ./llox_collatz 
  Time (mean ± σ):     135.6 µs ± 140.9 µs    [User: 42.5 µs, System: 88.6 µs]
  Range (min … max):     0.0 µs … 1280.3 µs    5000 runs
 
Benchmark 2: ./c_collatz
  Time (mean ± σ):     132.9 µs ± 146.4 µs    [User: 40.1 µs, System: 85.0 µs]
  Range (min … max):     0.0 µs … 1389.5 µs    5000 runs

Summary
  ./ccollatz ran
    1.02 ± 1.54 times faster than ./lcollatz
```
That's not suprising as we're doing O2 optimization passes.

### Usage:

```
./llox file.lox (here we get -> .s file)
clang file.s path_to_llox_repo/examples/llib/llib.c 
```

## Build
```
cd llox
mkdir build && cd build
cmake -DCMAKE_BUILD_TYPE=Release -DLLVM_DIR=../folder_with_llvm_build/lib/cmake/llvm \ 
-DCMAKE_INSTALL_PREFIX=../llox_install_folder ..

make
make install
```
