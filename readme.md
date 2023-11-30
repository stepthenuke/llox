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
