CXX = clang++
CXXFLAGS = -g -Wall -Wextra -pedantic-errors `../../llvm-12/bin/llvm-config --cxxflags --ldflags --libs --system-libs`

all: main

main: main.cpp
	$(CXX) $(CXXFLAGS) main.cpp -o main.out

clean:
	rm -rf main.out main.out.d*

