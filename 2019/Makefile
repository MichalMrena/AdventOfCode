DAY_FILE = $(DAY)_day.cpp
BIN_FILE = $(DAY)
COMMON_FLAGS = -std=c++20 -Wall -Wextra -Wpedantic -Wconversion
CXX = clang++-10

.PHONY: clean

release: $(DAY_FILE)
	mkdir -p ./build/release
	$(CXX) -o ./build/release/$(BIN_FILE) -O3 $(COMMON_FLAGS) $<

debug: $(DAY_FILE)
	mkdir -p ./build/debug
	$(CXX) -o ./build/debug/main_g -g $(COMMON_FLAGS) $<

clean:
	rm -r ./build