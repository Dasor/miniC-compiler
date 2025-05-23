# Compiler and flags
CXX := g++
CXXFLAGS := -std=c++17 -Wall -Wextra -g

# Output binary name
TARGET := lexer

# Source files
SRCS := main.cpp lexer.cpp

# Object files (replace .cpp with .o)
OBJS := $(SRCS:.cpp=.o)

# Default target
all: $(TARGET)

# Linking
$(TARGET): $(OBJS)
	$(CXX) $(CXXFLAGS) -o $@ $^

# Compilation
%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@

# Clean up build artifacts
clean:
	rm -f $(OBJS) $(TARGET)

.PHONY: all clean
