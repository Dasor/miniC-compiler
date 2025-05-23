# Compiler and flags
CXX := g++
CXXFLAGS := -std=c++17 -Wall -Wextra -g

# Output binary name
TARGET := comp

# Source files
SRCS := main.cpp lexer.cpp parser.cpp

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
