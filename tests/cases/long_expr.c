/* this is a multiline
comment that should be ignored
   and not cause any issues in the code */
// this is a single line comment

int main() {
    int a = 2 + 3 * 4;        // multiplication has higher precedence than addition
    int b = (2 + 3) * 4;      // parentheses should override that
    int c = 20 / 2 + 5;       // division before addition
    int d = 20 / (2 + 5);     // parentheses again change that
    int e = 10 - 3 - 2;       // left-associative subtraction
    int f = 10 - (3 - 2);     // parentheses override associativity

    return a + b + c + d + e + f;
}

