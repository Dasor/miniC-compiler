#define CATCH_CONFIG_MAIN
#include "catch.hpp"

#include <filesystem>
#include <fstream>
#include <cstdlib>

namespace fs = std::filesystem;

int compile(const std::string &input_path)
{
    const std::string tmp_ir = "tmp.ll";

    // Run your compiler and output to tmp.ll
    std::string compile_cmd = "./comp <" + input_path + " 2> " + tmp_ir;
    int compile_result = std::system(compile_cmd.c_str());
    if (compile_result < 0)
    {
        std::cerr << "Compilation failed with error code: " << compile_result << std::endl;
    }

    return compile_result;
}

int run_test(const std::string &input_path)
{
    const std::string tmp_ir = "tmp.ll";
    compile(input_path);
    // Run lli on the generated IR
    std::string run_cmd = "lli " + tmp_ir;
    int run_result = std::system(run_cmd.c_str());

    // Clean up if needed (optional)
    std::remove(tmp_ir.c_str());

    // Extract and return actual exit code
    return WEXITSTATUS(run_result);
}


int read_expected(const std::string &file_path)
{
    std::ifstream f(file_path);
    int val;
    f >> val;
    return val;
}

TEST_CASE("Compiler handles basic arithmetic", "[compiler]")
{
    fs::path case_dir = "tests/cases/arith";
    fs::path expected_dir = "tests/expected/arith";

    for (const auto &entry : fs::directory_iterator(case_dir))
    {
        if (entry.path().extension() != ".c")
            continue;

        std::string name = entry.path().stem(); // e.g. "basic_add"
        fs::path expected_file = expected_dir / (name + ".txt");

        SECTION("Test case: " + name)
        {
            int expected = read_expected(expected_file);
            int actual = run_test(entry.path());
            REQUIRE(actual == expected);
        }
    }
}

// Check for correct compilation only
TEST_CASE("Correct compilation", "[compiler]")
{
    fs::path case_dir = "tests/cases/other";
    fs::path expected_dir = "tests/expected/other";

    for (const auto &entry : fs::directory_iterator(case_dir))
    {
        if (entry.path().extension() != ".c")
            continue;

        std::string name = entry.path().stem(); // e.g. "basic_add"

        SECTION("Test case: " + name)
        {
            int actual = compile(entry.path());
            REQUIRE(actual == 0);
        }
    }
}

TEST_CASE("Compiler handles control flow", "[compiler]")
{
    fs::path case_dir = "tests/cases/controlFlow";
    fs::path expected_dir = "tests/expected/controlFlow";

    for (const auto &entry : fs::directory_iterator(case_dir))
    {
        if (entry.path().extension() != ".c")
            continue;

        std::string name = entry.path().stem(); // e.g. "basic_add"
        fs::path expected_file = expected_dir / (name + ".txt");

        SECTION("Test case: " + name)
        {
            int expected = read_expected(expected_file);
            int actual = run_test(entry.path());
            REQUIRE(actual == expected);
        }
    }
}
