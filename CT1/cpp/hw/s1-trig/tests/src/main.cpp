#include "calc.hpp"
#include "gtest/gtest.h"

#include <cmath>
#include <ostream>
#include <sstream>
#include <type_traits>

template <typename F,
          std::enable_if_t<!std::is_same_v<decltype(std::declval<F>()()), void>, bool> = true>
decltype(std::declval<F>()()) captureOstreamString(std::ostream & os, std::string & output, F && func)
{
    std::stringstream buffer;
    std::streambuf * sbuf = os.rdbuf();
    os.rdbuf(buffer.rdbuf());
    auto result{func()};
    os.rdbuf(sbuf);
    output = buffer.str();
    return result;
}

template <typename F,
          std::enable_if_t<std::is_same_v<decltype(std::declval<F>()()), void>, bool> = true>
void captureOstreamString(std::ostream & os, std::string & output, F && func)
{
    std::stringstream buffer;
    std::streambuf * sbuf = os.rdbuf();
    os.rdbuf(buffer.rdbuf());
    func();
    os.rdbuf(sbuf);
    output = buffer.str();
}

double runWithCerrCapture(double value, std::string const & str, std::string & output)
{
    return captureOstreamString(std::cerr, output, [value, &str]() -> double {
        return process_line(value, str);
    });
}

#ifdef COMMON_TESTS

TEST(common, err)
{
    std::string cerrOutput;
    EXPECT_DOUBLE_EQ(0, runWithCerrCapture(0, "fix", cerrOutput));
    EXPECT_EQ("Unknown operation fix\n", cerrOutput);

    EXPECT_DOUBLE_EQ(11, runWithCerrCapture(11, "sqrt", cerrOutput));
    EXPECT_EQ("Unknown operation sqrt\n", cerrOutput);

    EXPECT_DOUBLE_EQ(17, runWithCerrCapture(17, "\\ 11", cerrOutput));
    EXPECT_EQ("Unknown operation \\ 11\n", cerrOutput);
}

TEST(common, set)
{
    std::string cerrOutput;
    EXPECT_DOUBLE_EQ(0, process_line(0, "0"));
    EXPECT_DOUBLE_EQ(0, process_line(0, "0000"));
    EXPECT_DOUBLE_EQ(0, process_line(101, "0"));
    EXPECT_DOUBLE_EQ(1, process_line(101, "01"));
    EXPECT_DOUBLE_EQ(2, process_line(101, "02"));
    EXPECT_DOUBLE_EQ(3, process_line(101, "0003"));
    EXPECT_DOUBLE_EQ(13, process_line(0, "13"));
    EXPECT_DOUBLE_EQ(5, process_line(99, "5."));
    EXPECT_DOUBLE_EQ(1099, process_line(99, "1099"));
    EXPECT_DOUBLE_EQ(0.127, process_line(99, "0.127"));
    EXPECT_DOUBLE_EQ(0.0101, process_line(99, "0.0101"));
    EXPECT_DOUBLE_EQ(0.05625, process_line(1113, "0.05625"));
    EXPECT_DOUBLE_EQ(1234567890.0, process_line(1, "1234567890"));

    EXPECT_DOUBLE_EQ(1, runWithCerrCapture(1, "12345678900000", cerrOutput));
    EXPECT_EQ("Argument isn't fully parsed, suffix left: '0000'\n", cerrOutput);

    EXPECT_DOUBLE_EQ(99, runWithCerrCapture(99, "5 ", cerrOutput));
    EXPECT_EQ("Argument parsing error at 1: ' '\n", cerrOutput);

    EXPECT_DOUBLE_EQ(0, runWithCerrCapture(0, "0.001 11", cerrOutput));
    EXPECT_EQ("Argument parsing error at 5: ' 11'\n", cerrOutput);

    EXPECT_DOUBLE_EQ(0, runWithCerrCapture(0, "123-456", cerrOutput));
    EXPECT_EQ("Argument parsing error at 3: '-456'\n", cerrOutput);
}

TEST(common, add)
{
    std::string cerrOutput;
    EXPECT_DOUBLE_EQ(7, process_line(0, "+7"));
    EXPECT_DOUBLE_EQ(7, process_line(5, "+ 2"));
    EXPECT_DOUBLE_EQ(7, process_line(5, "+ \t\t   2"));
    EXPECT_DOUBLE_EQ(2.34, process_line(1.5, "+ 0.84"));
    EXPECT_DOUBLE_EQ(9, runWithCerrCapture(9, "+    12345678900000", cerrOutput));
    EXPECT_EQ("Argument isn't fully parsed, suffix left: '0000'\n", cerrOutput);
    EXPECT_DOUBLE_EQ(99, runWithCerrCapture(99, "+ 1 ", cerrOutput));
    EXPECT_EQ("Argument parsing error at 3: ' '\n", cerrOutput);
    EXPECT_DOUBLE_EQ(5, runWithCerrCapture(5, "+ 1 2", cerrOutput));
    EXPECT_EQ("Argument parsing error at 3: ' 2'\n", cerrOutput);
    EXPECT_DOUBLE_EQ(0, runWithCerrCapture(0, "+0.001 11", cerrOutput));
    EXPECT_EQ("Argument parsing error at 6: ' 11'\n", cerrOutput);
    EXPECT_DOUBLE_EQ(99, runWithCerrCapture(99, "+ -", cerrOutput));
    EXPECT_EQ("Argument parsing error at 2: '-'\nNo argument for a binary operation\n", cerrOutput);
    EXPECT_DOUBLE_EQ(113, runWithCerrCapture(113, "+", cerrOutput));
    EXPECT_EQ("No argument for a binary operation\n", cerrOutput);
    EXPECT_DOUBLE_EQ(113, runWithCerrCapture(113, "+) 10", cerrOutput));
    EXPECT_EQ("Argument parsing error at 1: ') 10'\nNo argument for a binary operation\n", cerrOutput);
}

TEST(common, sub)
{
    std::string cerrOutput;
    EXPECT_DOUBLE_EQ(-11, process_line(0, "- 11"));
    EXPECT_DOUBLE_EQ(0, process_line(0, "-0"));
    EXPECT_DOUBLE_EQ(0, process_line(3, "-3"));
    EXPECT_DOUBLE_EQ(-3, process_line(7, "-10"));
    EXPECT_DOUBLE_EQ(-12344.6789, process_line(1, "- 12345.67890"));
    EXPECT_DOUBLE_EQ(77, runWithCerrCapture(77, "- 123a3", cerrOutput));
    EXPECT_EQ("Argument parsing error at 5: 'a3'\n", cerrOutput);
}

TEST(common, mul)
{
    EXPECT_DOUBLE_EQ(0, process_line(0, "* 0"));
    EXPECT_DOUBLE_EQ(0, process_line(0, "*131"));
    EXPECT_DOUBLE_EQ(0, process_line(99, "* 0"));
    EXPECT_DOUBLE_EQ(8, process_line(2, "* 4"));
    EXPECT_DOUBLE_EQ(-16, process_line(-4, "*4"));
}

TEST(common, div)
{
    std::string cerrOutput;
    EXPECT_DOUBLE_EQ(0, process_line(0, "/ 11"));
    EXPECT_DOUBLE_EQ(11, runWithCerrCapture(11, "/ 0", cerrOutput));
    EXPECT_EQ("Bad right argument for division: 0\n", cerrOutput);
    EXPECT_DOUBLE_EQ(3, process_line(6, "/ 2"));
    EXPECT_DOUBLE_EQ(0.7, process_line(7, "/ 10"));
    EXPECT_DOUBLE_EQ(0.3333333333333333, process_line(1, "/ 3"));
    EXPECT_DOUBLE_EQ(-0.5, process_line(-2, "/ 4"));
    EXPECT_DOUBLE_EQ(100, process_line(10, "/ 0.1"));
}

TEST(common, rem)
{
    std::string cerrOutput;
    EXPECT_DOUBLE_EQ(0, process_line(0, "% 3"));
    EXPECT_DOUBLE_EQ(0, process_line(4, "%4"));
    EXPECT_DOUBLE_EQ(0, process_line(-24, "%4"));
    EXPECT_DOUBLE_EQ(-3, process_line(-13, "%5"));
    EXPECT_DOUBLE_EQ(-1, runWithCerrCapture(-1, "%0", cerrOutput));
    EXPECT_EQ("Bad right argument for remainder: 0\n", cerrOutput);
}

TEST(common, neg)
{
    std::string cerrOutput;
    EXPECT_DOUBLE_EQ(0, process_line(0, "_"));
    EXPECT_DOUBLE_EQ(1, process_line(-1, "_"));
    EXPECT_DOUBLE_EQ(1, runWithCerrCapture(1, "_ ", cerrOutput));
    EXPECT_EQ("Unexpected suffix for a unary operation: ' '\n", cerrOutput);
    EXPECT_DOUBLE_EQ(1, runWithCerrCapture(1, "_1", cerrOutput));
    EXPECT_EQ("Unexpected suffix for a unary operation: '1'\n", cerrOutput);
}

TEST(common, pow)
{
    EXPECT_DOUBLE_EQ(0, process_line(0, "^1"));
    EXPECT_DOUBLE_EQ(0, process_line(0, "^2"));
    EXPECT_DOUBLE_EQ(1, process_line(119, "^0"));
    EXPECT_DOUBLE_EQ(37, process_line(37, "^1"));
    EXPECT_DOUBLE_EQ(25, process_line(-5, "^2"));
    EXPECT_DOUBLE_EQ(-27, process_line(-3, "^3"));
    EXPECT_DOUBLE_EQ(5, process_line(25, "^0.5"));
}

TEST(common, sqrt)
{
    std::string cerrOutput;
    EXPECT_DOUBLE_EQ(1, process_line(1, "SQRT"));
    EXPECT_DOUBLE_EQ(0.7, process_line(0.49, "SQRT"));
    EXPECT_DOUBLE_EQ(5, process_line(25, "SQRT"));
    EXPECT_DOUBLE_EQ(-1, runWithCerrCapture(-1, "SQRT", cerrOutput));
    EXPECT_EQ("Bad argument for SQRT: -1\n", cerrOutput);
}

#endif

#ifdef FAC_LOG_VARIANT

TEST(fac_log, fac)
{
    std::string cerrOutput;
    EXPECT_DOUBLE_EQ(6, process_line(3, "!"));
    EXPECT_DOUBLE_EQ(1, process_line(1, "!"));

    EXPECT_DOUBLE_EQ(-1, runWithCerrCapture(-1, "!", cerrOutput));
    EXPECT_EQ("Bad argument for factorial: -1\n", cerrOutput);

    EXPECT_DOUBLE_EQ(5.7, runWithCerrCapture(5.7, "!", cerrOutput));
    EXPECT_EQ("Bad argument for factorial: 5.7\n", cerrOutput);
    EXPECT_NEAR(3.04888e+029, process_line(28, "!"), 1.0e+25);

    EXPECT_NEAR(0.2, runWithCerrCapture(1000 - 999.8, "!", cerrOutput), 1e-13);
    EXPECT_TRUE(std::string_view{cerrOutput}.starts_with("Bad argument for factorial:"));

    EXPECT_NEAR(24, runWithCerrCapture((1000 - 999.8) + 3.8, "!", cerrOutput), 1e-13);
    EXPECT_TRUE(cerrOutput.empty());
}

TEST(fac_log, log)
{
    std::string cerrOutput;
    EXPECT_DOUBLE_EQ(3, process_line(8, "LOG 2"));
    EXPECT_DOUBLE_EQ(0.5, process_line(3, "LOG 9"));

    EXPECT_DOUBLE_EQ(-1, runWithCerrCapture(-1, "LOG 2", cerrOutput));
    EXPECT_EQ("Bad argument for logarithm: -1\n", cerrOutput);

    EXPECT_DOUBLE_EQ(8, runWithCerrCapture(8, "LOG 1", cerrOutput));
    EXPECT_EQ("Bad base for logarithm: 1\n", cerrOutput);
}

#endif

#ifdef FOLD_VARIANT

TEST(fold, noop)
{
    std::string cerrOutput;

    EXPECT_DOUBLE_EQ(3, runWithCerrCapture(3, "() 1 2 3", cerrOutput));
    EXPECT_FALSE(cerrOutput.empty());
}

TEST(fold, wrong_op)
{
    std::string cerrOutput;

    EXPECT_DOUBLE_EQ(3, runWithCerrCapture(3, "(_) 1 2 3", cerrOutput));
    EXPECT_FALSE(cerrOutput.empty());

    EXPECT_DOUBLE_EQ(9, runWithCerrCapture(9, "(1) 1 2 3", cerrOutput));
    EXPECT_FALSE(cerrOutput.empty());

    EXPECT_DOUBLE_EQ(11, runWithCerrCapture(11, "(() 1 2 3", cerrOutput));
    EXPECT_FALSE(cerrOutput.empty());

    EXPECT_DOUBLE_EQ(19, runWithCerrCapture(19, "(S) 5", cerrOutput));
    EXPECT_FALSE(cerrOutput.empty());
}

TEST(fold, no_args)
{
    std::string cerrOutput;

    EXPECT_DOUBLE_EQ(1, runWithCerrCapture(1, "(+)", cerrOutput));
    EXPECT_FALSE(cerrOutput.empty());

    EXPECT_DOUBLE_EQ(3, runWithCerrCapture(3, "(/)   ", cerrOutput));
    EXPECT_FALSE(cerrOutput.empty());
}

TEST(fold, wrong_brackets)
{
    std::string cerrOutput;

    EXPECT_DOUBLE_EQ(1, runWithCerrCapture(1, ")+( 1 2 3", cerrOutput));
    EXPECT_FALSE(cerrOutput.empty());

    EXPECT_DOUBLE_EQ(1, runWithCerrCapture(1, "((+) 1 2 3", cerrOutput));
    EXPECT_FALSE(cerrOutput.empty());
}

TEST(fold, add)
{
    std::string cerrOutput;
    EXPECT_DOUBLE_EQ(34579.8345, process_line(0, "(+) 1 2 34567.8345 9"));
    EXPECT_DOUBLE_EQ(11, process_line(10, "(+) 1"));
    EXPECT_DOUBLE_EQ(13, process_line(10, "(+) 1                                                                            2"));
    EXPECT_DOUBLE_EQ(2, process_line(1, "(+) 0 0 0 0 1"));
    EXPECT_DOUBLE_EQ(4.321, process_line(0, "(+) 0 1 1.1 1.11 1.111"));
    EXPECT_DOUBLE_EQ(0, process_line(-12, "(+) 1 1 2 3 5"));

    EXPECT_DOUBLE_EQ(37, runWithCerrCapture(37, "(+) 1 2 3 a 4 5 6", cerrOutput));
    EXPECT_FALSE(cerrOutput.empty());

    EXPECT_DOUBLE_EQ(11, runWithCerrCapture(11, "(+ ) 1", cerrOutput));
    EXPECT_FALSE(cerrOutput.empty());
}

TEST(fold, sub)
{
    std::string cerrOutput;

    EXPECT_DOUBLE_EQ(-34579.8345, process_line(0, "(-) 1 2 34567.8345 9"));
    EXPECT_DOUBLE_EQ(9, process_line(10, "(-) 1"));
    EXPECT_DOUBLE_EQ(0, process_line(1, "(-) 0 0 0 0 1"));
    EXPECT_DOUBLE_EQ(-4.321, process_line(0, "(-) 0 1 1.1 1.11 1.111"));
    EXPECT_DOUBLE_EQ(0, process_line(12, "(-)1 1 2 3 5 "));

    EXPECT_DOUBLE_EQ(11, runWithCerrCapture(11, "(- 1", cerrOutput));
    EXPECT_FALSE(cerrOutput.empty());

    EXPECT_DOUBLE_EQ(17, runWithCerrCapture(17, "(-_ 1", cerrOutput));
    EXPECT_FALSE(cerrOutput.empty());

    EXPECT_DOUBLE_EQ(3, runWithCerrCapture(3, "(-1", cerrOutput));
    EXPECT_FALSE(cerrOutput.empty());
}

TEST(fold, mul)
{
    std::string cerrOutput;
    EXPECT_DOUBLE_EQ(0, process_line(0, "(*) 1 2 3.333"));
    EXPECT_DOUBLE_EQ(119.988, process_line(1, "(*) 15 2.4 3.333"));
    EXPECT_DOUBLE_EQ(2, process_line(2, "(*) 0.5 2 0.5 2"));
    EXPECT_DOUBLE_EQ(1024, process_line(2, "(*) 4 8 16"));
    EXPECT_DOUBLE_EQ(15.001, process_line(150.01, "(*) 1 1 0.1 1"));
    EXPECT_DOUBLE_EQ(62.2, process_line(31.1, "(*)    1          2                             "));

    EXPECT_DOUBLE_EQ(2, runWithCerrCapture(2, "((*) 0.5 2", cerrOutput));
    EXPECT_FALSE(cerrOutput.empty());
}

TEST(fold, pow)
{
    std::string cerrOutput;
    EXPECT_DOUBLE_EQ(65536, process_line(2, "(^) 2 2 2 2"));
    EXPECT_DOUBLE_EQ(0, process_line(0, "(^) 3 5 10"));
    EXPECT_DOUBLE_EQ(1, process_line(1, "(^)10 100"));
    EXPECT_DOUBLE_EQ(1, process_line(42, "(^) 0 0 0 0 0"));
    EXPECT_DOUBLE_EQ(3723.875, process_line(15.5, "(^) 3 1 1 1"));

    EXPECT_DOUBLE_EQ(7, runWithCerrCapture(7, "((^)) 10 100", cerrOutput));
    EXPECT_FALSE(cerrOutput.empty());

    EXPECT_DOUBLE_EQ(1, runWithCerrCapture(1, "(^* 10 100", cerrOutput));
    EXPECT_FALSE(cerrOutput.empty());
}

TEST(fold, div)
{
    std::string cerrOutput;
    EXPECT_DOUBLE_EQ(2, process_line(1024, "(/) 4 8 16"));
    EXPECT_DOUBLE_EQ(128, process_line(2, "(/) 1 0.5 0.25 0.125"));
    EXPECT_DOUBLE_EQ(5, process_line(5, "(/) 1 1 1 1 1"));

    EXPECT_DOUBLE_EQ(5, runWithCerrCapture(5, "(/) 1,2,3,4,5", cerrOutput));
    EXPECT_FALSE(cerrOutput.empty());
    EXPECT_DOUBLE_EQ(10.533333333333333, process_line(1264, "(/) 12 4 5 0.5"));

    EXPECT_DOUBLE_EQ(11, runWithCerrCapture(11, "(/) 1 0", cerrOutput));
    EXPECT_EQ("Bad right argument for division: 0\n", cerrOutput);

    EXPECT_DOUBLE_EQ(13, runWithCerrCapture(13, "(/) 4 5 1xx", cerrOutput));
    EXPECT_FALSE(cerrOutput.empty());
}

TEST(fold, rem)
{
    std::string cerrOutput;
    EXPECT_DOUBLE_EQ(2, process_line(15, "(%) 8 5"));
    EXPECT_DOUBLE_EQ(3, process_line(469, "(%) 123 93 11 4"));
    EXPECT_DOUBLE_EQ(0, process_line(1173, "(%) 173 17 16"));
    EXPECT_DOUBLE_EQ(0, process_line(1173, "(%) 173 17 16 16"));

    EXPECT_DOUBLE_EQ(-1, runWithCerrCapture(-1, "(%)1 0", cerrOutput));
    EXPECT_EQ("Bad right argument for remainder: 0\n", cerrOutput);

    EXPECT_DOUBLE_EQ(7, runWithCerrCapture(7, "(((((%))))) 10 100", cerrOutput));
    EXPECT_FALSE(cerrOutput.empty());
}

#endif

#ifdef NUMBER_SYSTEM_VARIANT

TEST(number_system, bogus)
{
    std::string cerrOutput;

    EXPECT_DOUBLE_EQ(997, runWithCerrCapture(997, "b1101", cerrOutput));
    EXPECT_FALSE(cerrOutput.empty());

    EXPECT_DOUBLE_EQ(997, runWithCerrCapture(997, "0b", cerrOutput));
    EXPECT_FALSE(cerrOutput.empty());

    EXPECT_DOUBLE_EQ(997, runWithCerrCapture(997, "0b 1101", cerrOutput));
    EXPECT_FALSE(cerrOutput.empty());

    EXPECT_DOUBLE_EQ(997, runWithCerrCapture(997, "0b1101b", cerrOutput));
    EXPECT_FALSE(cerrOutput.empty());

    EXPECT_DOUBLE_EQ(997, runWithCerrCapture(997, "0b11012", cerrOutput));
    EXPECT_FALSE(cerrOutput.empty());

    EXPECT_DOUBLE_EQ(997, runWithCerrCapture(997, "0b1101.3", cerrOutput));
    EXPECT_FALSE(cerrOutput.empty());

    EXPECT_DOUBLE_EQ(997, runWithCerrCapture(997, "0b0.1101.1", cerrOutput));
    EXPECT_FALSE(cerrOutput.empty());

    EXPECT_DOUBLE_EQ(997, runWithCerrCapture(997, "0bb1101", cerrOutput));
    EXPECT_FALSE(cerrOutput.empty());

    EXPECT_DOUBLE_EQ(997, runWithCerrCapture(997, "1b1101", cerrOutput));
    EXPECT_FALSE(cerrOutput.empty());

    EXPECT_DOUBLE_EQ(997, runWithCerrCapture(997, "01b1101", cerrOutput));
    EXPECT_FALSE(cerrOutput.empty());

    EXPECT_DOUBLE_EQ(997, runWithCerrCapture(997, "01.0b101", cerrOutput));
    EXPECT_FALSE(cerrOutput.empty());

    EXPECT_DOUBLE_EQ(997, runWithCerrCapture(997, "0bFFF", cerrOutput));
    EXPECT_FALSE(cerrOutput.empty());

    EXPECT_DOUBLE_EQ(997, runWithCerrCapture(997, "03578", cerrOutput));
    EXPECT_FALSE(cerrOutput.empty());

    EXPECT_DOUBLE_EQ(997, runWithCerrCapture(997, "0 1", cerrOutput));
    EXPECT_FALSE(cerrOutput.empty());

    EXPECT_DOUBLE_EQ(997, runWithCerrCapture(997, "0357.8", cerrOutput));
    EXPECT_FALSE(cerrOutput.empty());

    EXPECT_DOUBLE_EQ(997, runWithCerrCapture(997, "0357.0b1", cerrOutput));
    EXPECT_FALSE(cerrOutput.empty());

    EXPECT_DOUBLE_EQ(997, runWithCerrCapture(997, "07.357.5", cerrOutput));
    EXPECT_FALSE(cerrOutput.empty());

    EXPECT_DOUBLE_EQ(997, runWithCerrCapture(997, "0b1+1", cerrOutput));
    EXPECT_FALSE(cerrOutput.empty());

    EXPECT_DOUBLE_EQ(997, runWithCerrCapture(997, "5EA", cerrOutput));
    EXPECT_FALSE(cerrOutput.empty());

    EXPECT_DOUBLE_EQ(997, runWithCerrCapture(997, "0xFG", cerrOutput));
    EXPECT_FALSE(cerrOutput.empty());

    EXPECT_DOUBLE_EQ(997, runWithCerrCapture(997, "0xh", cerrOutput));
    EXPECT_FALSE(cerrOutput.empty());

    EXPECT_DOUBLE_EQ(997, runWithCerrCapture(997, "0.xf", cerrOutput));
    EXPECT_FALSE(cerrOutput.empty());

    EXPECT_DOUBLE_EQ(997, runWithCerrCapture(997, "0.0xf", cerrOutput));
    EXPECT_FALSE(cerrOutput.empty());

    EXPECT_DOUBLE_EQ(997, runWithCerrCapture(997, "12a34", cerrOutput));
    EXPECT_FALSE(cerrOutput.empty());

    EXPECT_DOUBLE_EQ(997, runWithCerrCapture(997, "0a34", cerrOutput));
    EXPECT_FALSE(cerrOutput.empty());

    EXPECT_DOUBLE_EQ(997, runWithCerrCapture(997, "0ba34", cerrOutput));
    EXPECT_FALSE(cerrOutput.empty());

    EXPECT_DOUBLE_EQ(997, runWithCerrCapture(997, "01.0xab", cerrOutput));
    EXPECT_FALSE(cerrOutput.empty());

    EXPECT_DOUBLE_EQ(997, runWithCerrCapture(997, "011.FF", cerrOutput));
    EXPECT_FALSE(cerrOutput.empty());

    EXPECT_DOUBLE_EQ(997, runWithCerrCapture(997, "123x456", cerrOutput));
    EXPECT_FALSE(cerrOutput.empty());

    EXPECT_DOUBLE_EQ(997, runWithCerrCapture(997, "0aeb1", cerrOutput));
    EXPECT_FALSE(cerrOutput.empty());

    EXPECT_DOUBLE_EQ(997, runWithCerrCapture(997, "09123", cerrOutput));
    EXPECT_FALSE(cerrOutput.empty());

    EXPECT_DOUBLE_EQ(997, runWithCerrCapture(997, "0x aa", cerrOutput));
    EXPECT_FALSE(cerrOutput.empty());

    EXPECT_DOUBLE_EQ(997, runWithCerrCapture(997, "123.x", cerrOutput));
    EXPECT_FALSE(cerrOutput.empty());

    EXPECT_DOUBLE_EQ(997, runWithCerrCapture(997, "123.xaa", cerrOutput));
    EXPECT_FALSE(cerrOutput.empty());

    EXPECT_DOUBLE_EQ(997, runWithCerrCapture(997, "11xx", cerrOutput));
    EXPECT_FALSE(cerrOutput.empty());

    EXPECT_DOUBLE_EQ(997, runWithCerrCapture(997, "11x.", cerrOutput));
    EXPECT_FALSE(cerrOutput.empty());

    EXPECT_DOUBLE_EQ(997, runWithCerrCapture(997, ".0b11", cerrOutput));
    EXPECT_FALSE(cerrOutput.empty());

    EXPECT_DOUBLE_EQ(997, runWithCerrCapture(997, ".011", cerrOutput));
    EXPECT_FALSE(cerrOutput.empty());

    EXPECT_DOUBLE_EQ(997, runWithCerrCapture(997, ".0xaa", cerrOutput));
    EXPECT_FALSE(cerrOutput.empty());

    EXPECT_DOUBLE_EQ(997, runWithCerrCapture(997, "x111", cerrOutput));
    EXPECT_FALSE(cerrOutput.empty());

    EXPECT_DOUBLE_EQ(997, runWithCerrCapture(997, "12ff", cerrOutput));
    EXPECT_FALSE(cerrOutput.empty());

    EXPECT_DOUBLE_EQ(997, runWithCerrCapture(997, "+.0123", cerrOutput));
    EXPECT_FALSE(cerrOutput.empty());

    EXPECT_DOUBLE_EQ(997, runWithCerrCapture(997, "- x11", cerrOutput));
    EXPECT_FALSE(cerrOutput.empty());

    EXPECT_DOUBLE_EQ(997, runWithCerrCapture(997, "0x.3", cerrOutput));
    EXPECT_FALSE(cerrOutput.empty());
}

TEST(number_system, binary)
{
    EXPECT_DOUBLE_EQ(1, process_line(99, "0b1"));
    EXPECT_DOUBLE_EQ(1, process_line(99, "0b001"));
    EXPECT_DOUBLE_EQ(1, process_line(99, "0b1.00"));
    EXPECT_DOUBLE_EQ(0, process_line(99, "0b0"));
    EXPECT_DOUBLE_EQ(0, process_line(99, "0b0.00"));
    EXPECT_DOUBLE_EQ(5, process_line(99, "0b101"));
    EXPECT_DOUBLE_EQ(0.5, process_line(99, "0b0.1"));
    EXPECT_DOUBLE_EQ(0.5, process_line(99, "0B0.1"));
    EXPECT_DOUBLE_EQ(0.625, process_line(99, "0b0.101"));
    EXPECT_DOUBLE_EQ(13.75, process_line(0, "0b1101.1100"));
    EXPECT_DOUBLE_EQ(1.748, process_line(1.123, "+0b0.101"));
    EXPECT_DOUBLE_EQ(-1.4, process_line(0.1, "-0b1.1"));
    EXPECT_DOUBLE_EQ(1.5, process_line(2.25, "^0b0.1"));
}

TEST(number_system, octal)
{
    EXPECT_DOUBLE_EQ(0, process_line(111, "00"));
    EXPECT_DOUBLE_EQ(0, process_line(111, "00.0"));
    EXPECT_DOUBLE_EQ(1, process_line(111, "01"));
    EXPECT_DOUBLE_EQ(1, process_line(111, "01.000"));
    EXPECT_DOUBLE_EQ(8, process_line(111, "010"));
    EXPECT_DOUBLE_EQ(121, process_line(111, "0171"));
    EXPECT_DOUBLE_EQ(0.125, process_line(111, "00.1"));
    EXPECT_DOUBLE_EQ(1.015625, process_line(111, "01.01"));
    EXPECT_DOUBLE_EQ(1871.84375, process_line(0, "03517.66"));
    EXPECT_DOUBLE_EQ(11.703125, process_line(1, "+012.55"));
    EXPECT_DOUBLE_EQ(-9.703125, process_line(1, "-012.55"));
    EXPECT_DOUBLE_EQ(1.5, process_line(2.25, "^00.4"));
}

TEST(number_system, hexal)
{
    EXPECT_DOUBLE_EQ(0, process_line(1001, "0x0"));
    EXPECT_DOUBLE_EQ(0, process_line(1001, "0x000.0"));
    EXPECT_DOUBLE_EQ(1, process_line(1001, "0x1"));
    EXPECT_DOUBLE_EQ(1, process_line(1001, "0x01"));
    EXPECT_DOUBLE_EQ(1, process_line(1001, "0X01"));
    EXPECT_DOUBLE_EQ(1, process_line(1001, "0x001.000"));
    EXPECT_DOUBLE_EQ(0.0625, process_line(1001, "0x0.1"));
    EXPECT_DOUBLE_EQ(0.00390625, process_line(1001, "0x0.01"));
    EXPECT_DOUBLE_EQ(0.625, process_line(1001, "0x0.a"));
    EXPECT_DOUBLE_EQ(0.5, process_line(1001, "0x0.8"));
    EXPECT_DOUBLE_EQ(0.9375, process_line(1001, "0x0.F"));
    EXPECT_DOUBLE_EQ(13, process_line(1001, "0xd"));
    EXPECT_DOUBLE_EQ(14, process_line(1001, "0x00e"));
    EXPECT_DOUBLE_EQ(215.625, process_line(0, "0xD7.a"));
    EXPECT_DOUBLE_EQ(215.625, process_line(0, "0xd7.A"));
    EXPECT_DOUBLE_EQ(21.85, process_line(1.1, "+0x14.C"));
    EXPECT_DOUBLE_EQ(-16.0328125, process_line(1.1, "-0x11.22"));
    EXPECT_DOUBLE_EQ(1.5, process_line(2.25, "^0x0.8"));
}

#endif

#ifdef TRIG_VARIANT

namespace {

const double eps = 1e-15;

const double near_inf = 1.633123935319537e+16;

const double sqrt_3 = 1.7320508075688772935274463;
const double sqrt_3_over_3 = 1.7320508075688772935274463l / 3.0;
const double half_sqrt_3 = 1.7320508075688772935274463l / 2.0;
const double half_sqrt_2 = 0.707106781186547524400844;

const double pi = 3.141592653589793115997963468544185161590576171875;
const double half_pi = 1.5707963267948965579989817342720925807952880859375;
const double quarter_pi = 0.78539816339744827899949086713604629039764404296875;

} // anonymous namespace

TEST(trig, sin)
{
    bool rad_on = false;
    EXPECT_NEAR(0.5, process_line(30, rad_on, "SIN"), eps);
    EXPECT_NEAR(1, process_line(90, rad_on, "SIN"), eps);
    EXPECT_NEAR(-1, process_line(-90, rad_on, "SIN"), eps);
    EXPECT_NEAR(0, process_line(0, rad_on, "SIN"), eps);
    EXPECT_NEAR(-1, process_line(270, rad_on, "SIN"), eps);
    EXPECT_NEAR(0, process_line(180, rad_on, "SIN"), eps);
    EXPECT_NEAR(-half_sqrt_2, process_line(225, rad_on, "SIN"), eps);
    EXPECT_NEAR(-0.5, process_line(330, rad_on, "SIN"), eps);
    EXPECT_NEAR(0, process_line(360, rad_on, "SIN"), eps);
    EXPECT_NEAR(half_sqrt_3, process_line(480, rad_on, "SIN"), eps);
    EXPECT_DOUBLE_EQ(0, process_line(0, rad_on, "RAD"));
    ASSERT_TRUE(rad_on);
    EXPECT_NEAR(1, process_line(half_pi, rad_on, "SIN"), eps);
    EXPECT_NEAR(-1, process_line(-half_pi, rad_on, "SIN"), eps);
    EXPECT_NEAR(0, process_line(0, rad_on, "SIN"), eps);
    EXPECT_NEAR(1e-1, process_line(1e-1, rad_on, "SIN"), 1e-2);
    EXPECT_NEAR(1e-2, process_line(1e-2, rad_on, "SIN"), 1e-3);
    EXPECT_NEAR(0, process_line(pi * 2, rad_on, "SIN"), eps);
    EXPECT_NEAR(0, process_line(pi * 4, rad_on, "SIN"), eps);
    EXPECT_NEAR(0.5, process_line(pi * 5 / 6, rad_on, "SIN"), eps);
    EXPECT_NEAR(-half_sqrt_2, process_line(pi * 7 / 4, rad_on, "SIN"), eps);
    EXPECT_DOUBLE_EQ(0, process_line(0, rad_on, "DEG"));
    ASSERT_FALSE(rad_on);
    EXPECT_NEAR(1, process_line(90, rad_on, "SIN"), eps);
    EXPECT_NEAR(-1, process_line(-90, rad_on, "SIN"), eps);
    EXPECT_NEAR(0, process_line(0, rad_on, "SIN"), eps);
    EXPECT_NEAR(0, process_line(180, rad_on, "SIN"), eps);
    EXPECT_NEAR(-0.5, process_line(210, rad_on, "SIN"), eps);
    EXPECT_NEAR(-1, process_line(270, rad_on, "SIN"), eps);
    EXPECT_NEAR(0, process_line(360, rad_on, "SIN"), eps);
}

TEST(trig, cos)
{
    bool rad_on = false;
    EXPECT_NEAR(0, process_line(90, rad_on, "COS"), eps);
    EXPECT_NEAR(1, process_line(0, rad_on, "COS"), eps);
    EXPECT_NEAR(half_sqrt_3, process_line(30, rad_on, "COS"), eps);
    EXPECT_NEAR(0.5, process_line(60, rad_on, "COS"), eps);
    EXPECT_NEAR(-1, process_line(180, rad_on, "COS"), eps);
    EXPECT_NEAR(1, process_line(360, rad_on, "COS"), eps);
    EXPECT_DOUBLE_EQ(0, process_line(0, rad_on, "RAD"));
    ASSERT_TRUE(rad_on);
    EXPECT_NEAR(0, process_line(half_pi, rad_on, "COS"), eps);
    EXPECT_NEAR(1, process_line(0, rad_on, "COS"), eps);
    EXPECT_NEAR(-1, process_line(pi, rad_on, "COS"), eps);
    EXPECT_NEAR(1, process_line(pi * 2, rad_on, "COS"), eps);
    EXPECT_NEAR(-half_sqrt_2, process_line(pi * 3 / 4, rad_on, "COS"), eps);
    EXPECT_NEAR(half_sqrt_3, process_line(pi * 11 / 6, rad_on, "COS"), eps);
    EXPECT_DOUBLE_EQ(0, process_line(0, rad_on, "DEG"));
    ASSERT_FALSE(rad_on);
    EXPECT_NEAR(0, process_line(90, rad_on, "COS"), eps);
    EXPECT_NEAR(1, process_line(0, rad_on, "COS"), eps);
    EXPECT_NEAR(-0.5, process_line(120, rad_on, "COS"), eps);
    EXPECT_NEAR(-1, process_line(180, rad_on, "COS"), eps);
    EXPECT_NEAR(-half_sqrt_3, process_line(210, rad_on, "COS"), eps);
    EXPECT_NEAR(-0.5, process_line(240, rad_on, "COS"), eps);
    EXPECT_NEAR(0, process_line(270, rad_on, "COS"), eps);
    EXPECT_NEAR(0.5, process_line(300, rad_on, "COS"), eps);
    EXPECT_NEAR(half_sqrt_3, process_line(330, rad_on, "COS"), eps);
    EXPECT_NEAR(1, process_line(360, rad_on, "COS"), eps);
}

TEST(trig, tan)
{
    bool rad_on = false;
    EXPECT_NEAR(1, process_line(45, rad_on, "TAN"), eps);
    EXPECT_NEAR(-1, process_line(-45, rad_on, "TAN"), eps);
    EXPECT_NEAR(0, process_line(0, rad_on, "TAN"), eps);
    EXPECT_NEAR(sqrt_3_over_3, process_line(30, rad_on, "TAN"), eps);
    EXPECT_NEAR(sqrt_3, process_line(60, rad_on, "TAN"), eps);
    EXPECT_NEAR(0, process_line(180, rad_on, "TAN"), eps);
    EXPECT_NEAR(0, process_line(360, rad_on, "TAN"), eps);
    EXPECT_DOUBLE_EQ(near_inf, process_line(90, rad_on, "TAN"));
    EXPECT_DOUBLE_EQ(0, process_line(0, rad_on, "RAD"));
    ASSERT_TRUE(rad_on);
    EXPECT_NEAR(1, process_line(quarter_pi, rad_on, "TAN"), eps);
    EXPECT_NEAR(-1, process_line(-quarter_pi, rad_on, "TAN"), eps);
    EXPECT_NEAR(0, process_line(0, rad_on, "TAN"), eps);
    EXPECT_NEAR(0, process_line(pi, rad_on, "TAN"), eps);
    EXPECT_NEAR(0, process_line(pi * 2, rad_on, "TAN"), eps);
    EXPECT_NEAR(sqrt_3_over_3, process_line(pi * 7 / 6, rad_on, "TAN"), eps);
    EXPECT_DOUBLE_EQ(near_inf, process_line(half_pi, rad_on, "TAN"));
    EXPECT_DOUBLE_EQ(-near_inf, process_line(-half_pi, rad_on, "TAN"));
    EXPECT_DOUBLE_EQ(0, process_line(0, rad_on, "DEG"));
    ASSERT_FALSE(rad_on);
    EXPECT_NEAR(1, process_line(45, rad_on, "TAN"), eps);
    EXPECT_NEAR(-1, process_line(-45, rad_on, "TAN"), eps);
    EXPECT_NEAR(0, process_line(0, rad_on, "TAN"), eps);
    EXPECT_NEAR(-1, process_line(135, rad_on, "TAN"), eps);
    EXPECT_NEAR(0, process_line(180, rad_on, "TAN"), eps);
    EXPECT_NEAR(1, process_line(225, rad_on, "TAN"), eps);
    EXPECT_NEAR(-1, process_line(315, rad_on, "TAN"), eps);
    EXPECT_NEAR(0, process_line(360, rad_on, "TAN"), eps);
}

TEST(trig, ctn)
{
    bool rad_on = false;
    EXPECT_NEAR(1, process_line(45, rad_on, "CTN"), eps);
    EXPECT_NEAR(sqrt_3, process_line(30, rad_on, "CTN"), eps);
    EXPECT_TRUE(std::isinf(process_line(0, rad_on, "CTN")));
    EXPECT_NEAR(0, process_line(90, rad_on, "CTN"), eps);
    EXPECT_DOUBLE_EQ(0, process_line(0, rad_on, "RAD"));
    ASSERT_TRUE(rad_on);
    EXPECT_NEAR(1, process_line(quarter_pi, rad_on, "CTN"), eps);
    EXPECT_NEAR(sqrt_3, process_line(pi / 6, rad_on, "CTN"), eps);
    EXPECT_TRUE(std::isinf(process_line(0, rad_on, "CTN")));
    EXPECT_NEAR(0, process_line(half_pi, rad_on, "CTN"), eps);
    EXPECT_NEAR(1, process_line(quarter_pi * 5, rad_on, "CTN"), eps);
    EXPECT_NEAR(-sqrt_3_over_3, process_line(pi * 5 / 3, rad_on, "CTN"), eps);
    EXPECT_DOUBLE_EQ(0, process_line(0, rad_on, "DEG"));
    ASSERT_FALSE(rad_on);
    EXPECT_NEAR(1, process_line(45, rad_on, "CTN"), eps);
    EXPECT_NEAR(sqrt_3, process_line(30, rad_on, "CTN"), eps);
    EXPECT_NEAR(sqrt_3_over_3, process_line(60, rad_on, "CTN"), eps);
    EXPECT_TRUE(std::isinf(process_line(0, rad_on, "CTN")));
    EXPECT_NEAR(0, process_line(90, rad_on, "CTN"), eps);
}

TEST(trig, asin)
{
    bool rad_on = false;
    EXPECT_DOUBLE_EQ(-90, process_line(-1, rad_on, "ASIN"));
    EXPECT_DOUBLE_EQ(-30, process_line(-0.5, rad_on, "ASIN"));
    EXPECT_DOUBLE_EQ(0, process_line(0, rad_on, "ASIN"));
    EXPECT_DOUBLE_EQ(30, process_line(0.5, rad_on, "ASIN"));
    EXPECT_DOUBLE_EQ(90, process_line(1, rad_on, "ASIN"));
    EXPECT_DOUBLE_EQ(0, process_line(0, rad_on, "RAD"));
    ASSERT_TRUE(rad_on);
    EXPECT_DOUBLE_EQ(-half_pi, process_line(-1, rad_on, "ASIN"));
    EXPECT_DOUBLE_EQ(-pi / 6, process_line(-0.5, rad_on, "ASIN"));
    EXPECT_DOUBLE_EQ(0, process_line(0, rad_on, "ASIN"));
    EXPECT_DOUBLE_EQ(pi / 6, process_line(0.5, rad_on, "ASIN"));
    EXPECT_DOUBLE_EQ(half_pi, process_line(1, rad_on, "ASIN"));
    EXPECT_DOUBLE_EQ(0, process_line(0, rad_on, "DEG"));
    ASSERT_FALSE(rad_on);
    EXPECT_DOUBLE_EQ(-90, process_line(-1, rad_on, "ASIN"));
    EXPECT_DOUBLE_EQ(-30, process_line(-0.5, rad_on, "ASIN"));
    EXPECT_DOUBLE_EQ(0, process_line(0, rad_on, "ASIN"));
    EXPECT_DOUBLE_EQ(30, process_line(0.5, rad_on, "ASIN"));
    EXPECT_DOUBLE_EQ(90, process_line(1, rad_on, "ASIN"));
}

TEST(trig, acos)
{
    bool rad_on = false;
    EXPECT_DOUBLE_EQ(180, process_line(-1, rad_on, "ACOS"));
    EXPECT_DOUBLE_EQ(0, process_line(1, rad_on, "ACOS"));
    EXPECT_DOUBLE_EQ(90, process_line(0, rad_on, "ACOS"));
    EXPECT_DOUBLE_EQ(0, process_line(0, rad_on, "RAD"));
    ASSERT_TRUE(rad_on);
    EXPECT_DOUBLE_EQ(pi, process_line(-1, rad_on, "ACOS"));
    EXPECT_DOUBLE_EQ(0, process_line(1, rad_on, "ACOS"));
    EXPECT_DOUBLE_EQ(half_pi, process_line(0, rad_on, "ACOS"));
    EXPECT_DOUBLE_EQ(0, process_line(0, rad_on, "DEG"));
    ASSERT_FALSE(rad_on);
    EXPECT_DOUBLE_EQ(180, process_line(-1, rad_on, "ACOS"));
    EXPECT_DOUBLE_EQ(0, process_line(1, rad_on, "ACOS"));
    EXPECT_DOUBLE_EQ(90, process_line(0, rad_on, "ACOS"));
}

TEST(trig, atan)
{
    bool rad_on = false;
    EXPECT_DOUBLE_EQ(0, process_line(0, rad_on, "ATAN"));
    EXPECT_DOUBLE_EQ(45, process_line(1, rad_on, "ATAN"));
    EXPECT_DOUBLE_EQ(0, process_line(0, rad_on, "RAD"));
    ASSERT_TRUE(rad_on);
    EXPECT_DOUBLE_EQ(0, process_line(0, rad_on, "ATAN"));
    EXPECT_DOUBLE_EQ(quarter_pi, process_line(1, rad_on, "ATAN"));
    EXPECT_DOUBLE_EQ(0, process_line(0, rad_on, "DEG"));
    ASSERT_FALSE(rad_on);
    EXPECT_DOUBLE_EQ(0, process_line(0, rad_on, "ATAN"));
    EXPECT_DOUBLE_EQ(45, process_line(1, rad_on, "ATAN"));
}

TEST(trig, actn)
{
    bool rad_on = false;
    EXPECT_DOUBLE_EQ(45, process_line(1, rad_on, "ACTN"));
    EXPECT_DOUBLE_EQ(30, process_line(sqrt_3, rad_on, "ACTN"));
    EXPECT_DOUBLE_EQ(0, process_line(0, rad_on, "RAD"));
    ASSERT_TRUE(rad_on);
    EXPECT_DOUBLE_EQ(quarter_pi, process_line(1, rad_on, "ACTN"));
    EXPECT_DOUBLE_EQ(pi / 6, process_line(sqrt_3, rad_on, "ACTN"));
    EXPECT_DOUBLE_EQ(pi / 3, process_line(sqrt_3_over_3, rad_on, "ACTN"));
    EXPECT_DOUBLE_EQ(0, process_line(0, rad_on, "DEG"));
    ASSERT_FALSE(rad_on);
    EXPECT_DOUBLE_EQ(45, process_line(1, rad_on, "ACTN"));
    EXPECT_DOUBLE_EQ(30, process_line(sqrt_3, rad_on, "ACTN"));
    EXPECT_DOUBLE_EQ(120, process_line(-sqrt_3_over_3, rad_on, "ACTN"));
}

#endif

int main(int argc, char ** argv)
{
    testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
