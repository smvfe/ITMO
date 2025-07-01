#include "calc.hpp"

#include <iostream>
#include <string>

int main()
{
    std::string s;
    double val{0.0};
    while (std::getline(std::cin, s)) {
        std::cout << (val = process_line(val, s)) << std::endl;
    }
    return 0;
}
