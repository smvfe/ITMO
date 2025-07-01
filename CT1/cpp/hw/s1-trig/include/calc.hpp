#ifndef CALC_HPP
#define CALC_HPP

#include <string_view>

[[nodiscard]] double process_line(double currentValue,
                                  std::string const & line);

[[nodiscard]] double process_line(double currentValue, bool & rad_on, std::string const & line);

#endif
