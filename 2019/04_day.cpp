#include <iostream>
#include <fstream>
#include <utility>
#include <string>
#include <iterator>
#include <charconv>
#include <array>
#include <algorithm>
#include <functional>
#include <iterator>

auto parse_input () -> std::pair<int, int>
{
    auto istr = std::fstream("./input/04_day.txt");
    auto line = std::string();
    std::getline(istr, line);
    auto mid  = std::find(std::begin(line), std::end(line), '-');
    auto midd = static_cast<std::size_t>(std::distance(std::begin(line), mid));
    auto from = int(0);
    auto to   = int(0);
    std::from_chars(line.data(), line.data() + midd, from);
    std::from_chars(line.data() + midd + 1, line.data() + midd + 1 + (line.size() - midd), to);
    return {from, to};
}

auto to_digits (int n) -> std::array<int, 6>
{
    auto ds  = std::array<int, 6>();
    auto out = std::rbegin(ds);
    auto end = std::rend(ds);
    while (out != end)
    {
        *out = n % 10;
        n /= 10;
        ++out;
    }
    return ds;
}
auto solve_part_1 (int const from, int const to) -> int
{
    auto total = int(0);
    for (auto n = from; n <= to; ++n)
    {
        auto const digits     = to_digits(n);
        auto const isPassword = std::adjacent_find(std::begin(digits), std::end(digits), std::greater<>())  == std::end(digits)
                            and std::adjacent_find(std::begin(digits), std::end(digits), std::equal_to<>()) != std::end(digits);
        total += isPassword ? 1 : 0;
    }
    return total;
}

template<class ForwardIt>
auto strict_pair_find (ForwardIt first, ForwardIt last) -> ForwardIt
{
    while (first != last)
    {
        auto it   = first;
        auto next = std::next(first);
        auto len  = 0;
        while (next != last and *it == *next)
        {
            ++len;
            ++it;
            ++next;
        }
        if (1 == len)
        {
            return first;
        }
        first = next;
    }

    return last;
}

auto solve_part_2 (int const from, int const to) -> int
{
    auto total = int(0);
    for (auto n = from; n <= to; ++n)
    {
        auto const digits     = to_digits(n);
        auto const isPassword = std::adjacent_find(std::begin(digits), std::end(digits), std::greater<>()) == std::end(digits)
                            and strict_pair_find(std::begin(digits), std::end(digits)) != std::end(digits);
        total += isPassword ? 1 : 0;
    }
    return total;
}

auto main () -> int
{
    auto const input = parse_input();
    std::cout << "Part 1: " << solve_part_1(input.first, input.second) << '\n';
    std::cout << "Part 2: " << solve_part_2(input.first, input.second) << '\n';
}