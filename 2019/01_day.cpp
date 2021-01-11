#include <vector>
#include <fstream>
#include <iostream>
#include <numeric>

auto parse_input () -> std::vector<int>
{
    auto istr = std::ifstream("input/01_day.txt");
    auto n    = int(0);
    auto ns   = std::vector<int>();

    while (istr >> n)
    {
        ns.push_back(n);
    }

    return ns;
}

auto fuel_requirement_1 (int const w) -> int
{
    return w / 3 - 2;
}

auto fuel_requirement_2 (int const w) -> int
{
    auto const r = fuel_requirement_1(w);
    return r < 0 ? 0 : r + fuel_requirement_2(r);
}

auto solve_part_1 (std::vector<int> const& ns) -> int
{
    return std::transform_reduce( std::begin(ns), std::end(ns)
                                , 0, std::plus<>(), fuel_requirement_1 );
}

auto solve_part_2 (std::vector<int> const& ns) -> int
{
    return std::transform_reduce( std::begin(ns), std::end(ns)
                                , 0, std::plus<>(), fuel_requirement_2 );
}

auto main () -> int
{
    auto const input = parse_input();
    std::cout << "Part 1: " << solve_part_1(input) << '\n';
    std::cout << "Part 2: " << solve_part_2(input) << '\n';
}