#include <vector>
#include <fstream>
#include <iostream>
#include <string>
#include <array>
#include <charconv>

auto parse_input () -> std::vector<int>
{
    auto ns    = std::vector<int>();
    auto istr  = std::fstream("./input/02_day.txt");
    auto chars = std::string();
    while (std::getline(istr, chars, ','))
    {
        auto n = int {};
        std::from_chars(chars.data(), chars.data() + chars.size(), n);
        ns.push_back(n);
    }
    return ns;
}

struct intcode_machine
{
    std::size_t      eip;
    std::vector<int> program;
};

auto argv (intcode_machine const& machine, std::size_t const i) -> std::size_t
{
    return static_cast<std::size_t>(machine.program[machine.eip + i]);
}

auto op_add (intcode_machine& machine) -> bool
{
    auto const a1ptr = argv(machine, 1);
    auto const a2ptr = argv(machine, 2);
    auto const dptr  = argv(machine, 3);
    auto const a1 = machine.program[a1ptr];
    auto const a2 = machine.program[a2ptr];
    machine.program[dptr] = a1 + a2;
    machine.eip += 4;
    return true;
}

auto op_mul (intcode_machine& machine) -> bool
{
    auto const a1ptr = argv(machine, 1);
    auto const a2ptr = argv(machine, 2);
    auto const dptr  = argv(machine, 3);
    auto const a1 = machine.program[a1ptr];
    auto const a2 = machine.program[a2ptr];
    machine.program[dptr] = a1 * a2;
    machine.eip += 4;
    return true;
}

auto op_halt (intcode_machine&) -> bool
{
    return false;
}

using op_t = bool(*)(intcode_machine&);
auto consteval make_op_table () -> std::array<op_t, 100>
{
    auto ops = std::array<op_t, 100> {};
    ops[1]   = op_add;
    ops[2]   = op_mul;
    ops[99]  = op_halt;
    return ops;
}

auto execute_op (intcode_machine& machine) -> bool
{
    static auto constinit Ops = make_op_table();
    auto const opcode = argv(machine, 0);
    return Ops[opcode](machine);
}

auto run_program (intcode_machine& machine) -> int
{
    while (execute_op(machine));
    return machine.program[0];
}

auto solve_part_1 (std::vector<int> input) -> int
{
    auto machine = intcode_machine {0, std::move(input)};
    machine.program[1] = 12;
    machine.program[2] = 2;
    return run_program(machine);
}

auto solve_part_2 (std::vector<int> input) -> int
{
    for (auto i = 0; i < 100; ++i)
    {
        for (auto j = 0; j < 100; ++j)
        {
            auto machine = intcode_machine {0, input};
            machine.program[1] = i;
            machine.program[2] = j;

            if (19690720 == run_program(machine))
            {
                return 100 * i + j;
            }
        }
    }

    return -1;
}

auto main () -> int
{
    auto const input = parse_input();
    std::cout << "Part 1: " << solve_part_1(input) << '\n';
    std::cout << "Part 2: " << solve_part_2(input) << '\n';
}