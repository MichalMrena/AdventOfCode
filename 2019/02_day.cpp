#include <iostream>
#include <vector>
#include <map>
#include <functional>

struct state_t
{
    std::vector<int> intcode {1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,6,19,1,19,5,23,2,13,23,27,1,10,27,31,2,6,31,35,1,9,35,39,2,10,39,43,1,43,9,47,1,47,9,51,2,10,51,55,1,55,9,59,1,59,5,63,1,63,6,67,2,6,67,71,2,10,71,75,1,75,5,79,1,9,79,83,2,83,10,87,1,87,6,91,1,13,91,95,2,10,95,99,1,99,6,103,2,13,103,107,1,107,2,111,1,111,9,0,99,2,14,0,0};
    size_t eip {0};

    auto op_code () const -> int
    {
        return this->intcode.at(this->eip);
    }
};

class machine_t
{
private:
    state_t state_;
    std::map<int, std::function<bool()>> ops_
    {
        {1,  std::bind(&machine_t::add, std::ref(*this)) }
      , {2,  std::bind(&machine_t::mul, std::ref(*this)) }
      , {99, std::bind(&machine_t::end, std::ref(*this)) }
    };

public:
    auto init (const int p1, const int p2) -> void
    {
        state_.intcode[1] = p1;
        state_.intcode[2] = p2;
    }

    auto run () -> void
    {
        while (ops_.at(state_.op_code())()) {};
    }

    auto result () const -> int
    {
        return state_.intcode.at(0);
    }

private:
    auto add () -> bool
    {
        const auto op1  {state_.intcode.at(state_.intcode.at(state_.eip + 1))};
        const auto op2  {state_.intcode.at(state_.intcode.at(state_.eip + 2))};
        const auto iout {state_.intcode.at(state_.eip + 3)};

        state_.intcode[iout] = op1 + op2;
        state_.eip += 4;

        return true;
    }

    auto mul () -> bool
    {
        const auto op1  {state_.intcode.at(state_.intcode.at(state_.eip + 1))};
        const auto op2  {state_.intcode.at(state_.intcode.at(state_.eip + 2))};
        const auto iout {state_.intcode.at(state_.eip + 3)};

        state_.intcode[iout] = op1 * op2;
        state_.eip += 4;

        return true;
    }

    auto end () -> bool
    {
        return false;
    }

};

auto solve_part_1 ()
{
    machine_t machine;

    machine.init(12, 2);
    machine.run();

    std::cout << machine.result() << '\n';
}

auto solve_part_2 ()
{
    for (int noun {0}; noun < 100; noun++)
    {
        for (int verb {0}; verb < 100; verb++)
        {
            machine_t machine;

            machine.init(noun, verb);
            machine.run();

            if (19690720 == machine.result())
            {
                std::cout << (100 * noun + verb) << '\n';
            }
        }
    }
}

auto main () -> int
{
    solve_part_1();
    solve_part_2();

    return 0;
}
