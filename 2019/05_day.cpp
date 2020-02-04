#include <iostream>
#include <vector>
#include <queue>
#include <map>
#include <array>
#include <functional>
#include <initializer_list>

#define ever ;;

enum class param_mode_e { possition, immediate, };

auto decode_mode (const int n)
{
    return 0 == n ? param_mode_e::possition : param_mode_e::immediate;
}

struct state_t
{
    std::vector<int> intcode_;
    size_t eip_;

    state_t(std::initializer_list<int> initState) :
        intcode_ {initState}
      , eip_     {0}
    {
    }
};

class machine_t
{
private:
    const std::map<int, std::function<bool()>> ops_
    {
        {1,  std::bind(&machine_t::add, std::ref(*this)) }
      , {2,  std::bind(&machine_t::mul, std::ref(*this)) }
      , {3,  std::bind(&machine_t::in,  std::ref(*this)) }
      , {4,  std::bind(&machine_t::out, std::ref(*this)) }
      , {5,  std::bind(&machine_t::jit, std::ref(*this)) }
      , {6,  std::bind(&machine_t::jif, std::ref(*this)) }
      , {7,  std::bind(&machine_t::lt,  std::ref(*this)) }
      , {8,  std::bind(&machine_t::eq,  std::ref(*this)) }
      , {99, std::bind(&machine_t::end, std::ref(*this)) }
    };

    state_t state_;
    std::array<param_mode_e, 3> modes_;
    std::queue<int> stdin_;
    std::queue<int> stdout_;

public:
    machine_t(std::initializer_list<int> initState) :
        state_ {initState}
    {
    }

    auto init (const int p1, const int p2) -> void
    {
        state_.intcode_[1] = p1;
        state_.intcode_[2] = p2;
    }

    auto run () -> void
    {
        for (ever)
        {
            const auto opnum  {state_.intcode_.at(state_.eip_)};
            const auto opcode {parse_op(opnum)};

            if (! ops_.at(opcode)())
            {
                break;
            }
        }
    }

    auto add_input (const int input) -> void
    {
        stdin_.push(input);
    }

    auto print_output () -> void
    {
        while (! stdout_.empty())
        {
            std::cout << stdout_.front() << '\n';
            stdout_.pop();
        }
    }

private:
    auto add () -> bool
    {
        const auto op1  {get_param(0)};
        const auto op2  {get_param(1)};
        const auto iout {get_out_param(2)};

        state_.intcode_[iout] = op1 + op2;
        state_.eip_ += 4;

        return true;
    }

    auto mul () -> bool
    {
        const auto op1  {get_param(0)};
        const auto op2  {get_param(1)};
        const auto iout {get_out_param(2)};

        state_.intcode_[iout] = op1 * op2;
        state_.eip_ += 4;

        return true;
    }

    auto end () -> bool
    {
        return false;
    }

    auto in () -> bool
    {
        const auto input {stdin_.front()};
        stdin_.pop();

        const auto iout {get_out_param(0)};
        state_.intcode_[iout] = input;
        state_.eip_ += 2;

        return true;
    }

    auto out () -> bool
    {
        stdout_.push(get_param(0));
        state_.eip_ += 2;

        return true;
    }

    auto jit () -> bool
    {
        const auto p1     {get_param(0)};
        const auto neweip {get_param(1)};

        if (p1 != 0)
        {
            state_.eip_ = neweip;
        }
        else
        {
            state_.eip_ += 3;
        }
        
        return true;
    }

    auto jif () -> bool
    {
        const auto p1     {get_param(0)};
        const auto neweip {get_param(1)};

        if (p1 == 0)
        {
            state_.eip_ = neweip;
        }
        else
        {
            state_.eip_ += 3;
        }

        return true;
    }

    auto lt () -> bool
    {
        const auto p1   {get_param(0)};
        const auto p2   {get_param(1)};
        const auto iout {get_out_param(2)};

        state_.intcode_[iout] = p1 < p2 ? 1 : 0;
        state_.eip_ += 4;

        return true;
    }

    auto eq () -> bool
    {
        const auto p1   {get_param(0)};
        const auto p2   {get_param(1)};
        const auto iout {get_out_param(2)};

        state_.intcode_[iout] = p1 == p2 ? 1 : 0;
        state_.eip_ += 4;

        return true;
    }

    auto get_param (int i) -> int
    {
        return param_mode_e::immediate == modes_[i] 
            ? state_.intcode_.at(state_.eip_ + i + 1)
            : state_.intcode_.at(state_.intcode_.at(state_.eip_ + i + 1));
    } 

    auto get_out_param (int i) -> int
    {
        return state_.intcode_.at(state_.eip_ + i + 1);
    }

    auto parse_op (int opnum) -> int
    {
        const auto opcode {opnum % 100};
        opnum /= 100;

        auto paramIt  {modes_.begin()};
        auto paramEnd {modes_.end()};

        while (opnum > 0 and paramIt != paramEnd)
        {
            *paramIt = decode_mode(opnum % 10);

            opnum /= 10;
            ++paramIt;
        }

        while (paramIt != paramEnd)
        {
            *paramIt = param_mode_e::possition;
            ++paramIt;
        }

        return opcode;
    }
};

auto solve_part1 ()
{
    machine_t machine {3,225,1,225,6,6,1100,1,238,225,104,0,1001,191,50,224,101,-64,224,224,4,224,1002,223,8,223,101,5,224,224,1,224,223,223,2,150,218,224,1001,224,-1537,224,4,224,102,8,223,223,1001,224,2,224,1,223,224,223,1002,154,5,224,101,-35,224,224,4,224,1002,223,8,223,1001,224,5,224,1,224,223,223,1102,76,17,225,1102,21,44,224,1001,224,-924,224,4,224,102,8,223,223,1001,224,4,224,1,224,223,223,101,37,161,224,101,-70,224,224,4,224,1002,223,8,223,101,6,224,224,1,223,224,223,102,46,157,224,1001,224,-1978,224,4,224,102,8,223,223,1001,224,5,224,1,224,223,223,1102,5,29,225,1101,10,7,225,1101,43,38,225,1102,33,46,225,1,80,188,224,1001,224,-73,224,4,224,102,8,223,223,101,4,224,224,1,224,223,223,1101,52,56,225,1101,14,22,225,1101,66,49,224,1001,224,-115,224,4,224,1002,223,8,223,1001,224,7,224,1,224,223,223,1101,25,53,225,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,108,226,226,224,1002,223,2,223,1005,224,329,101,1,223,223,108,677,677,224,1002,223,2,223,1006,224,344,1001,223,1,223,8,677,677,224,102,2,223,223,1006,224,359,101,1,223,223,7,226,677,224,102,2,223,223,1005,224,374,101,1,223,223,107,226,226,224,102,2,223,223,1006,224,389,101,1,223,223,7,677,226,224,1002,223,2,223,1006,224,404,1001,223,1,223,1107,677,226,224,1002,223,2,223,1006,224,419,1001,223,1,223,1007,226,226,224,102,2,223,223,1005,224,434,101,1,223,223,1008,226,677,224,102,2,223,223,1005,224,449,1001,223,1,223,1007,677,677,224,1002,223,2,223,1006,224,464,1001,223,1,223,1008,226,226,224,102,2,223,223,1006,224,479,101,1,223,223,1007,226,677,224,1002,223,2,223,1005,224,494,1001,223,1,223,108,226,677,224,1002,223,2,223,1006,224,509,101,1,223,223,8,226,677,224,102,2,223,223,1005,224,524,1001,223,1,223,107,677,677,224,1002,223,2,223,1005,224,539,101,1,223,223,107,226,677,224,1002,223,2,223,1006,224,554,101,1,223,223,1107,226,677,224,1002,223,2,223,1006,224,569,1001,223,1,223,1108,677,226,224,102,2,223,223,1005,224,584,1001,223,1,223,1008,677,677,224,102,2,223,223,1005,224,599,1001,223,1,223,1107,677,677,224,102,2,223,223,1006,224,614,101,1,223,223,7,226,226,224,102,2,223,223,1005,224,629,1001,223,1,223,1108,677,677,224,102,2,223,223,1006,224,644,1001,223,1,223,8,677,226,224,1002,223,2,223,1005,224,659,101,1,223,223,1108,226,677,224,102,2,223,223,1005,224,674,101,1,223,223,4,223,99,226};

    machine.add_input(1);
    machine.run();
    machine.print_output();
}

auto solve_part2 ()
{
    machine_t machine {3,225,1,225,6,6,1100,1,238,225,104,0,1001,191,50,224,101,-64,224,224,4,224,1002,223,8,223,101,5,224,224,1,224,223,223,2,150,218,224,1001,224,-1537,224,4,224,102,8,223,223,1001,224,2,224,1,223,224,223,1002,154,5,224,101,-35,224,224,4,224,1002,223,8,223,1001,224,5,224,1,224,223,223,1102,76,17,225,1102,21,44,224,1001,224,-924,224,4,224,102,8,223,223,1001,224,4,224,1,224,223,223,101,37,161,224,101,-70,224,224,4,224,1002,223,8,223,101,6,224,224,1,223,224,223,102,46,157,224,1001,224,-1978,224,4,224,102,8,223,223,1001,224,5,224,1,224,223,223,1102,5,29,225,1101,10,7,225,1101,43,38,225,1102,33,46,225,1,80,188,224,1001,224,-73,224,4,224,102,8,223,223,101,4,224,224,1,224,223,223,1101,52,56,225,1101,14,22,225,1101,66,49,224,1001,224,-115,224,4,224,1002,223,8,223,1001,224,7,224,1,224,223,223,1101,25,53,225,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,108,226,226,224,1002,223,2,223,1005,224,329,101,1,223,223,108,677,677,224,1002,223,2,223,1006,224,344,1001,223,1,223,8,677,677,224,102,2,223,223,1006,224,359,101,1,223,223,7,226,677,224,102,2,223,223,1005,224,374,101,1,223,223,107,226,226,224,102,2,223,223,1006,224,389,101,1,223,223,7,677,226,224,1002,223,2,223,1006,224,404,1001,223,1,223,1107,677,226,224,1002,223,2,223,1006,224,419,1001,223,1,223,1007,226,226,224,102,2,223,223,1005,224,434,101,1,223,223,1008,226,677,224,102,2,223,223,1005,224,449,1001,223,1,223,1007,677,677,224,1002,223,2,223,1006,224,464,1001,223,1,223,1008,226,226,224,102,2,223,223,1006,224,479,101,1,223,223,1007,226,677,224,1002,223,2,223,1005,224,494,1001,223,1,223,108,226,677,224,1002,223,2,223,1006,224,509,101,1,223,223,8,226,677,224,102,2,223,223,1005,224,524,1001,223,1,223,107,677,677,224,1002,223,2,223,1005,224,539,101,1,223,223,107,226,677,224,1002,223,2,223,1006,224,554,101,1,223,223,1107,226,677,224,1002,223,2,223,1006,224,569,1001,223,1,223,1108,677,226,224,102,2,223,223,1005,224,584,1001,223,1,223,1008,677,677,224,102,2,223,223,1005,224,599,1001,223,1,223,1107,677,677,224,102,2,223,223,1006,224,614,101,1,223,223,7,226,226,224,102,2,223,223,1005,224,629,1001,223,1,223,1108,677,677,224,102,2,223,223,1006,224,644,1001,223,1,223,8,677,226,224,1002,223,2,223,1005,224,659,101,1,223,223,1108,226,677,224,102,2,223,223,1005,224,674,101,1,223,223,4,223,99,226};

    machine.add_input(5);
    machine.run();
    machine.print_output();
}

auto main () -> int
{
    solve_part1();
    solve_part2();

    return 0;
}
