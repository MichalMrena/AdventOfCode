#include <iostream>
#include <vector>
#include <queue>
#include <list>
#include <map>
#include <array>
#include <functional>
#include <initializer_list>
#include <algorithm>

#define ever ;;

namespace aoc
{
    constexpr std::initializer_list<int> program = {3,8,1001,8,10,8,105,1,0,0,21,38,63,72,85,110,191,272,353,434,99999,3,9,102,4,9,9,101,2,9,9,102,3,9,9,4,9,99,3,9,1001,9,4,9,102,2,9,9,1001,9,5,9,1002,9,5,9,101,3,9,9,4,9,99,3,9,1001,9,2,9,4,9,99,3,9,1001,9,3,9,102,2,9,9,4,9,99,3,9,101,2,9,9,102,2,9,9,1001,9,2,9,1002,9,4,9,101,2,9,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,99};

    enum class param_mode_e { possition, immediate, };

    auto decode_mode (const int n) -> param_mode_e
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

        auto extract_output () -> int
        {
            const auto ret {stdout_.front()};
            stdout_.pop();
            return ret;
        }

        auto halted () const -> bool
        {
            return 99 == state_.intcode_[state_.eip_];
        }

        auto has_output () const -> bool
        {
            return not stdout_.empty();
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
            if (stdin_.empty())
            {
                return false;
            }

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

    class machine_link_t
    {
    private:
        machine_t& first;
        machine_t& second;

    public:
        machine_link_t( machine_t& pFirst
                      , machine_t& pSecond ) :
            first {pFirst}
          , second {pSecond}
        {
        }

        auto run () -> void
        {
            first.run();

            while (first.has_output())
            {
                second.add_input(first.extract_output());
            }
        }
    };

    template<size_t MachineCount, size_t MachineIndex>
    class serial_machine_t
    {
    protected:
        machine_t machine_;
        serial_machine_t<MachineCount, MachineIndex - 1> nextMachine_;
        machine_link_t link_;

        friend class serial_machine_t<MachineCount, MachineIndex + 1>;

    protected:
        serial_machine_t( std::array<int, MachineCount> phases
                        , std::initializer_list<int> initState ) :
            machine_     {initState}
          , nextMachine_ {phases, initState}
          , link_        {machine_, nextMachine_.machine_}
        {
            machine_.add_input(phases[MachineIndex]);
        }

        auto add_input (const int input) -> void
        {
            machine_.add_input(input);
        }

        auto run () -> void
        {
            link_.run();
            nextMachine_.run();
        }

        auto last_machine () -> machine_t&
        {
            return nextMachine_.last_machine();
        }
    };

    template<size_t MachineCount>
    class serial_machine_t<MachineCount, 0>
    {
    private:
        machine_t machine_;

        friend class serial_machine_t<MachineCount, 1>;
        friend class serial_machine_t<MachineCount, MachineCount - 1>;

    public:
        serial_machine_t( std::array<int, MachineCount> phases
                        , std::initializer_list<int> initState ) :
            machine_ {initState}
        {
            machine_.add_input(phases[0]);
        }

        auto run () -> void
        {
            machine_.run();
        }

        auto add_input (const int input) -> void
        {
            machine_.add_input(input);
        }

    private:
        auto last_machine () -> machine_t&
        {
            return machine_;
        }
    };

    template<size_t MachineCount>
    class serial_machines_t : public serial_machine_t<MachineCount, MachineCount - 1>
    {
    private:
        using base = serial_machine_t<MachineCount, MachineCount - 1>;

    public:
        serial_machines_t( std::array<int, MachineCount> phases
                         , std::initializer_list<int> initState ) :
            base {phases, initState}
        {
        }

        auto run_looped () -> void
        {
            base::run();

            auto& lastMachine {base::last_machine()};
            machine_link_t loopLink {lastMachine, base::machine_};

            while (not lastMachine.halted())
            {
                loopLink.run();
                base::run();
            }
        }

        auto add_input (const int input) -> void
        {
            base::add_input(input);
        }

        auto last_output () -> int
        {
            return base::last_machine().extract_output();
        }
    };

    auto solve_part1 ()
    {
        std::array<int, 5> phases {0, 1, 2, 3, 4};
        int maxOutput {0};

        do 
        {
            serial_machines_t<5> machines {phases, program};
            machines.add_input(0);
            machines.run_looped();
            maxOutput = std::max(maxOutput, machines.last_output());
        } 
        while(std::next_permutation(phases.begin(), phases.end()));

        std::cout << maxOutput << '\n';
    }

    auto solve_part2 ()
    {
        std::array<int, 5> phases {5, 6, 7, 8, 9};
        int maxOutput {0};

        do 
        {
            serial_machines_t<5> machines {phases, program};
            machines.add_input(0);
            machines.run_looped();
            maxOutput = std::max(maxOutput, machines.last_output());
        } 
        while(std::next_permutation(phases.begin(), phases.end()));

        std::cout << maxOutput << '\n';
    }
}

auto main () -> int
{
    aoc::solve_part1();
    aoc::solve_part2();

    return 0;
}