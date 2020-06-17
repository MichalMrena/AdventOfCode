#include <iostream>
#include <vector>
#include <queue>
#include <map>
#include <array>
#include <functional>
#include <initializer_list>
#include <stdexcept>
#include <cstddef>

#define ever ;;

namespace aoc
{
    using code_t = long long;

    constexpr std::initializer_list<code_t> program = {3,8,1005,8,311,1106,0,11,0,0,0,104,1,104,0,3,8,102,-1,8,10,1001,10,1,10,4,10,108,0,8,10,4,10,1002,8,1,28,1006,0,2,2,109,10,10,1,1,19,10,1,1103,20,10,3,8,102,-1,8,10,1001,10,1,10,4,10,108,1,8,10,4,10,1002,8,1,65,1006,0,33,1,7,0,10,3,8,102,-1,8,10,101,1,10,10,4,10,108,0,8,10,4,10,1002,8,1,94,3,8,102,-1,8,10,1001,10,1,10,4,10,108,1,8,10,4,10,101,0,8,116,1,1002,1,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,108,0,8,10,4,10,1002,8,1,142,2,1101,6,10,3,8,1002,8,-1,10,101,1,10,10,4,10,108,0,8,10,4,10,1001,8,0,168,2,1107,7,10,1006,0,68,1,5,6,10,1,2,5,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,0,10,4,10,1002,8,1,206,1,1008,16,10,3,8,102,-1,8,10,1001,10,1,10,4,10,1008,8,1,10,4,10,1001,8,0,232,3,8,102,-1,8,10,101,1,10,10,4,10,108,1,8,10,4,10,102,1,8,253,1006,0,30,2,1,4,10,1,1008,1,10,2,1109,4,10,3,8,102,-1,8,10,1001,10,1,10,4,10,1008,8,1,10,4,10,102,1,8,291,101,1,9,9,1007,9,1051,10,1005,10,15,99,109,633,104,0,104,1,21102,387508339604,1,1,21102,1,328,0,1106,0,432,21101,0,47677022988,1,21101,0,339,0,1106,0,432,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,21102,209382822080,1,1,21102,386,1,0,1105,1,432,21101,179318123523,0,1,21102,1,397,0,1105,1,432,3,10,104,0,104,0,3,10,104,0,104,0,21102,709584904960,1,1,21101,420,0,0,1106,0,432,21102,709580444008,1,1,21102,431,1,0,1105,1,432,99,109,2,21202,-1,1,1,21102,1,40,2,21101,0,463,3,21101,0,453,0,1105,1,496,109,-2,2105,1,0,0,1,0,0,1,109,2,3,10,204,-1,1001,458,459,474,4,0,1001,458,1,458,108,4,458,10,1006,10,490,1101,0,0,458,109,-2,2106,0,0,0,109,4,2102,1,-1,495,1207,-3,0,10,1006,10,513,21102,1,0,-3,21202,-3,1,1,22102,1,-2,2,21102,1,1,3,21102,532,1,0,1106,0,537,109,-4,2105,1,0,109,5,1207,-3,1,10,1006,10,560,2207,-4,-2,10,1006,10,560,21201,-4,0,-4,1106,0,628,22101,0,-4,1,21201,-3,-1,2,21202,-2,2,3,21101,579,0,0,1105,1,537,21201,1,0,-4,21101,1,0,-1,2207,-4,-2,10,1006,10,598,21102,0,1,-1,22202,-2,-1,-2,2107,0,-3,10,1006,10,620,21201,-1,0,1,21101,0,620,0,106,0,495,21202,-2,-1,-2,22201,-4,-2,-4,109,-5,2105,1,0};

    enum class param_mode_e { possition, immediate, relative, };

    auto decode_mode (int const n) -> param_mode_e
    {
        switch (n)
        {
            case 0: return param_mode_e::possition;
            case 1: return param_mode_e::immediate;
            case 2: return param_mode_e::relative;
            default: throw std::runtime_error {"Not good."};
        }
    }

    struct machine_state
    {
        std::vector<code_t> intcode_;
        size_t              eip_;
        size_t              relBase_;

        machine_state(std::initializer_list<code_t> const& initState) :
            intcode_ {initState},
            eip_     {0},
            relBase_ {0}
        {
        }

        auto operator[] (size_t i) -> code_t&
        {
            if (i >= intcode_.size())
            {
                intcode_.resize(i + 1, 0);
            }

            return intcode_.at(i);
        }
    };

    class intcode_machine
    {
    private:
        const std::map<int, std::function<bool()>> ops_
        {
            {1,  std::bind(&intcode_machine::add, this) }
          , {2,  std::bind(&intcode_machine::mul, this) }
          , {3,  std::bind(&intcode_machine::in,  this) }
          , {4,  std::bind(&intcode_machine::out, this) }
          , {5,  std::bind(&intcode_machine::jit, this) }
          , {6,  std::bind(&intcode_machine::jif, this) }
          , {7,  std::bind(&intcode_machine::lt,  this) }
          , {8,  std::bind(&intcode_machine::eq,  this) }
          , {9,  std::bind(&intcode_machine::arb, this) }
          , {99, std::bind(&intcode_machine::end, this) }
        };

        machine_state               state_;
        std::array<param_mode_e, 3> modes_;
        std::queue<code_t>          stdin_;
        std::queue<code_t>          stdout_;

    public:
        intcode_machine(std::initializer_list<code_t> initState) :
            state_ {initState}
        {
        }

        auto run () -> void
        {
            for (ever)
            {
                auto const opnum  = state_.intcode_.at(state_.eip_);
                auto const opcode = parse_op(opnum);

                if (! ops_.at(opcode)())
                {
                    break;
                }
            }
        }

        auto add_input (code_t const input) -> void
        {
            stdin_.push(input);
        }

        auto extract_output () -> code_t
        {
            auto const ret = stdout_.front();
            stdout_.pop();
            return ret;
        }

        auto has_output () const -> bool
        {
            return not stdout_.empty();
        }

        auto halted () const -> bool
        {
            return 99 == state_.intcode_.at(state_.eip_);
        }

    private:
        auto add () -> bool
        {
            auto const op1  = get_param(0);
            auto const op2  = get_param(1);
            auto const iout = get_io_param(2);

            state_[iout] = op1 + op2;
            state_.eip_ += 4;

            return true;
        }

        auto mul () -> bool
        {
            auto const op1  = get_param(0);
            auto const op2  = get_param(1);
            auto const iout = get_io_param(2);

            state_[iout] = op1 * op2;
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

            auto const input = stdin_.front();
            stdin_.pop();

            auto const iout = get_io_param(0);
            state_[iout] = input;
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
            auto const p1     = get_param(0);
            auto const neweip = get_param(1);

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
            auto const p1     = get_param(0);
            auto const neweip = get_param(1);

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
            auto const p1   = get_param(0);
            auto const p2   = get_param(1);
            auto const iout = get_io_param(2);

            state_[iout] = p1 < p2 ? 1 : 0;
            state_.eip_ += 4;

            return true;
        }

        auto eq () -> bool
        {
            auto const p1   = get_param(0);
            auto const p2   = get_param(1);
            auto const iout = get_io_param(2);

            state_[iout] = p1 == p2 ? 1 : 0;
            state_.eip_ += 4;

            return true;
        }

        auto arb () -> bool
        {
            auto const p1 = get_param(0);
            
            state_.relBase_ += p1;
            state_.eip_     += 2;

            return true;
        }

        auto get_param (int const i) -> code_t
        {
            auto const paramValue = state_[state_.eip_ + i + 1];

            switch (modes_.at(i))
            {
                case param_mode_e::immediate: return paramValue;
                case param_mode_e::possition: return state_[paramValue];
                case param_mode_e::relative:  return state_[state_.relBase_ + paramValue];
            }

            throw std::logic_error {"not good"};
        } 

        auto get_io_param (code_t const i) -> code_t
        {
            auto const paramValue = state_[state_.eip_ + i + 1];

            switch (modes_.at(i))
            {
                case param_mode_e::possition: return paramValue;
                case param_mode_e::relative:  return state_.relBase_ + paramValue;
            }

            throw std::logic_error {"not good"};
        }

        auto parse_op (int opnum) -> int
        {
            auto const opcode = opnum % 100;
            opnum /= 100;

            auto paramIt  = modes_.begin();
            auto paramEnd = modes_.end();

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

    struct point
    {
        int x;
        int y;
    };

    auto operator< (point const& lhs, point const& rhs)
    {
        return (lhs.x < rhs.x) || ((lhs.x == rhs.x) && (lhs.y < rhs.y));
    }

    auto operator+ (point const& lhs, point const& rhs)
    {
        return point {lhs.x + rhs.x, lhs.y + rhs.y};
    }

    class hull
    {
    public:
        using color_t = std::int8_t;

    private:
        std::map<point, color_t> pixels_;

    public:
        inline static constexpr auto black = color_t {0};
        inline static constexpr auto white = color_t {1};
        
        auto color_at (point const& p) const -> color_t
        {
            auto const it = pixels_.find(p);
            return pixels_.end() == it ? hull::black : it->second;
        }

        auto paint (point const& p, color_t const c) -> void
        {
            auto const it = pixels_.find(p);
            if (pixels_.end() == it)
            {
                pixels_.emplace(p, c);
            }
            else
            {
                it->second = c;
            }
        }

        auto painted () const -> std::size_t
        {
            return pixels_.size();
        }

        auto print_pixels (std::ostream& ost) -> void
        {
            for (auto const& [p, c] : pixels_)
            {
                if (hull::white == c)
                {
                    ost << p.x << ';' << p.y << std::endl;
                }
            }
        }
    };

    class painting_robot
    {
    public:
        using turn_t   = std::int8_t;
        using moves_c  = std::array<point, 4>;
        using moves_it = typename moves_c::const_iterator;

    private:
        inline static constexpr auto moves = moves_c {point {0, 1}, point {1, 0}, point {0, -1}, point {-1, 0}};
        point    position_ {0, 0};
        moves_it it_       {painting_robot::moves.begin()};

    public:
        auto position () const -> point
        {
            return position_;
        }

        auto turn (turn_t const t) -> void
        {
            switch (t)
            {
                case 0:  this->turn_clockwise();         break;
                case 1:  this->turn_counter_clockwise(); break;
                default: throw std::logic_error {"Not good."};
            }
        }

        auto move () -> void
        {
            position_ = position_ + *it_;
        }

    private:
        auto turn_clockwise () -> void
        {
            ++it_;
            if (painting_robot::moves.end() == it_)
            {
                it_ = painting_robot::moves.begin();
            }
        }

        auto turn_counter_clockwise () -> void
        {
            if (painting_robot::moves.begin() == it_)
            {
                it_ = painting_robot::moves.end() - 1;
            }
            else
            {
                --it_;
            }
        }
    };

    auto solve_part1 ()
    {
        auto machine = intcode_machine {program};
        auto robot   = painting_robot {};
        auto panel   = hull {};

        while (not machine.halted())
        {
            machine.add_input(panel.color_at(robot.position()));
            machine.run();
            panel.paint(robot.position(), machine.extract_output());
            robot.turn(machine.extract_output());
            robot.move();
        }

        std::cout << panel.painted() << std::endl;
    }

    auto solve_part2 ()
    {
        auto machine = intcode_machine {program};
        auto robot   = painting_robot {};
        auto panel   = hull {};

        panel.paint(point {0, 0}, hull::white);

        while (not machine.halted())
        {
            machine.add_input(panel.color_at(robot.position()));
            machine.run();
            panel.paint(robot.position(), machine.extract_output());
            robot.turn(machine.extract_output());
            robot.move();
        }

        // plot them in excel or something similar
        panel.print_pixels(std::cout);
    }
}

auto main () -> int
{
    aoc::solve_part1();
    aoc::solve_part2();

    return 0;
}
