#include <array>
#include <charconv>
#include <cmath>
#include <fstream>
#include <iostream>
#include <unordered_set>
#include <vector>

struct instruction
{
    int direction_;
    int distance_;
};

struct vec_2
{
    int x_;
    int y_;
};

struct state
{
    vec_2 position_ {0, 0};
    int orientation_ {0};
};

auto constexpr LEFT = -1;
auto constexpr RIGHT = 1;
auto constexpr NORTH = vec_2 {0, -1};
auto constexpr SOUTH = vec_2 {0, 1};
auto constexpr EAST = vec_2 {1, 0};
auto constexpr WEST = vec_2 {-1, 0};
auto constexpr Orientations = std::array<vec_2, 4> {NORTH, EAST, SOUTH, WEST};

auto read_input () -> std::vector<instruction>
{
    auto ifst = std::fstream("input/day_01.txt");
    auto token = std::string();
    auto instructions = std::vector<instruction>();
    while (std::getline(ifst, token, ','))
    {
        auto const last = token.data() + token.size();
        auto first = token.data();
        while (*first == ' ')
        {
            ++first;
        }
        auto const direction = *first == 'R' ? 1 : -1;
        ++first;
        auto distance = int();
        std::from_chars(first, last, distance);
        instructions.emplace_back(instruction {direction, distance});
    }
    return instructions;
}

auto vec_add (vec_2 const lhs, vec_2 const rhs) -> vec_2
{
    return vec_2 {lhs.x_ + rhs.x_, lhs.y_ + rhs.y_};
}

auto vec_mul (vec_2 const lhs, vec_2 const rhs) -> vec_2
{
    return vec_2 {lhs.x_ * rhs.x_, lhs.y_ * rhs.y_};
}

auto update (state const s, instruction const i) -> state
{
    auto const map_index = [](auto const i)
    {
        return i < 0 ? i + 4 : (i % 4);
    };
    auto const newIndex = map_index(s.orientation_ + i.direction_);
    auto const newOrientation = Orientations[newIndex];
    auto const delta = vec_mul(newOrientation, {i.distance_, i.distance_});
    auto const newPosition = vec_add(s.position_, delta);
    return state {newPosition, newIndex};
}

auto l1_norm (vec_2 const p) -> int
{
    return std::abs(p.x_) + std::abs(p.y_);
}

auto part_1 (std::vector<instruction> const& is) -> int
{
    auto s = state {};
    for (auto const i : is)
    {
        s = update(s, i);
    }
    return l1_norm(s.position_);
}

auto part_2 (std::vector<instruction> const& is) -> int
{
    auto const hash = [](vec_2 const v)
    {
        auto const hx = std::hash<int>()(v.x_);
        auto const hy = std::hash<int>()(v.y_);
        return hy + 0x9e3779b9 + (hx << 6) + (hx >> 2);
    };
    auto const eq = [](vec_2 const lhs, vec_2 const rhs)
    {
        return lhs.x_ == rhs.x_ && lhs.y_ == rhs.y_;
    };
    using hash_t = decltype(hash);
    using eq_t = decltype(eq);

    auto visited = std::unordered_set<vec_2, hash_t, eq_t>(is.size(), hash, eq);
    auto s = state {};
    visited.insert(s.position_);
    for (auto const i : is)
    {
        s = update(s, i);
        // TODO treba vkladať všetky diskrétne body priamky...
        if (visited.contains(s.position_))
        {
            return l1_norm(s.position_);
        }
        visited.insert(s.position_);
    }
    return -1;
}

auto main () -> int
{
    auto input = read_input();
    std::cout << "part 1: " << part_1(input) << '\n';
    std::cout << "part 2: " << part_2(input) << '\n';
}