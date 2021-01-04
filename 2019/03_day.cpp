#include <vector>
#include <fstream>
#include <iostream>
#include <algorithm>
#include <string>
#include <utility>
#include <cmath>
#include <iterator>
#include <functional>

struct point
{
    int x;
    int y;
};

struct line
{
    point a;
    point b;
};

enum class direction
{
    up,
    down,
    left,
    right
};

enum class orientation
{
    horizontal,
    vertical
};

using wire = std::vector<point>;

auto parse_input () -> std::pair<wire, wire>
{
    struct move
    {
        direction dir;
        int       val;
    };

    auto split = [](auto const& s, auto const c)
    {
        auto const delims = {c};
        auto const end    = std::cend(s);
        auto first        = std::cbegin(s);
        auto words        = std::vector<std::string>();

        while (first != end)
        {
            auto const last = std::find_first_of( first, end
                                                , std::cbegin(delims)
                                                , std::cend(delims) );
            if (first != last)
            {
                words.emplace_back(first, last);
            }

            if (last == end)
            {
                break;
            }

            first = std::next(last);
        }

        return words;
    };

    auto parse_move = [](auto const& s)
    {
        auto const val = std::stoi(s.substr(1));
        switch (s[0])
        {
            case 'U': return move {direction::up,    val};
            case 'D': return move {direction::down,  val};
            case 'L': return move {direction::left,  val};
            case 'R': return move {direction::right, val};
            default: throw "not good";
        }
    };

    auto make_point = [p = point {0, 0}](auto const m) mutable
    {
        switch (m.dir)
        {
            case direction::up:    return p = point {p.x, p.y + m.val};
            case direction::down:  return p = point {p.x, p.y - m.val};
            case direction::left:  return p = point {p.x - m.val, p.y};
            case direction::right: return p = point {p.x + m.val, p.y};
            default: throw "not good";
        }
    };

    auto parse_row = [=](auto const& row)
    {
        auto const ws = split(row, ',');
        auto ms = std::vector<move>();
        std::transform( std::begin(ws), std::end(ws)
                      , std::back_inserter(ms), parse_move );

        auto ps = std::vector<point> {point {0, 0}};
        std::transform( std::begin(ms), std::end(ms)
                      , std::back_inserter(ps), make_point );
        return ps;
    };

    auto istr = std::fstream("./input/03_day.txt");
    auto row1 = std::string();
    auto row2 = std::string();
    std::getline(istr, row1);
    std::getline(istr, row2);

    return std::make_pair(parse_row(row1), parse_row(row2));
}

auto get_orientation (line const& l) -> orientation
{
    return l.a.x == l.b.x ? orientation::vertical
                          : orientation::horizontal;
}

auto is_between (int const x, int const a, int const b) -> bool
{
    return (x >= a and x <= b)
        or (x >= b and x <= a);
}

auto intersect (line const& l1, line const& l2) -> bool
{
    auto const o1 = get_orientation(l1);
    auto const o2 = get_orientation(l2);

    // This assumption doesn't hold in general.
    if (o1 == o2)
    {
        return false;
    }

    if (orientation::horizontal == o1)
    {
        return is_between(l2.a.x, l1.a.x, l1.b.x)
           and is_between(l1.a.y, l2.a.y, l2.b.y);
    }
    else
    {
        return is_between(l1.a.x, l2.a.x, l2.b.x)
           and is_between(l2.a.y, l1.a.y, l1.b.y);
    }
}

auto intersection (line const& l1, line const& l2) -> point
{
    auto const o1 = get_orientation(l1);
    return orientation::horizontal == o1 ? point {l2.a.x, l1.a.y}
                                         : point {l1.a.x, l2.a.y};
}

auto find_intersections (std::pair<wire, wire> const& ws) -> std::vector<point>
{
    auto is         = std::vector<point>();
    auto it1        = std::begin(ws.first);
    auto const end1 = std::end(ws.first);

    while (it1 != end1)
    {
        auto const l1   = line {*it1, *(++it1)};
        auto const end2 = std::end(ws.second);
        auto it2        = std::next(std::begin(ws.second));
        while (it2 != end2)
        {
            auto const l2 = line {*it2, *(++it2)};
            if (intersect(l1, l2))
            {
                is.push_back(intersection(l1, l2));
            }
        }
    }

    return is;
}

auto manhattan_dist (point const l, point const r = point {0, 0}) -> int
{
    return std::abs(l.x - r.x) + std::abs(l.y - r.y);
}

auto manhattan_dist_cmp (point const l, point const r) -> int
{
    return manhattan_dist(l) < manhattan_dist(r);
}

auto contains_point (line const& l, point const p) -> bool
{
    auto const o = get_orientation(l);
    return orientation::horizontal == o
        ? p.y == l.a.y and is_between(p.x, l.a.x, l.b.x)
        : p.x == l.a.x and is_between(p.y, l.a.y, l.b.y);
}

auto line_length (line const& l) -> int
{
    return manhattan_dist(l.a, l.b);
}

auto dist_to_point (wire const& ps, point const p) -> int
{
    auto d         = 0;
    auto it        = std::begin(ps);
    auto const end = std::end(ps);

    while (it != end)
    {
        auto const l = line {*it, *(++it)};
        if (contains_point(l, p))
        {
            d += manhattan_dist(l.a, p);
            break;
        }
        else
        {
            d += line_length(l);
        }
    }

    return d;
}

auto dist_to_point_cmp
    (wire const& ls1, wire const& ls2, point const p1, point const p2) -> int
{
    return dist_to_point(ls1, p1) + dist_to_point(ls2, p1)
         < dist_to_point(ls1, p2) + dist_to_point(ls2, p2);
}

auto solve_part_1 (std::pair<wire, wire> const& ws) -> int
{
    auto const is = find_intersections(ws);
    auto const i  = *std::min_element( std::begin(is), std::end(is)
                                     , manhattan_dist_cmp );
    return manhattan_dist(i);
}

auto solve_part_2 (std::pair<wire, wire> const& ws) -> int
{
    auto const is  = find_intersections(ws);
    auto const cmp = std::bind_front(dist_to_point_cmp, ws.first, ws.second);
    auto const i   = *std::min_element( std::begin(is), std::end(is)
                                      , cmp );
    return dist_to_point(ws.first, i) + dist_to_point(ws.second, i);
}

auto main () -> int
{
    auto const input = parse_input();
    std::cout << "Part 1: " << solve_part_1(input) << '\n';
    std::cout << "Part 2: " << solve_part_2(input) << '\n';
}