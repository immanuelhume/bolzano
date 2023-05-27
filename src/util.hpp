#pragma once

#include "doctest.h"
#include "mupdf/fitz.h"
#include "spdlog/spdlog.h"
#include <SDL2/SDL.h>
#include <chrono>
#include <cstdint>
#include <exception>
#include <functional>
#include <iostream>
#include <list>
#include <optional>
#include <stdexcept>
#include <unordered_map>

template <typename K, typename V> class LruKv {

#define ASSERT_INVARIANTS                                                                                              \
    do {                                                                                                               \
        assert(_xs.size() == _hm.size());                                                                              \
    } while (0);

  public:
    LruKv(uint32_t capacity = 16) : _capacity(capacity) {}

    void insert(const K &key, const V &value) {
        auto it = _hm.find(key);

        if (it != _hm.end()) {
            _xs.erase(it->second.first);
        }

        _xs.push_front(key);
        _hm.insert({key, {_xs.begin(), value}});

        if (_hm.size() <= _capacity) {
            ASSERT_INVARIANTS;
            return;
        }

        // Do not manipulate the linked list here!
        spdlog::debug("LRU cache reached capacity, evicting item no. {}", _xs.back());

        K to_delete = _xs.back();

        assert(contains(to_delete));
        assert(contains_key(to_delete));

        if (on_delete) {
            on_delete(_hm.find(to_delete)->second.second);
        }

        _hm.erase(to_delete);
        _xs.pop_back();

        assert(!contains(to_delete));
        assert(!contains_key(to_delete));

        ASSERT_INVARIANTS;
    }

    V *get(const K &key) {
        auto it = _hm.find(key);

        if (it == _hm.end()) {
            return nullptr;
        }
        _xs.splice(_xs.begin(), _xs, it->second.first);

        ASSERT_INVARIANTS;

        return &it->second.second;
    }

    V *get_or_insert(const K &key, std::function<V(void)> producer) {
        if (!contains(key)) {
            insert(key, producer());
        }
        assert(contains(key));
        ASSERT_INVARIANTS;
        return get(key);
    }

    void clear() {
        if (on_delete) {
            for_each(on_delete);
        }
        _xs.clear();
        _hm.clear();

        ASSERT_INVARIANTS;
        assert(_hm.size() == 0);
        assert(_xs.size() == 0);
    }

    bool contains(const K &key) const { return _hm.find(key) != _hm.end(); }

    void set_capacity(uint32_t capacity) {
        _capacity = capacity;

        while (_hm.size() > _capacity) {
            _hm.erase(_xs.back());
            _xs.pop_back();
        }
        ASSERT_INVARIANTS;
    }

    uint32_t size() const { return _hm.size(); }

    void for_each(std::function<void(const V &)> f) const {
        for (auto &[_, v] : _hm) {
            f(v.second);
        }
        ASSERT_INVARIANTS;
    }

    /**
    Iterate through values, mutably.
    */
    void for_each(std::function<void(V &)> f) {
        for (auto &[_, v] : _hm) {
            f(v.second);
        }
        ASSERT_INVARIANTS;
    }

  private:
    /**
    A linear search through the list of keys. For debugging.
    */
    bool contains_key(const K &key) {
        for (const K &x : _xs) {
            if (x == key) {
                return true;
            }
        }
        return false;
    }

  public:
    std::function<void(V &)> on_delete = nullptr;

  private:
    uint32_t                                                             _capacity;
    std::list<K>                                                         _xs;
    std::unordered_map<K, std::pair<typename std::list<K>::iterator, V>> _hm;
};

enum class ErrSource {
    mupdf,
    sdl,
    bolzano,
};

struct BolzanoError {
    ErrSource   source_hint;
    std::string desc;
};

class PerfTimer {
  public:
    PerfTimer(const std::string &_ident = "") : ident(_ident), start(std::chrono::high_resolution_clock::now()) {}

    ~PerfTimer() {
        auto stop     = std::chrono::high_resolution_clock::now();
        auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(stop - start);
        spdlog::info("{}: took {}ms", ident, duration.count());
    }

  private:
    std::string                           ident;
    std::chrono::system_clock::time_point start;
};

inline std::pair<int, int> texture_size(SDL_Texture *texture) {
    int w, h;
    SDL_QueryTexture(texture, nullptr, nullptr, &w, &h);
    return {w, h};
}

#define ASSERT_SDL_RECT(rect)                                                                                          \
    do {                                                                                                               \
        assert(rect.w > 0);                                                                                            \
        assert(rect.h > 0);                                                                                            \
    } while (0)

#define ASSERT_FZ_RECT(rect)                                                                                           \
    do {                                                                                                               \
        assert(rect.x1 > rect.x0);                                                                                     \
        assert(rect.y1 > rect.y0);                                                                                     \
    } while (0)

/**
A wrapper around fz_rect, fz_irect, and SDL_Rect nonsense.
*/
class Rect {
  public:
    Rect() { _rect = fz_empty_rect; }
    Rect(const fz_rect &rhs) { _rect = rhs; }
    Rect(const fz_irect &rhs) {
        _rect.x0 = rhs.x0;
        _rect.y0 = rhs.y0;
        _rect.x1 = rhs.x1;
        _rect.y1 = rhs.y1;
    }
    Rect(const SDL_Rect &rhs) {
        _rect.x0 = rhs.x;
        _rect.y0 = rhs.y;
        _rect.x1 = rhs.x + rhs.w;
        _rect.y1 = rhs.y + rhs.h;
    }
    Rect(float x0, float y0, float x1, float y1) { _rect = {x0, y0, x1, y1}; }

    fz_rect as_fz_rect() const { return _rect; }

    fz_irect as_fz_irect() const {
        fz_irect ret;
        ret.x0 = std::floor(_rect.x0);
        ret.y0 = std::floor(_rect.y0);
        ret.x1 = std::floor(_rect.x1);
        ret.y1 = std::floor(_rect.y1);
        return ret;
    }

    SDL_Rect as_sdl_rect() const {
        fz_irect r = as_fz_irect();
        return {r.x0, r.y0, r.x1 - r.x0, r.y1 - r.y0};
    }

    float  x0() const { return _rect.x0; }
    float &x0_mut() { return _rect.x0; }
    float  y0() const { return _rect.y0; }
    float &y0_mut() { return _rect.y0; }
    float  x1() const { return _rect.x1; }
    float &x1_mut() { return _rect.x1; }
    float  y1() const { return _rect.y1; }
    float &y1_mut() { return _rect.y1; }
    float  w() const { return _rect.x1 - _rect.x0; }
    float  h() const { return _rect.y1 - _rect.y0; }

    bool operator==(const Rect &other) const {
        constexpr float EPSILON = 1e-6f;
        return std::fabs(_rect.x0 - other._rect.x0) < EPSILON && std::fabs(_rect.y0 - other._rect.y0) < EPSILON &&
               std::fabs(_rect.x1 - other._rect.x1) < EPSILON && std::fabs(_rect.y1 - other._rect.y1) < EPSILON;
    }

    bool is_empty() { return _rect.x0 >= _rect.x1 || _rect.y0 >= _rect.y1; }

    bool contains_point(float x, float y) { return _rect.x0 <= x && _rect.x1 >= x && _rect.y0 <= y && _rect.y1 >= y; }

    /**
    Returns a new rectangle such that it fits within another. Does not modify itself.
    */
    Rect bound(const Rect &oth) {
        float x0 = std::max(_rect.x0, oth.x0());
        float y0 = std::max(_rect.y0, oth.y0());
        float x1 = std::min(_rect.x1, oth.x1());
        float y1 = std::min(_rect.y1, oth.y1());
        return Rect(x0, y0, x1, y1);
    }

  private:
    fz_rect _rect;
};

template <> struct fmt::formatter<fz_rect> {
    constexpr auto parse(format_parse_context &ctx) { return ctx.begin(); }

    template <typename FormatContext> auto format(const fz_rect &rect, FormatContext &ctx) {
        return format_to(ctx.out(), "fz_rect: x0={}, y0={}, x1={}, y1={}", rect.x0, rect.y0, rect.x1, rect.y1);
    }
};

template <> struct fmt::formatter<fz_irect> {
    constexpr auto parse(format_parse_context &ctx) { return ctx.begin(); }

    template <typename FormatContext> auto format(const fz_irect &rect, FormatContext &ctx) {
        return format_to(ctx.out(), "fz_irect: x0={}, y0={}, x1={}, y1={}", rect.x0, rect.y0, rect.x1, rect.y1);
    }
};

template <> struct fmt::formatter<SDL_Rect> {
    constexpr auto parse(format_parse_context &ctx) { return ctx.begin(); }

    template <typename FormatContext> auto format(const SDL_Rect &rect, FormatContext &ctx) {
        return format_to(ctx.out(), "SDL_Rect: x={}, y={}, w={}, h={}", rect.x, rect.y, rect.w, rect.h);
    }
};

template <> struct fmt::formatter<Rect> {
    constexpr auto parse(format_parse_context &ctx) { return ctx.begin(); }

    template <typename FormatContext> auto format(const Rect &rect, FormatContext &ctx) {
        return format_to(ctx.out(), "Rect: x0={}, y0={}, x1={}, y1={}", rect.x0(), rect.y0(), rect.x1(), rect.y1());
    }
};

inline bool is_inside(fz_rect rect, float x, float y) {
    return rect.x0 <= x && rect.x1 >= x && rect.y0 <= y && rect.y1 >= y;
}

inline bool is_inside(SDL_Rect rect, float x, float y) {
    return rect.x <= x && rect.x + rect.w >= x && rect.y <= y && rect.y + rect.h >= y;
}

inline bool are_rects_overlapping(Rect a, Rect b) {
    if (a.x1() <= b.x0() || b.x1() <= a.x0() || a.y1() <= b.y0() || b.y1() <= a.y0()) {
        return false;
    }
    return true;
}

inline Rect get_bbox(Rect a, Rect b) {
    return Rect(std::min(a.x0(), b.x0()), std::min(a.y0(), b.y0()), std::max(a.x1(), b.x1()), std::max(a.y1(), b.y1()));
}

TEST_CASE("get_bbox works") {
    SUBCASE("no overlaps") {
        Rect a(0.0, 0.0, 1.0, 1.0);
        Rect b(2.0, 0.0, 3.0, 1.0);
        Rect want(0.0, 0.0, 3.0, 1.0);
        Rect got = get_bbox(a, b);
        CHECK(got == want);
    }
    SUBCASE("some overlap") {
        Rect a(0.0, 0.0, 1.0, 1.0);
        Rect b(-0.5, 0.25, 1.5, 0.75);
        Rect want(-0.5, 0.0, 1.5, 1.0);
        Rect got = get_bbox(a, b);
        CHECK(got == want);
    }
}

TEST_CASE("are_rects_overlapping works") {
    Rect a(0.0, 0.0, 1.0, 1.0);
    SUBCASE("stacking") {
        Rect b(0.0, 1.0, 1.0, 2.0);
        CHECK(!are_rects_overlapping(a, b));
    }
    SUBCASE("side by side") {
        Rect b(1.0, 0.0, 2.0, 1.0);
        CHECK(!are_rects_overlapping(a, b));
    }
    SUBCASE("with some overlap") {
        Rect b(0.5, 0.5, 1.5, 1.5);
        CHECK(are_rects_overlapping(a, b));
    }
    SUBCASE("enclosed within") {
        Rect b(0.25, 0.25, 0.75, 0.75);
        CHECK(are_rects_overlapping(a, b));
    }
    SUBCASE("same rectangle") { CHECK(are_rects_overlapping(a, a)); }
}

inline std::vector<Rect> rect_delta(Rect prev, Rect next) {
    if (next.is_empty()) {
        return {};
    }

    if (!are_rects_overlapping(prev, next)) {
        return {next};
    }

    float x0 = std::max(prev.x0(), next.x0());
    float x1 = std::min(prev.x1(), next.x1());
    float y0 = std::max(prev.y0(), next.y0());
    float y1 = std::min(prev.y1(), next.y1());

    std::vector<Rect> res;

    if (next.x0() < x0) {
        res.push_back({next.x0(), next.y0(), x0, next.y1()});
    }
    if (next.x1() > x1) {
        res.push_back({x1, next.y0(), next.x1(), next.y1()});
    }
    if (next.y0() < y0) {
        res.push_back({x0, next.y0(), x1, y0});
    }
    if (next.y1() > y1) {
        res.push_back({x0, y1, x1, next.y1()});
    }

    return res;
}

TEST_CASE("rect_delta works") {
    Rect prev(0.0, 0.0, 1.0, 1.0);

    SUBCASE("no overlap") {
        Rect next(0.0, 1.0, 2.0, 2.0);
        auto got = rect_delta(prev, next);
        CHECK(got.size() == 1);
        CHECK(got[0] == next);
    }
    SUBCASE("protrudes left") {
        Rect next(-0.5, 0.0, 0.5, 1.0);
        Rect want(-0.5, 0.0, 0.0, 1.0);
        auto got = rect_delta(prev, next);
        CHECK(got.size() == 1);
        CHECK(got[0] == want);
    }
    SUBCASE("protrudes right") {
        Rect next(0.5, 0.0, 1.5, 1.0);
        Rect want(1.0, 0.0, 1.5, 1.0);
        auto got = rect_delta(prev, next);
        CHECK(got.size() == 1);
        CHECK(got[0] == want);
    }
    SUBCASE("protrudes top") {
        Rect next(0.0, -0.5, 1.0, 0.5);
        Rect want(0.0, -0.5, 1.0, 0.0);
        auto got = rect_delta(prev, next);
        CHECK(got.size() == 1);
        CHECK(got[0] == want);
    }
    SUBCASE("protrudes bottom") {
        Rect next(0.0, 0.5, 1.0, 1.5);
        Rect want(0.0, 1.0, 1.0, 1.5);
        auto got = rect_delta(prev, next);
        CHECK(got.size() == 1);
        CHECK(got[0] == want);
    }

    SUBCASE("protrudes partially") {
        Rect next(0.25, -0.25, 0.75, 0.25);
        Rect want(0.25, -0.25, 0.75, 0.0);
        auto got = rect_delta(prev, next);
        CHECK(got.size() == 1);
        CHECK(got[0] == want);
    }

    SUBCASE("protrudes top right") {
        Rect              next(0.75, -0.25, 1.25, 0.25);
        std::vector<Rect> want{Rect(1.0, -0.25, 1.25, 0.25), Rect(0.75, -0.25, 1.0, 0.0)};

        auto got = rect_delta(prev, next);

        CHECK(got.size() == 2);
        CHECK(got[0] == want[0]);
        CHECK(got[1] == want[1]);
    }

    SUBCASE("next is empty rect") {
        auto got = rect_delta(prev, fz_empty_rect);

        CHECK(got.empty());
    }
    SUBCASE("prev is empty rect") {
        Rect next(0.0, 0.0, 1.0, 1.0);
        auto got = rect_delta(fz_empty_rect, next);

        CHECK(got.size() == 1);
        CHECK(got[0] == next);
    }
    SUBCASE("next is enclosed in prev") {
        Rect next(0.0, 0.0, 0.5, 0.5);
        auto got = rect_delta(prev, next);
        CHECK(got.empty());
    }

    SUBCASE("exactly same") {
        Rect r(1130.19824, 0, 1225.19824, 35.9321289);
        auto got = rect_delta(r, r);
        CHECK(got.empty());
    }
}

enum class FindRefStatus : uint8_t {
    Ok,         // found both label and refnum
    Invalid,    // no refnum
    RefnumOnly, // found a refnum, but no label
    CheckPrev,  // found refnum, check previous line for label
};

struct FindRefResult {
    std::string   label;
    std::string   refnum;
    FindRefStatus status;
};

template <> struct fmt::formatter<FindRefStatus> {
    constexpr auto parse(format_parse_context &ctx) { return ctx.begin(); }

    template <typename FormatContext> auto format(const FindRefStatus &status, FormatContext &ctx) {
        std::string_view statusStr;
        switch (status) {
        case FindRefStatus::Ok:
            statusStr = "Ok";
            break;
        case FindRefStatus::Invalid:
            statusStr = "Invalid";
            break;
        case FindRefStatus::RefnumOnly:
            statusStr = "RefnumOnly";
            break;
        case FindRefStatus::CheckPrev:
            statusStr = "CheckPrev";
            break;
        }
        return format_to(ctx.out(), "{}", statusStr);
    }
};

template <> struct fmt::formatter<FindRefResult> {
    constexpr auto parse(format_parse_context &ctx) { return ctx.begin(); }

    template <typename FormatContext> auto format(const FindRefResult &result, FormatContext &ctx) {
        return format_to(ctx.out(), "label: {}, refnum: {}, status: {}", result.label, result.refnum, result.status);
    }
};

template <> struct fmt::formatter<fz_point> {
    constexpr auto parse(format_parse_context &ctx) { return ctx.begin(); }

    template <typename ParseContext> auto format(const fz_point &point, ParseContext &ctx) {
        return fmt::format_to(ctx.out(), "({},{})", point.x, point.y);
    }
};

template <> struct fmt::formatter<fz_quad> {
    constexpr auto parse(format_parse_context &ctx) { return ctx.begin(); }

    template <typename ParseContext> auto format(const fz_quad &quad, ParseContext &ctx) {
        return fmt::format_to(ctx.out(), "ul: {}, ur: {}, ll: {}, lr: {}", quad.ul, quad.ur, quad.ll, quad.lr);
    }
};