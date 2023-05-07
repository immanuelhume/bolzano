#pragma once

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

#define ASSERT_INVARIANTS                                                                          \
    do {                                                                                           \
        assert(_xs.size() == _hm.size());                                                          \
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
    An O(n) search through the list of keys. For debugging.
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
    PerfTimer(const std::string &_ident = "")
        : ident(_ident), start(std::chrono::high_resolution_clock::now()) {}

    ~PerfTimer() {
        auto stop     = std::chrono::high_resolution_clock::now();
        auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(stop - start);
        spdlog::info("{}: took {}ms", ident, duration.count());
    }

  private:
    std::string                           ident;
    std::chrono::system_clock::time_point start;
};

const float                EPSILON = 0.0001;
template <typename T> bool almost(T x, T y) { return std::abs(x - y) < EPSILON; }

inline SDL_Rect sdl_from_fz_rect(fz_rect rect) {
    return {static_cast<int>(rect.x0), static_cast<int>(rect.y0),
            static_cast<int>(rect.x1 - rect.x0), static_cast<int>(rect.y1 - rect.y0)};
}

#define ASSERT_SDL_RECT(rect)                                                                      \
    do {                                                                                           \
        assert(rect.w > 0);                                                                        \
        assert(rect.h > 0);                                                                        \
    } while (0)

#define ASSERT_FZ_RECT(rect)                                                                       \
    do {                                                                                           \
        assert(rect.x1 > rect.x0);                                                                 \
        assert(rect.y1 > rect.y0);                                                                 \
    } while (0)
