#pragma once

#include "doctest.h"
#include "tl/expected.hpp"
#include "util.hpp"
#include <memory>
#include <utility>
#include <vector>

inline std::pair<int, int> texture_size(SDL_Texture *texture) {
    int w, h;
    SDL_QueryTexture(texture, nullptr, nullptr, &w, &h);
    return {w, h};
}

inline bool are_rects_overlapping(fz_rect x, fz_rect y) {
    if (x.x1 <= y.x0 || y.y1 <= x.x0 || x.y1 <= y.y0 || y.y1 <= x.x0) {
        return false;
    }
    return true;
}

inline fz_rect get_bbox(fz_rect a, fz_rect b) {
    return {
        std::min(a.x0, b.x0),
        std::min(a.y0, b.y0),
        std::max(a.x1, b.x1),
        std::max(a.y1, b.y1),
    };
}

inline bool is_same_rect(fz_rect a, fz_rect b) {
    return almost(a.x0, b.x0) && almost(a.x1, b.x1) && almost(a.y0, b.y0) && almost(a.y1, b.y1);
}

TEST_CASE("get_bbox works") {
    SUBCASE("no overlaps") {
        fz_rect a{0.0, 0.0, 1.0, 1.0};
        fz_rect b{2.0, 0.0, 3.0, 1.0};
        fz_rect want{0.0, 0.0, 3.0, 1.0};
        fz_rect got = get_bbox(a, b);
        CHECK(is_same_rect(got, want));
    }
    SUBCASE("some overlap") {
        fz_rect a{0.0, 0.0, 1.0, 1.0};
        fz_rect b{-0.5, 0.25, 1.5, 0.75};
        fz_rect want{-0.5, 0.0, 1.5, 1.0};
        fz_rect got = get_bbox(a, b);
        CHECK(is_same_rect(got, want));
    }
}

TEST_CASE("are_rects_overlapping works") {
    fz_rect x{0.0, 0.0, 1.0, 1.0};
    SUBCASE("stacking") {
        fz_rect y{0.0, 1.0, 1.0, 2.0};
        CHECK(!are_rects_overlapping(x, y));
    }
    SUBCASE("side by side") {
        fz_rect y{1.0, 0.0, 2.0, 1.0};
        CHECK(!are_rects_overlapping(x, y));
    }
    SUBCASE("with some overlap") {
        fz_rect y{0.5, 0.5, 1.5, 1.5};
        CHECK(are_rects_overlapping(x, y));
    }
}

inline std::vector<fz_rect> rect_delta(fz_rect prev, fz_rect next) {
    if (is_same_rect(fz_empty_rect, next)) {
        return {};
    }

    if (!are_rects_overlapping(prev, next)) {
        return {next};
    }

    float x0 = std::max(prev.x0, next.x0);
    float x1 = std::min(prev.x1, next.x1);
    float y0 = std::max(prev.y0, next.y0);
    float y1 = std::min(prev.y1, next.y1);

    std::vector<fz_rect> res;

    if (next.x0 < x0) {
        res.push_back({next.x0, next.y0, x0, next.y1});
    }
    if (next.x1 > x1) {
        res.push_back({x1, next.y0, next.x1, next.y1});
    }
    if (next.y0 < y0) {
        res.push_back({x0, next.y0, x1, y0});
    }
    if (next.y1 > y1) {
        res.push_back({x0, y1, x1, next.y1});
    }

    return res;
}

TEST_CASE("rect_delta works") {
    fz_rect prev{0.0, 0.0, 1.0, 1.0};

    SUBCASE("no overlap") {
        fz_rect next{1.0, 1.0, 2.0, 2.0};
        auto    got = rect_delta(prev, next);
        CHECK(got.size() == 1);
        CHECK(is_same_rect(got[0], next));
    }
    SUBCASE("protrudes left") {
        fz_rect next{-0.5, 0.0, 0.5, 1.0};
        fz_rect want{-0.5, 0.0, 0.0, 1.0};
        auto    got = rect_delta(prev, next);
        CHECK(got.size() == 1);
        CHECK(is_same_rect(got[0], want));
    }
    SUBCASE("protrudes right") {
        fz_rect next{0.5, 0.0, 1.5, 1.0};
        fz_rect want{1.0, 0.0, 1.5, 1.0};
        auto    got = rect_delta(prev, next);
        CHECK(got.size() == 1);
        CHECK(is_same_rect(got[0], want));
    }
    SUBCASE("protrudes top") {
        fz_rect next{0.0, -0.5, 1.0, 0.5};
        fz_rect want{0.0, -0.5, 1.0, 0.0};
        auto    got = rect_delta(prev, next);
        CHECK(got.size() == 1);
        CHECK(is_same_rect(got[0], want));
    }
    SUBCASE("protrudes bottom") {
        fz_rect next{0.0, 0.5, 1.0, 1.5};
        fz_rect want{0.0, 1.0, 1.0, 1.5};
        auto    got = rect_delta(prev, next);
        CHECK(got.size() == 1);
        CHECK(is_same_rect(got[0], want));
    }

    SUBCASE("protrudes partially") {
        fz_rect next{0.25, -0.25, 0.75, 0.25};
        fz_rect want{0.25, -0.25, 0.75, 0.0};
        auto    got = rect_delta(prev, next);
        CHECK(got.size() == 1);
        CHECK(is_same_rect(got[0], want));
    }

    SUBCASE("protrudes top right") {
        fz_rect              next{0.75, -0.25, 1.25, 0.25};
        std::vector<fz_rect> want{{1.0, -0.25, 1.25, 0.25}, {0.75, -0.25, 1.0, 0.0}};

        auto got = rect_delta(prev, next);

        CHECK(got.size() == 2);
        CHECK(is_same_rect(got[0], want[0]));
        CHECK(is_same_rect(got[1], want[1]));
    }

    SUBCASE("next is empty rect") {
        auto got = rect_delta(prev, fz_empty_rect);

        CHECK(got.empty());
    }
    SUBCASE("prev is empty rect") {
        fz_rect next{0.0, 0.0, 1.0, 1.0};
        auto    got = rect_delta(fz_empty_rect, next);

        CHECK(got.size() == 1);
        CHECK(is_same_rect(got[0], next));
    }
    SUBCASE("next is enclosed in prev") {
        fz_rect next{0.0, 0.0, 0.5, 0.5};
        auto    got = rect_delta(prev, next);
        CHECK(got.empty());
    }
}

class Page {
  public:
    /**
    Creates a page. Does not take ownership of the display list.
    */
    Page(fz_context *ctx, int pnum, fz_display_list *dlist) {
        _ctx   = ctx;
        _pnum  = pnum;
        _dlist = dlist;

        spdlog::debug("Page {} constructed", pnum);
    }
    Page(const Page &rhs) {
        _ctx   = rhs._ctx;
        _pnum  = rhs._pnum;
        _dlist = rhs._dlist;

        _rect    = rhs._rect;
        _scaling = rhs._scaling;

        _texture  = rhs._texture;
        _renderer = rhs._renderer;

        spdlog::debug("Page {} copied", _pnum);
    }
    Page(Page &&rhs) {
        // Move contructor should be the same as copy constructor.
        _ctx   = rhs._ctx;
        _pnum  = rhs._pnum;
        _dlist = rhs._dlist;

        _rect    = rhs._rect;
        _scaling = rhs._scaling;

        _texture  = rhs._texture;
        _renderer = rhs._renderer;

        spdlog::debug("Page {} moved", _pnum);
    }
    ~Page() { spdlog::debug("Page {} destroyed", _pnum); }

    /**
    Initializes a texture to the dimensions of the page, after scaling. Subsequent requests to
    generate pixmaps will be copied onto the texture created by this function.

    renderer: SDL renderer.

    transform: Page scaling. This determines the size of the texture.
    */
    tl::expected<void, BolzanoError> init_texture(SDL_Renderer *renderer, float scaling) {
        fz_irect rect;
        fz_try(_ctx) {
            fz_rect tmp = fz_bound_display_list(_ctx, _dlist);
            tmp         = fz_transform_rect(tmp, fz_scale(scaling, scaling));
            rect        = fz_round_rect(tmp);
        }
        fz_catch(_ctx) {
            spdlog::error("Page::init_texture: failed to get page bounds: {}",
                          fz_caught_message(_ctx));
            return tl::make_unexpected(BolzanoError{ErrSource::mupdf, fz_caught_message(_ctx)});
        }

        int w = rect.x1 - rect.x0;
        int h = rect.y1 - rect.y0;

        SDL_Texture *texture =
            SDL_CreateTexture(renderer, SDL_PIXELFORMAT_RGB24, SDL_TEXTUREACCESS_STREAMING, w, h);
        if (!texture) {
            spdlog::error("Page::init_texture: failed to create SDL texture: {}", SDL_GetError());
            return tl::make_unexpected(BolzanoError{ErrSource::sdl, SDL_GetError()});
        }

        _renderer = renderer; // assign the renderer for future use
        _texture  = texture;
        _scaling  = scaling;

        return {};
    }

    /**
    Incrementally render a (part of) this page.

    rect: Part of the page to render. This is post-scaled.
    */
    tl::expected<void, BolzanoError> render(fz_rect rect) {
        assert(_renderer);
        assert(_texture);

        fz_rect bbox      = get_bbox(_rect, rect);
        auto    new_rects = rect_delta(_rect, bbox);

        fz_matrix transform     = fz_scale(_scaling, _scaling);
        fz_matrix inv_transform = fz_invert_matrix(transform);

        for (fz_rect src_rect : new_rects) {
            // Sometimes, the rectangle could be degenerate. Just skip it if so.
            if (src_rect.x1 <= src_rect.x0 || src_rect.y1 <= src_rect.y0) {
                continue;
            }

            fz_pixmap *pix = nullptr;

            fz_try(_ctx) {
                pix = fz_new_pixmap_with_bbox(_ctx, fz_device_rgb(_ctx), fz_round_rect(src_rect),
                                              nullptr, 0);
            }
            fz_catch(_ctx) {
                spdlog::error("Page::render: Failed to create pixmap: {}", fz_caught_message(_ctx));
                return tl::make_unexpected(BolzanoError{ErrSource::mupdf, fz_caught_message(_ctx)});
            }
            fz_try(_ctx) { fz_clear_pixmap_with_value(_ctx, pix, 0xFF); }
            fz_catch(_ctx) {
                spdlog::error("Page::render: Failed to clear pixmap: {}", fz_caught_message(_ctx));
                return tl::make_unexpected(BolzanoError{ErrSource::mupdf, fz_caught_message(_ctx)});
            }

            assert(pix != nullptr);

            spdlog::debug("Rendering to pixmap of {}x{}", pix->w, pix->h);

            fz_device *draw_dev;
            fz_try(_ctx) {
                draw_dev = fz_new_draw_device(_ctx, transform, pix);
                // Here, we use the src_rect to clip out the part of the page we want
                fz_run_display_list(_ctx, _dlist, draw_dev, fz_identity,
                                    fz_transform_rect(src_rect, inv_transform), nullptr);
                fz_close_device(_ctx, draw_dev);
            }
            fz_always(_ctx) { fz_drop_device(_ctx, draw_dev); }
            fz_catch(_ctx) {
                fz_drop_pixmap(_ctx, pix);
                spdlog::error("Page::render: Could not draw page: {}", fz_caught_message(_ctx));
                return tl::make_unexpected(BolzanoError{ErrSource::mupdf, fz_caught_message(_ctx)});
            }

            SDL_Rect       dst        = sdl_from_fz_rect(src_rect);
            unsigned char *pix_handle = nullptr;
            int            pitch;

            if (SDL_LockTexture(_texture, &dst, reinterpret_cast<void **>(&pix_handle), &pitch) <
                0) {
                fz_drop_pixmap(_ctx, pix); // drop the pixmap here! otherwise it'll leak
                spdlog::error("While rendering page {}:", _pnum);
                spdlog::error("Could not lock texture at {}: {}", (void *)_texture, SDL_GetError());
                return tl::make_unexpected(BolzanoError{ErrSource::sdl, SDL_GetError()});
            }
            for (int r = 0; r < pix->h; r++) {
                // We need to copy each row separately
                memcpy(pix_handle + r * pitch, pix->samples + r * pix->w * pix->n, pix->w * pix->n);
            }
            SDL_UnlockTexture(_texture);

            fz_drop_pixmap(_ctx, pix);
        }

        _rect = bbox; // we've cleared this region

        return {};
    }

    /**
    Retrieves a (weak) reference to the SDL texture.
    */
    SDL_Texture *texture() { return _texture; }

    /**
    Gets the page number.
    */
    int num() { return _pnum; }

  private:
    int _pnum;

    fz_context      *_ctx   = nullptr;
    fz_display_list *_dlist = nullptr;

    fz_rect _rect    = fz_empty_rect;
    float   _scaling = 1.0;

    // @memory: we should really use a smart pointer here
    SDL_Texture  *_texture  = nullptr;
    SDL_Renderer *_renderer = nullptr;
};

/**
Part of a page, and its translations into screen space.
*/
struct PageRect {
    int      pnum;
    fz_rect  src;
    SDL_Rect dst;
};

/**
Maps a position in the window to a position on some page.
*/
struct PositionTracker {
    int   pnum;
    float page_xoff;
    float page_yoff;

    // These are integers coz that's what SDL uses.
    int scr_xoff;
    int scr_yoff;
};

class Document {
  public:
    Document(char *filename, float scaling = 300.0 / 72.0) {
        _ctx = fz_new_context(nullptr, nullptr, FZ_STORE_UNLIMITED);
        if (!_ctx) {
            spdlog::error("Failed to create mupdf context");
            exit(1);
        }

        fz_try(_ctx) {
            fz_register_document_handlers(_ctx);
            _doc      = fz_open_document(_ctx, filename);
            _nr_pages = fz_count_pages(_ctx, _doc);

            spdlog::debug("Found {} pages in the document", _nr_pages);
        }
        fz_catch(_ctx) {
            spdlog::error("Could not open document: {}", fz_caught_message(_ctx));
            exit(1); // we couldn't open the document, so there's no point continuing the program
        }

        assert(_doc != nullptr);

        _scaling = scaling;

        _dlists.resize(_nr_pages);
        _stext_pages.resize(_nr_pages);

        for (int i = 0; i < _nr_pages; i++) {
            fz_page   *page = nullptr;
            fz_device *dev  = nullptr;
            fz_try(_ctx) {
                page = fz_load_page(_ctx, _doc, i);
                assert(page != nullptr);

                _dlists[i] = fz_new_display_list_from_page(_ctx, page);
                assert(_dlists[i] != nullptr);

                dev = fz_new_list_device(_ctx, _dlists[i]);
                fz_run_page(_ctx, page, dev, fz_identity, nullptr); // really fz_identity?
                fz_close_device(_ctx, dev);

                _stext_pages[i] = fz_new_stext_page_from_display_list(_ctx, _dlists[i], nullptr);
            }
            fz_always(_ctx) {
                fz_drop_device(_ctx, dev);
                fz_drop_page(_ctx, page);
            }
            fz_catch(_ctx) {
                spdlog::error("Could not load page: {}", fz_caught_message(_ctx));
                exit(1); // @todo: determine what to do?
            }
        }

        // @memory: This is important af. But it is also incovenient, and should be automatically
        // handled by smart ptrs.
        _renders.on_delete = [](Page &p) {
            spdlog::debug("Destroying texture at {} (page {})", (void *)p.texture(), p.num());
            SDL_DestroyTexture(p.texture());
        };
    }

    Document(const Document &rhs) = delete;
    Document(Document &&rhs)      = delete;

    ~Document() {
        for (int i = 0; i < _nr_pages; i++) {
            fz_drop_display_list(_ctx, _dlists[i]);
            fz_drop_stext_page(_ctx, _stext_pages[i]);
        }
        fz_drop_document(_ctx, _doc);
    }

    int count_pages() { return _nr_pages; }

    void scroll_y(PositionTracker &pos, int dy, int gap) {
        pos.page_yoff += dy;
        int ph = height(pos.pnum);

        if ((0 <= pos.page_yoff && pos.page_yoff <= ph) || (dy > 0 && pos.pnum == _nr_pages - 1) ||
            (dy < 0 && pos.pnum == 0)) {
            // If (A) we did not scroll into the prev/next page, or (B) we're on the first/last
            // page, there's nothing else to do.
            return;
        }

        if (pos.page_yoff < 0) {
            pos.pnum--;
            pos.page_yoff = height(pos.pnum) + pos.page_yoff + gap;
        } else {
            pos.pnum++;
            pos.page_yoff -= ph + gap;
        }
    }

    void scroll_x(PositionTracker &pos, int dx, int gap) {
        pos.page_xoff += dx;
        return;
    }

    /**
    Returns a list of pages and their rectangles which should appear, given some window dimensions
    and current position. The horizontal offset of each page will be based on the position given.

    All computations are done post-scaled.
    */
    std::vector<PageRect> tile1(float winw, float winh, const PositionTracker &pos, int gap) {
        assert(pos.pnum >= 0);
        assert(pos.pnum < _nr_pages);

        std::vector<PageRect> tiles;

        auto [pw, ph] = page_dim(pos.pnum);

        fz_rect src{
            std::max(pos.page_xoff - pos.scr_xoff, (float)0.0),
            std::max(pos.page_yoff - pos.scr_yoff, (float)0.0),
            std::min(pos.page_xoff + (winw - pos.scr_xoff), pw),
            std::min(pos.page_yoff + (winh - pos.scr_yoff), ph),
        };
        SDL_Rect dst{
            static_cast<int>(std::max(pos.scr_xoff - pos.page_xoff, (float)0.0)),
            static_cast<int>(std::max(pos.scr_yoff - pos.page_yoff, (float)0.0)),
            static_cast<int>(src.x1 - src.x0),
            static_cast<int>(src.y1 - src.y0),
        };

        tiles.push_back({pos.pnum, src, dst});

        // The remaining heights on screen to fill, <= for both means that we're done.
        int htop = pos.scr_yoff - pos.page_yoff - gap;
        int hbot = (winh - pos.scr_yoff) - (ph - pos.page_yoff) - gap;

        float xx = static_cast<float>(pos.page_xoff) / pw;
        tile_up(pos.pnum - 1, winw, htop, gap, pos.scr_xoff, xx, tiles);
        tile_down(pos.pnum + 1, winw, hbot, winh, gap, pos.scr_xoff, xx, tiles);

        return tiles;
    }

    tl::expected<SDL_Texture *, BolzanoError> render(SDL_Renderer *renderer, const PageRect &tile) {
        // spdlog::debug("Got request to render page {} with rect({}, {}, {}, {})", tile.pnum,
        //               tile.src.x0, tile.src.y0, tile.src.x1, tile.src.y1);
        Page *p = _renders.get_or_insert(tile.pnum, [this, &tile, renderer]() {
            Page new_p(_ctx, tile.pnum, _dlists[tile.pnum]);
            new_p.init_texture(renderer, _scaling);
            return new_p;
        });
        return p->render(tile.src).map([p]() { return p->texture(); });
    }

    /**
    Returns dimensions of some page w.r.t. the current scaling.
    */
    std::pair<float, float> page_dim(int pnum) {
        assert(pnum >= 0);
        assert(pnum < _nr_pages);

        fz_rect rect = fz_bound_display_list(_ctx, _dlists[pnum]);
        return {_scaling * (rect.x1 - rect.x0), _scaling * (rect.y1 - rect.y0)};
    }

    int width(int pnum) { return page_dim(pnum).first; }

    int height(int pnum) { return page_dim(pnum).second; }

    void scale_by(float factor) {
        if (factor == 1.0) {
            return;
        }
        _scaling *= factor;
        _renders.clear();
    }

    /**
    @tmp: centralizes the document vertically.
    @todo: centralize horizontally also.
    */
    void try_centralize(PositionTracker &pos, int winw, int winh) {
        assert(pos.pnum >= 0);
        assert(pos.pnum < _nr_pages);

        auto [pw, ph] = page_dim(pos.pnum);

        if (ph < winh) {
            // pos.page_xoff = pw / 2.0;
            pos.page_yoff = ph / 2.0;
            // pos.scr_xoff  = winw / 2.0;
            pos.scr_yoff = winh / 2.0;
        } else {
            pos.page_yoff = 0;
            pos.scr_yoff  = 0;
        }
    }

  private:
    /**
    Tiles pages upwards.

    pnum: Page number to start from.

    w: Viewport width.

    h: Remaining viewport height. This function will fill this height with pages.

    gap: Gap between pages.

    scr_xoff: An x coordinate of the viewport.

    xx: Fraction of a page's width which will be mapped to scr_xoff. We use this because each
    page might have a different width, and we still need some way of determining how to position
    the page horizontally.

    tiles: The list of tiles to append to.
    */
    void tile_up(int pnum, int w, int h, int gap, int scr_xoff, float xx,
                 std::vector<PageRect> &tiles) {
        while (h > 0 && pnum >= 0) {
            auto [pw, ph] = page_dim(pnum);
            int page_xoff = xx * pw;

            fz_rect src{
                static_cast<float>(std::max(page_xoff - scr_xoff, 0)),
                static_cast<float>(std::max(ph - h, (float)0)),
                static_cast<float>(std::min(static_cast<float>(page_xoff + (w - scr_xoff)), pw)),
                std::max(static_cast<float>(h), ph),
            };
            SDL_Rect dst{
                std::max(scr_xoff - page_xoff, 0),
                std::max(static_cast<int>(h - ph), 0),
                static_cast<int>(src.x1 - src.x0),
                static_cast<int>(src.y1 - src.y0),
            };

            ASSERT_FZ_RECT(src);
            ASSERT_SDL_RECT(dst);

            tiles.push_back({pnum, src, dst});

            h -= ph;
            h -= gap;
            pnum--;
        }
    }

    /**
    See tile_up. This function takes an extra "winh" parameter for the window height.
    */
    void tile_down(int pnum, int w, int h, int winh, int gap, int scr_xoff, float xx,
                   std::vector<PageRect> &tiles) {
        while (h > 0 && pnum < _nr_pages) {
            auto [pw, ph] = page_dim(pnum);
            int page_xoff = xx * pw;

            fz_rect src{
                static_cast<float>(std::max(page_xoff - scr_xoff, 0)),
                0,
                static_cast<float>(std::min(static_cast<float>(page_xoff + (w - scr_xoff)), pw)),
                std::min(static_cast<float>(h), ph),
            };
            SDL_Rect dst{
                std::max(scr_xoff - page_xoff, 0),
                std::max(winh - h, 0),
                static_cast<int>(src.x1 - src.x0),
                static_cast<int>(src.y1 - src.y0),
            };

            ASSERT_FZ_RECT(src);
            ASSERT_SDL_RECT(dst);

            tiles.push_back({pnum, src, dst});

            h -= ph;
            h -= gap;
            pnum++;
        }
    }

  private:
    fz_context  *_ctx = nullptr;
    fz_document *_doc = nullptr;

    std::vector<fz_display_list *> _dlists;
    std::vector<fz_stext_page *>   _stext_pages;
    LruKv<int, Page>               _renders;

    int   _nr_pages;
    float _scaling;
};
