#pragma once

#include "mupdf/fitz.h"
#include "tl/expected.hpp"
#include "util.hpp"
#include <deque>
#include <memory>
#include <regex>
#include <utility>

class Page {
  public:
    /**
    Creates a page. Does not take ownership of the display list.
    */
    Page(fz_context *ctx, int pnum, fz_display_list *dlist, fz_separations *sep) {
        _ctx   = ctx;
        _pnum  = pnum;
        _dlist = dlist;
        _sep   = sep;

        spdlog::debug("Page {} constructed", pnum);
    }
    Page(const Page &rhs) {
        _ctx   = rhs._ctx;
        _pnum  = rhs._pnum;
        _dlist = rhs._dlist;
        _sep   = rhs._sep;

        _rendered_rect = rhs._rendered_rect;
        _scaling       = rhs._scaling;

        _texture  = rhs._texture;
        _renderer = rhs._renderer;

        spdlog::debug("Page {} copied", _pnum);
    }
    Page(Page &&rhs) {
        // Move contructor should be the same as copy constructor.
        _ctx   = rhs._ctx;
        _pnum  = rhs._pnum;
        _dlist = rhs._dlist;
        _sep   = rhs._sep;

        _rendered_rect = rhs._rendered_rect;
        _scaling       = rhs._scaling;

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
        Rect page_rect;
        fz_try(_ctx) {
            fz_rect tmp = fz_bound_display_list(_ctx, _dlist);
            tmp         = fz_transform_rect(tmp, fz_scale(scaling, scaling));
            page_rect   = Rect(tmp);
        }
        fz_catch(_ctx) {
            spdlog::error("Page::init_texture: failed to get page bounds: {}",
                          fz_caught_message(_ctx));
            return tl::make_unexpected(BolzanoError{ErrSource::mupdf, fz_caught_message(_ctx)});
        }

        SDL_Rect tex_rect = page_rect.as_sdl_rect();

        SDL_Texture *texture = SDL_CreateTexture(
            renderer, SDL_PIXELFORMAT_RGB24, SDL_TEXTUREACCESS_STREAMING, tex_rect.w, tex_rect.h);
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
    tl::expected<void, BolzanoError> render(Rect rect) {
        assert(_renderer);
        assert(_texture);

        // Obviously there might be parts of `rect` which we have already rendered. Diff it against
        // what we've rendered so far, and see if there are new rectangles we need to render.
        Rect bbox      = get_bbox(_rendered_rect, rect);
        auto new_rects = rect_delta(_rendered_rect, bbox);

        fz_matrix transform     = fz_scale(_scaling, _scaling);
        fz_matrix inv_transform = fz_invert_matrix(transform);

        for (Rect src_rect : new_rects) {
            // The rectangle could be degenerate because something else didn't work. Just skip it if
            // so.
            if (src_rect.w() <= 0 || src_rect.h() <= 0) {
                continue;
            }

            fz_pixmap *pix = nullptr;

            fz_try(_ctx) {
                pix = fz_new_pixmap_with_bbox(_ctx, fz_device_rgb(_ctx), src_rect.as_fz_irect(),
                                              _sep, 0);
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

            { // @todo: remove this after you're done debugging
                auto [w, h] = texture_size(_texture);
                spdlog::debug("With texture of {}x{}", w, h);
            }

            fz_device *draw_dev;
            fz_try(_ctx) {
                draw_dev = fz_new_draw_device(_ctx, transform, pix);
                // Here, we use the src_rect to clip out the part of the page we want
                fz_run_display_list(_ctx, _dlist, draw_dev, fz_identity,
                                    fz_transform_rect(src_rect.as_fz_rect(), inv_transform),
                                    nullptr);
                fz_close_device(_ctx, draw_dev);
            }
            fz_always(_ctx) { fz_drop_device(_ctx, draw_dev); }
            fz_catch(_ctx) {
                fz_drop_pixmap(_ctx, pix);
                spdlog::error("Page::render: Could not draw page: {}", fz_caught_message(_ctx));
                return tl::make_unexpected(BolzanoError{ErrSource::mupdf, fz_caught_message(_ctx)});
            }

            SDL_Rect dst        = src_rect.as_sdl_rect();
            uint8_t *pix_handle = nullptr;
            int      pitch;

            spdlog::debug("Locking texture at {}", dst);

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

        _rendered_rect = bbox; // we've cleared this region

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
    fz_separations  *_sep   = nullptr;

    Rect  _rendered_rect; // a rectangle containing parts of the page we've already rendered
    float _scaling = 1.0;

    // @memory: we should really use a smart pointer here
    SDL_Texture  *_texture  = nullptr;
    SDL_Renderer *_renderer = nullptr;
};

/**
Part of a page, and its translations into screen space.
*/
struct PageRect {
    int  pnum;
    Rect src;
    Rect dst;

    PageRect bound_dst(const Rect &oth) {
        Rect new_dst = dst.bound(oth);
        Rect new_src(src.x0() + new_dst.x0() - dst.x0(),  //
                     src.y0() + new_dst.y0() - dst.y0(),  //
                     src.x1() + new_dst.x1() - dst.x1(),  //
                     src.y1() + new_dst.y1() - dst.y1()); //
        return PageRect{pnum, new_src, new_dst};
    }
};

template <> struct fmt::formatter<PageRect> {
    // Presentation format: 'd' - default.
    char presentation = 'd';

    // Parses format specifications of the form ['d'].
    constexpr auto parse(format_parse_context &ctx) -> format_parse_context::iterator {
        auto it = ctx.begin(), end = ctx.end();
        if (it != end && *it == 'd') presentation = *it++;
        if (it != end && *it != '}') ctx.on_error("invalid format");

        return it;
    }

    // Formats the PageRect object `rect` using the parsed format specification (presentation).
    auto format(const PageRect &rect, format_context &ctx) const -> format_context::iterator {
        return fmt::format_to(ctx.out(), "PageRect: pnum={}, src={}, dst={}", rect.pnum, rect.src,
                              rect.dst);
    }
};

enum class TileMode : uint8_t {
    Single,
    Dual, // even pages on left
};

/**
Maps a point in the window to a point on some page.
*/
struct PositionTracker {
    int   pnum;
    float page_xoff;
    float page_yoff;
    float scr_xoff;
    float scr_yoff;
};

template <> struct fmt::formatter<PositionTracker> {
    // Presentation format: 'd' - default.
    char presentation = 'd';

    // Parses format specifications of the form ['d'].
    constexpr auto parse(format_parse_context &ctx) -> format_parse_context::iterator {
        auto it = ctx.begin(), end = ctx.end();
        if (it != end && *it == 'd') presentation = *it++;
        if (it != end && *it != '}') ctx.on_error("invalid format");

        return it;
    }

    // Formats the PositionTracker object `pos` using the parsed format specification
    // (presentation).
    auto format(const PositionTracker &pos, format_context &ctx) const -> format_context::iterator {
        return fmt::format_to(
            ctx.out(),
            "PositionTracker: pnum={}, page_xoff={}, page_yoff={}, scr_xoff={}, scr_yoff={}",
            pos.pnum, pos.page_xoff, pos.page_yoff, pos.scr_xoff, pos.scr_yoff);
    }
};

class Document {
  public:
    Document(char *filename, float scaling = 300.0 / 72.0) {
        PerfTimer __t("Load document");
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
        _separations.resize(_nr_pages);
        _stext_pages.resize(_nr_pages);

        for (int i = 0; i < _nr_pages; i++) {
            fz_page   *page = nullptr;
            fz_device *dev  = nullptr;
            fz_try(_ctx) {
                page = fz_load_page(_ctx, _doc, i);
                assert(page != nullptr);

                _dlists[i] = fz_new_display_list_from_page(_ctx, page);
                assert(_dlists[i] != nullptr);
                _separations[i] = fz_page_separations(_ctx, page); // this can be null

                dev = fz_new_list_device(_ctx, _dlists[i]);
                fz_run_page(_ctx, page, dev, fz_identity, nullptr); // really fz_identity?
                fz_close_device(_ctx, dev);

                // @todo: we definitely shouldn't do this at startup
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
        auto [pw, ph] = page_dim(pos.pnum);

        switch (tile_mode) {
        case TileMode::Single:
            if (pos.page_yoff > ph && pos.pnum < _nr_pages - 1) { // scroll down into next page
                pos.pnum++;
                pos.page_yoff -= ph + gap;
            } else if (pos.page_yoff < 0 && pos.pnum > 0) { // scroll up into prev page
                pos.pnum--;
                pos.page_yoff = height(pos.pnum) + pos.page_yoff + gap;
            }
            break;
        case TileMode::Dual:
            // @todo: check of pos.pnum+-1 exist!
            float oth_h = pos.pnum % 2 == 0 ? height(pos.pnum + 1) : height(pos.pnum - 1);
            if (pos.page_yoff > ph) {
                if (oth_h <= ph) {
                    pos.pnum += 2; // @todo: check if this even exists
                    pos.page_yoff -= ph + gap;
                } else {
                    // nothing
                }
            } else if (pos.page_yoff < 0) {
                if (oth_h <= ph) {
                    pos.pnum -= 2; // @todo: check if this even exists
                    pos.page_yoff = height(pos.pnum) + pos.page_yoff + gap;
                } else {
                    // nothing
                }
            }
            break;
        }

        // @todo: determine new x offset
        // float new_w   = width(pos.pnum);
        // pos.page_xoff = (pos.page_xoff / pw) * new_w;
    }

    void scroll_x(PositionTracker &pos, int dx, int gap) {
        pos.page_xoff += dx;
        return;
    }

    std::deque<PageRect> tile(float winw, float winh, const PositionTracker &pos, int ygap,
                              int xgap) {
        switch (tile_mode) {
        case TileMode::Single:
            return tile1(winw, winh, pos, ygap);
        case TileMode::Dual:
            return tile2(winw, winh, pos, ygap, xgap);
        }
    }

    tl::expected<SDL_Texture *, BolzanoError> render(SDL_Renderer *renderer, const PageRect &tile) {
        // spdlog::debug("Got request to render page {} with rect({}, {}, {}, {})", tile.pnum,
        //               tile.src.x0, tile.src.y0, tile.src.x1, tile.src.y1);
        Page *p = _renders.get_or_insert(tile.pnum, [this, &tile, renderer]() {
            Page new_p(_ctx, tile.pnum, _dlists[tile.pnum], _separations[tile.pnum]);
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

    float width(int pnum) { return page_dim(pnum).first; }

    float height(int pnum) { return page_dim(pnum).second; }

    void scale_by(float factor) {
        if (factor == 1.0) {
            return;
        }
        _scaling *= factor;
        _renders.clear();
    }

    /**
    up: direction to jump in - true is upwards, false is down
    */
    void skip_one(bool up, PositionTracker &pos) {
        switch (tile_mode) {
        case TileMode::Single:
            if (up) pos.pnum = std::min(_nr_pages, pos.pnum - 1);
            else pos.pnum = std::max(0, pos.pnum + 1);
            break;
        case TileMode::Dual:
            if (up) pos.pnum = std::min(_nr_pages, pos.pnum - 2);
            else pos.pnum = std::max(0, pos.pnum + 2);
            break;
        }
    }

    /**
    @todo: centralize horizontally also.
    */
    void try_centralize(PositionTracker &pos, int winw, int winh) {
        assert(pos.pnum >= 0);
        assert(pos.pnum < _nr_pages);

        auto [pw, ph] = page_dim(pos.pnum);

        // If the height of the page fits within the window, then we'll centralize it. Otherwise,
        // flush it to the top.
        if (ph < winh) {
            pos.page_yoff = ph / 2.0;
            pos.scr_yoff  = winh / 2.0;
        } else {
            pos.page_yoff = 0;
            pos.scr_yoff  = 0;
        }
    }

    std::optional<std::tuple<int, float, float>>
    map_to_page(const PositionTracker &pos, int x, int y, const std::deque<PageRect> &tiles) {
        float dx = x - pos.scr_xoff;
        float dy = y - pos.scr_yoff;

        float px = pos.page_xoff + dx;
        float py = pos.page_yoff + dy;

        auto tile = std::find_if(tiles.begin(), tiles.end(),
                                 [&pos](const PageRect &p) { return p.pnum == pos.pnum; });
        if (tile == tiles.end()) return std::nullopt;

        // Place "px" into its correct page
        switch (tile_mode) {
        case TileMode::Single:
            if (px < 0 || px > width(tile->pnum)) return std::nullopt;
            break;
        case TileMode::Dual:
            if (px < 0) {
                if (tile->pnum % 2 == 0 || tile == tiles.begin()) return std::nullopt;
                tile--;
                px += width(tile->pnum);
                if (px < 0) return std::nullopt;
            } else if (px > width(tile->pnum)) {
                if (tile->pnum % 2 == 1 || tile == tiles.end() - 1) return std::nullopt;
                px -= width(tile->pnum);
                tile++;
                if (px > width(tile->pnum)) return std::nullopt;
            }
            break;
        }

        // Place "py" into its correct page
        switch (tile_mode) {
        case TileMode::Single:
            while (py < 0) {
                if (tile == tiles.begin()) return std::nullopt;
                tile--;
                py += height(tile->pnum);
            }
            while (py > height(tile->pnum)) {
                if (tile == tiles.end() - 1) return std::nullopt;
                py -= height(tile->pnum);
                tile++;
            }
            break;
        case TileMode::Dual:
            while (py < 0) {
                if (tile == tiles.begin() || tile == tiles.begin() + 1) return std::nullopt;
                tile -= 2;
                py += height(tile->pnum);
            }
            while (py > height(tile->pnum)) {
                if (tile == tiles.end() - 2 || tile == tiles.end() - 1) return std::nullopt;
                py -= height(tile->pnum);
                tile += 2;
            }
            break;
        }

        // If we actually reach this line, then `tile` must be pointing at the page we want.
        return {{tile->pnum, px, py}};
    }

    /**
    Finds the best fz_stext_line which contains a given point.
    */
    fz_stext_line *best_text_line(int pnum, float x, float y) {
        assert(pnum >= 0);
        assert(pnum < _nr_pages);

        x /= _scaling;
        y /= _scaling;

        fz_stext_line  *ret = nullptr;
        fz_stext_block *blo = _stext_pages[pnum]->first_block;
        fz_stext_line  *lin = nullptr;

        while (true) {
            if (is_inside(blo->bbox, x, y) && blo->type == 0) {
                lin = blo->u.t.first_line;
                while (true) {
                    if (is_inside(lin->bbox, x, y)) {
                        if (!ret) {
                            ret = lin;
                        } else {
                            int dy0 = std::abs((ret->bbox.y1 + ret->bbox.y0) / 2 - y);
                            int dy1 = std::abs((lin->bbox.y1 + lin->bbox.y0) / 2 - y);
                            if (dy0 > dy1) ret = lin;
                        }
                    }
                    if (lin == blo->u.t.last_line) break;
                    lin = lin->next;
                    continue;
                }
            }
            if (blo == _stext_pages[pnum]->last_block) break;
            blo = blo->next;
        }

        return ret;
    }

    FindRefResult find_ref(float x, fz_stext_line *line) {
        x /= _scaling;

        assert(line);
        assert(line->bbox.x0 < x);
        assert(x < line->bbox.x1);

        const static std::regex                 REFNUM_PATTERN("(\\d+(.\\d+)*)");
        const static std::array<std::string, 6> DOC_LABELS = {"theorem", "corollary",  "lemma",
                                                              "example", "definition", "exercise"};

        std::vector<int> chars;
        int              idx_ctr = -1;

        int idx = 0;

        // First navigate to the character closest to our x coord
        fz_stext_char *ch = line->first_char;
        while (ch) {
            chars.push_back(ch->c);
            if (ch->quad.ul.x <= x && x <= ch->quad.ur.x) {
                idx_ctr = idx;
                break;
            }
            ch = ch->next;
            idx++;
        }
        if (idx_ctr == -1) {
            return {"", "", FindRefStatus::Invalid};
        }

        ch = ch->next;

        // Continue searching right until the first whitespace
        while (ch && ch->c != ' ') {
            chars.push_back(ch->c);
            ch = ch->next;
        }

        // Find the first index from the left of our cursor which is whitespace
        int idx_left = idx_ctr - 1;
        while (idx_left >= 0) {
            if (chars[idx_left] == ' ') {
                idx_left++; // move one right
                break;
            }
            idx_left--;
        }
        idx_left = std::max(0, idx_left); // just in case it's negative

        std::smatch refnum_match;
        std::string refnum(chars.begin() + idx_left, chars.end());
        if (std::regex_search(refnum, refnum_match, REFNUM_PATTERN)) {
            refnum = refnum_match[1];
        } else {
            // If we can't find a reference number, then even if we find a label, there's no use.
            return {"", "", FindRefStatus::Invalid};
        }

        // Now find the word before it, which should be the label, e.g. "theorem", "example"
        int idx_label = idx_left - 2;
        if (idx_label < 0) {
            // This means that there isn't another word to the left of the refnum we found. So we
            // signal to caller to check the previous line.
            return {"", refnum, FindRefStatus::CheckPrev};
        }
        while (idx_label >= 0) {
            if (chars[idx_label] == ' ') {
                idx_label++; // move one right
                break;
            }
            idx_label--;
        }
        idx_label = std::max(0, idx_label);

        std::string label(chars.begin() + idx_label, chars.begin() + idx_left - 1);
        std::transform(label.begin(), label.end(), label.begin(),
                       [](unsigned char c) { return std::tolower(c); });

        for (const std::string &l : DOC_LABELS) {
            if (l == label) {
                return {label, refnum, FindRefStatus::Ok};
            }
        }

        return {"", refnum, FindRefStatus::RefnumOnly};
    };

  private:
    /**
    Returns a list of pages and their rectangles which should appear, given some window dimensions
    and current position. The horizontal offset of each page will be based on the position given.

    All computations are done post-scaled.
    */
    std::deque<PageRect> tile1(float winw, float winh, const PositionTracker &pos, int gap) {
        assert(pos.pnum >= 0);
        assert(pos.pnum < _nr_pages);

        std::deque<PageRect> tiles;
        Rect                 win_rect(0.0f, 0.0f, winw, winh);

        // do the current page first
        auto [pw, ph] = page_dim(pos.pnum);
        tiles.push_back(map_to_screen(pos).bound_dst(win_rect));

        // The remaining heights on screen to fill, <= for both means that we're done.
        float htop = pos.scr_yoff - pos.page_yoff - gap; // upwards
        float hbot = winh - ph - htop - gap - gap;       // downwards

        float xx = pos.page_xoff / pw;

        int at_p = pos.pnum - 1;
        while (htop > 0 && at_p >= 0) {
            auto [_pw, _ph] = page_dim(at_p);
            float page_xoff = xx * _pw;

            PositionTracker _pos{at_p, page_xoff, _ph, pos.scr_xoff, htop};
            tiles.push_front(map_to_screen(_pos).bound_dst(win_rect));

            htop -= ph;
            htop -= gap;
            at_p--;
        }

        at_p = pos.pnum + 1;
        while (hbot > 0 && at_p < _nr_pages) {
            auto [_pw, _ph] = page_dim(at_p);
            float page_xoff = xx * _pw;

            PositionTracker _pos{at_p, page_xoff, 0.0, pos.scr_xoff, winh - hbot};
            tiles.push_back(map_to_screen(_pos).bound_dst(win_rect));

            hbot -= ph;
            hbot -= gap;
            at_p++;
        }

        return tiles;
    }

    std::deque<PageRect> tile2(float winw, float winh, const PositionTracker &pos, int ygap,
                               int xgap) {
        assert(pos.pnum >= 0);
        assert(pos.pnum < _nr_pages);

        std::deque<PageRect> tiles;
        Rect                 win_rect(0.0f, 0.0f, winw, winh);

        // simple approach: draw pos.pnum first, and then check left or right
        PageRect init_tile = map_to_screen(pos);
        tiles.push_back(init_tile.bound_dst(win_rect));

        float xx     = pos.page_xoff / width(pos.pnum);
        float xx_scr = pos.scr_xoff;

        float htop = -1;
        float hbot = -1;

        float ratio_y = init_tile.src.y0() / height(pos.pnum);

        if (pos.pnum % 2 == 0) { // need to draw right
            // @todo: check if pnum + 1 exists
            float           page_yoff = ratio_y * height(pos.pnum + 1);
            PositionTracker p{pos.pnum + 1, 0, page_yoff, init_tile.dst.x1() + xgap,
                              init_tile.dst.y0()};

            PageRect tile = map_to_screen(p);
            if (init_tile.dst.x1() < winw) tiles.push_back(tile.bound_dst(win_rect));

            htop = std::min(init_tile.dst.y0(), tile.dst.y0()) - ygap;
            hbot = winh - std::max(init_tile.dst.y1(), tile.dst.y1()) - ygap;
        } else { // need to draw left
            // @todo: check if pnum - 1 exists
            float           pw        = width(pos.pnum - 1);
            float           page_yoff = ratio_y * height(pos.pnum - 1);
            PositionTracker p{pos.pnum - 1, pw, page_yoff, init_tile.dst.x0() - xgap,
                              init_tile.dst.y0()};

            PageRect tile = map_to_screen(p);

            if (init_tile.dst.x0() > 0) tiles.push_front(tile.bound_dst(win_rect));
            xx     = p.page_xoff / pw;
            xx_scr = p.scr_xoff;

            // @todo: this logic for getting the heights is duplicated!
            htop = std::min(init_tile.dst.y0(), tile.dst.y0()) - ygap;
            hbot = winh - std::max(init_tile.dst.y1(), tile.dst.y1()) - ygap;
        }

        int at_p = pos.pnum % 2 == 0 ? pos.pnum - 2 : pos.pnum - 3; // left side
        while (at_p >= 0 && htop > 0) {                             // tile upwards
            auto [lw, lh] = page_dim(at_p);
            PositionTracker le{at_p, xx * lw, lh, xx_scr, htop};
            PageRect        ltile = map_to_screen(le);

            assert(at_p + 1 < _nr_pages);

            PositionTracker ri{at_p + 1, 0, height(at_p + 1), ltile.dst.x1() + xgap, htop};
            PageRect        rtile = map_to_screen(ri);

            if (ltile.dst.x1() < winw) tiles.push_front(rtile.bound_dst(win_rect));
            if (rtile.dst.x0() > 0) tiles.push_front(ltile.bound_dst(win_rect));

            htop -= std::max(ltile.dst.h(), rtile.dst.h()) + ygap;
            at_p -= 2;
        }

        at_p = pos.pnum % 2 == 0 ? pos.pnum + 2 : pos.pnum + 1; // left side
        while (at_p < _nr_pages && hbot > 0) {                  // tile downwards
            auto [lw, lh] = page_dim(at_p);
            PositionTracker le{at_p, xx * lw, 0, xx_scr, winh - hbot};
            PageRect        ltile = map_to_screen(le);

            if (at_p + 1 >= _nr_pages) {
                break;
            }

            PositionTracker ri{at_p + 1, 0, 0, ltile.dst.x1() + xgap, winh - hbot};
            PageRect        rtile = map_to_screen(ri);

            if (rtile.dst.x0() > 0) tiles.push_back(ltile.bound_dst(win_rect));
            if (ltile.dst.x1() < winw) tiles.push_back(rtile.bound_dst(win_rect));

            hbot -= std::max(ltile.dst.h(), rtile.dst.h()) + ygap;
            at_p += 2;
        }

        return tiles;
    }

    /**
    Given some position tells you how to draw that page on the screen. The resultant rectangles may
    overflow the window.

    winw: (Virtual) window width.

    winh: (Virtual) window height.

    pos: Some page-point to screen-point mapping.
    */
    PageRect map_to_screen(const PositionTracker &pos) {
        assert(pos.pnum >= 0);
        assert(pos.pnum < _nr_pages);

        auto [pw, ph] = page_dim(pos.pnum);

        Rect src(0.0f, 0.0f, pw, ph);
        Rect dst(pos.scr_xoff - pos.page_xoff, pos.scr_yoff - pos.page_yoff,
                 pos.scr_xoff + (pw - pos.page_xoff), pos.scr_yoff + (ph - pos.page_yoff));

        return {pos.pnum, src, dst};
    }

  public:
    TileMode tile_mode = TileMode::Single;

  private:
    fz_context  *_ctx = nullptr;
    fz_document *_doc = nullptr;

    std::vector<fz_display_list *> _dlists;
    std::vector<fz_separations *>  _separations;
    std::vector<fz_stext_page *>   _stext_pages;
    LruKv<int, Page>               _renders;

    int   _nr_pages;
    float _scaling;
};
