#include <SDL2/SDL_keycode.h>
#include <SDL2/SDL_pixels.h>
#define DOCTEST_CONFIG_DISABLE

#include "firamono_regular.hpp"
#include "main.hpp"
#include "spdlog/cfg/env.h"
#include <filesystem>
#include <iostream>
#include <unistd.h>

const int VERTICAL_GAP   = 1;
const int HORIZONTAL_GAP = 1;
const int SCROLL_Y       = 48;
const int SCROLL_X       = 48;

const int UI_FONT_SIZE = 20;

const float ZOOM_IN_FACTOR  = 1.1;
const float ZOOM_OUT_FACTOR = 0.9090909090909091;

int main(int argc, char **argv) {
    spdlog::cfg::load_env_levels();

    int   init_pnum = 0;
    char *filename  = nullptr;

    int c;
    while ((c = getopt(argc, argv, "f:p:")) != -1) {
        switch (c) {
        case 'f':
            filename = optarg;
            break;
        case 'p':
            init_pnum = std::stoi(optarg);
            break;
        default:
            std::cerr << "Bad argument format" << std::endl;
            return EXIT_FAILURE;
        }
    }

    if (!filename) {
        std::cerr << "Path to PDF file is required\n";
        return EXIT_FAILURE;
    }

    assert(filename != nullptr);

    spdlog::debug("Preparing to open document at {}", filename);

    Document doc(filename);

    SDL_Window   *window   = nullptr;
    SDL_Renderer *renderer = nullptr;

    if (SDL_Init(SDL_INIT_VIDEO) < 0) {
        spdlog::error("Could not initialize SDL: {}", SDL_GetError());
        return EXIT_FAILURE;
    }

    window = SDL_CreateWindow("Bolzano", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, 1920, 1080,
                              SDL_WINDOW_SHOWN | SDL_WINDOW_RESIZABLE);
    if (!window) {
        spdlog::error("Could not create SDL window: {}", SDL_GetError());
        return EXIT_FAILURE;
    }
    renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_PRESENTVSYNC);
    if (!renderer) {
        spdlog::error("Could not create SDL renderer: {}", SDL_GetError());
        return EXIT_FAILURE;
    }

    if (SDL_SetRenderDrawBlendMode(renderer, SDL_BLENDMODE_BLEND)) {
        spdlog::error("Could not set blend mode for renderer: {}", SDL_GetError());
        return EXIT_FAILURE;
    }

    if (TTF_Init() < 0) {
        spdlog::error("Could not initialize TTF: {}", SDL_GetError());
        return EXIT_FAILURE;
    }

    SDL_RWops *font_mem = SDL_RWFromConstMem(FiraMono_Regular_ttf, FiraMono_Regular_ttf_len);
    if (!font_mem) {
        spdlog::error("Could not load font: {}", SDL_GetError());
        return EXIT_FAILURE;
    }
    TTF_Font *font = TTF_OpenFontRW(font_mem, 1, UI_FONT_SIZE);
    if (!font) {
        spdlog::error("Could not create font after loading it: {}", SDL_GetError());
        return EXIT_FAILURE;
    }

    TextBar           text_bar(font);
    const std::string filename_full    = std::filesystem::canonical(filename).string();
    const std::string page_num_display = fmt::format("[{}/{}]", init_pnum, doc.count_pages());
    text_bar.set_left(filename_full);
    text_bar.set_right(page_num_display);

    PositionTracker      pos{init_pnum, 0, 0, 0, 0};
    std::deque<PageRect> tiles;

    int  cur_ref_winw = -1;
    auto ref_viewport = SDL_Rect{};
    auto ref_pos      = PositionTracker{};

    bool should_render_refs = false;

    int ok = 0; // just a store for error codes

    bool        is_panning   = false;
    bool        is_searching = false;
    std::string query_str    = "";

    auto last_search_results      = std::vector<std::vector<fz_quad>>(doc.count_pages());
    auto last_selected_search     = std::pair<int, int>(-1, -1); // page, index
    bool are_searches_highlighted = false;

    SDL_Event e;
    while (true) {
        int winw, winh;
        SDL_GetWindowSize(window, &winw, &winh);

        int      mousex, mousey;
        uint32_t mouse_state = SDL_GetMouseState(&mousex, &mousey);

        bool is_ref_viewport_focused = should_render_refs && is_inside(ref_viewport, mousex, mousey);
        std::optional<SDL_MouseButtonEvent> recalculate_ref = std::nullopt;

        if (is_ref_viewport_focused) {
            if (SDL_BUTTON(1) & mouse_state) {
                if (!is_panning) {
                    is_panning = true;
                    SDL_GetRelativeMouseState(nullptr, nullptr);
                } else {
                    int dx, dy;
                    SDL_GetRelativeMouseState(&dx, &dy);
                    ref_pos.scr_xoff += dx;
                    ref_pos.scr_yoff += dy;
                    ref_viewport.x += dx;
                    ref_viewport.y += dy;
                }
            } else {
                if (is_panning) is_panning = false;
            }
        }

        while (SDL_PollEvent(&e)) {
            if (e.type == SDL_QUIT) {
                goto quit;
            } else if (e.type == SDL_TEXTINPUT) {
                if (is_searching) {
                    query_str.append(e.text.text);
                } else {
#define INPUT(str) strcmp(e.text.text, str) == 0
                    if (INPUT("j")) { // scroll down
                        if (!is_ref_viewport_focused) doc.scroll_y(pos, SCROLL_Y, VERTICAL_GAP);
                        else doc.scroll_y_tile_single(ref_pos, SCROLL_Y, VERTICAL_GAP);

                    } else if (INPUT("k")) { // scroll up
                        if (!is_ref_viewport_focused) doc.scroll_y(pos, -SCROLL_Y, VERTICAL_GAP);
                        else doc.scroll_y_tile_single(ref_pos, -SCROLL_Y, VERTICAL_GAP);

                    } else if (INPUT("l")) { // scroll right
                        if (!is_ref_viewport_focused) doc.scroll_x(pos, SCROLL_X, HORIZONTAL_GAP);
                        else doc.scroll_x(ref_pos, SCROLL_X, HORIZONTAL_GAP);

                    } else if (INPUT("h")) { // scroll left
                        if (!is_ref_viewport_focused) doc.scroll_x(pos, -SCROLL_X, HORIZONTAL_GAP);
                        else doc.scroll_x(ref_pos, -SCROLL_X, HORIZONTAL_GAP);

                    } else if (INPUT("J")) { // goto next page
                        doc.skip_one(false, pos);
                        doc.try_centralize_y(pos, winh);

                        spdlog::debug("{}", pos);
                    } else if (INPUT("K")) { // goto previous page
                        doc.skip_one(true, pos);
                        doc.try_centralize_y(pos, winh);
                        spdlog::debug("{}", pos);

                    } else if (INPUT("G")) { // @todo: goto bottom

                    } else if (INPUT("+")) { // zoom in
                        doc.scale_by(ZOOM_IN_FACTOR);
                        auto scale_mat = fz_scale(ZOOM_IN_FACTOR, ZOOM_IN_FACTOR);
                        for (auto &quads : last_search_results) {
                            for (auto &quad : quads) {
                                quad = fz_transform_quad(quad, scale_mat);
                            }
                        }

                    } else if (INPUT("-")) { // zoom out
                        doc.scale_by(ZOOM_OUT_FACTOR);
                        auto scale_mat = fz_scale(ZOOM_OUT_FACTOR, ZOOM_OUT_FACTOR);
                        for (auto &quads : last_search_results) {
                            for (auto &quad : quads) {
                                quad = fz_transform_quad(quad, scale_mat);
                            }
                        }
                    } else if (INPUT("=")) {
                        doc.centralize_x(pos, winw);

                    } else if (INPUT("q")) { // exit
                        goto quit;

                    } else if (INPUT("d")) { // toggle tiling mode
                        switch (doc.tile_mode) {
                        case TileMode::Single:
                            doc.tile_mode = TileMode::Dual;
                            break;
                        case TileMode::Dual:
                            doc.tile_mode = TileMode::Single;
                            break;
                        }
                    } else if (INPUT("/")) { // start search
                        is_searching = true;
                    } else if (INPUT("n")) {
                        if (are_searches_highlighted) {
                            // we want to jump to the next search
                            auto [last_pnum, last_idx] = last_selected_search;
                            // check the current page first, then check the remaining pages, else pick the first result
                            int selected_pnum = -1;
                            int selected_idx  = -1;
                            if (last_idx + 1 < (int)last_search_results[last_pnum].size()) {
                                selected_pnum = last_pnum;
                                selected_idx  = last_idx + 1;
                            } else {
                                for (int pnum = last_pnum + 1; pnum < doc.count_pages(); pnum++) {
                                    if (!last_search_results[pnum].empty()) {
                                        selected_pnum = pnum;
                                        selected_idx  = 0;
                                        break;
                                    }
                                }
                                if (selected_pnum == -1) {
                                    for (int pnum = 0; pnum < doc.count_pages(); pnum++) {
                                        if (!last_search_results[pnum].empty()) {
                                            selected_pnum = pnum;
                                            selected_idx  = 0;
                                        }
                                    }
                                }
                            }

                            assert(selected_pnum != -1);
                            assert(selected_idx != -1);

                            last_selected_search = {selected_pnum, selected_idx};

                            spdlog::debug("selected page {} index {}", selected_pnum, selected_idx);

                            auto quad = last_search_results[selected_pnum][selected_idx];
                            pos       = doc.ensure_visible(pos, selected_pnum, quad.ul.x, quad.ul.y, winw, winh);
                        }
                    }
                }
            } else if (e.type == SDL_MOUSEBUTTONDOWN) {
                if (e.button.button == SDL_BUTTON_RIGHT) {
                    spdlog::debug("RMB at ({}, {})", e.button.x, e.button.y);
                    recalculate_ref = e.button;
                }

            } else if (e.type == SDL_KEYDOWN) {
                if (e.key.keysym.sym == SDLK_ESCAPE) {
                    if (should_render_refs) should_render_refs = false;
                    if (is_searching) {
                        query_str    = "";
                        is_searching = false;
                    }
                } else if (e.key.keysym.sym == SDLK_RETURN) {
                    if (is_searching) {
                        for (auto &res : last_search_results) {
                            res.clear();
                        }
                        last_selected_search = {-1, -1};

                        auto search_results = doc.search(query_str);

                        // The behaviour in most PDF viewers seem to be: to scroll to the first occuring result from the
                        // top of the window? We will approach this naively for now - determine the page and offset
                        // which corresponds to the top of the window, and then iterate through search results to
                        // determine the one to show.

                        spdlog::debug("Got {} results for {}", search_results.size(), query_str);

                        if (!search_results.empty()) {
                            auto top_of_screen     = doc.map_to_page(pos, pos.scr_xoff, 0, tiles);
                            auto [pnum_top, _, py] = top_of_screen.value();

                            spdlog::debug("At page {}, top of screen corresponds to y offset of {}", pnum_top, py);

                            auto idx           = 0ul;
                            auto canTake       = false;
                            auto selected_pnum = -1;
                            auto selected_quad = fz_quad{};
                            auto selected_idx  = -1;
                            while (true) {
                                auto [pnum, quad] = search_results[idx];
                                canTake           = canTake || (pnum == pnum_top && quad.ul.y > py) || pnum > pnum_top;
                                if (canTake) {
                                    selected_pnum = pnum;
                                    selected_quad = quad;
                                    selected_idx  = idx;
                                    break;
                                }
                                idx++;
                                if (idx >= search_results.size()) {
                                    canTake = true;
                                    idx %= search_results.size();
                                }
                            }
                            assert(selected_idx != -1);

                            spdlog::debug("Nearest highlight is on {}, quad {}", selected_pnum, selected_quad);

                            // Now that we have selected a page and a quad, let's jump to that place!
                            pos = doc.ensure_visible(pos, selected_pnum, selected_quad.ul.x, selected_quad.ul.y, winw,
                                                     winh);

                            spdlog::debug("Will be moving to {}", pos);

                            for (const auto &res : search_results) {
                                auto [pnum, quad] = res;
                                if (pnum == selected_pnum && quad == selected_quad) {
                                    last_selected_search = {pnum, last_search_results[pnum].size()};
                                }
                                last_search_results[pnum].push_back(quad);
                            }

                            are_searches_highlighted = true;
                        }

                        // @todo: do we want to keep the query string?
                        query_str    = "";
                        is_searching = false;
                    }
                } else if (e.key.keysym.sym == SDLK_BACKSPACE) {
                    if (is_searching && !query_str.empty()) {
                        query_str.pop_back();
                    }
                }
            }
        }

        SDL_SetRenderDrawColor(renderer, 0, 0, 0, SDL_ALPHA_OPAQUE);
        SDL_RenderClear(renderer);

        // START DRAWING SHIT
        //
        // Now we finally start to draw stuff. We don't use the concept of pages when drawing. Instead, we just care
        // about "tiles", and query the document for a bunch of tiles to draw given the current position, dimensions,
        // etc. Each tile can be rendered to a texture and then copied to our SDL renderer.

        tiles = doc.tile(winw, winh, pos, VERTICAL_GAP, HORIZONTAL_GAP);
        for (const PageRect &tile : tiles) {
            SDL_Texture *tex = doc.render(renderer, tile).value();
            SDL_Rect     src = tile.src.as_sdl_rect();
            SDL_Rect     dst = tile.dst.as_sdl_rect();
            if (SDL_RenderCopy(renderer, tex, &src, &dst) < 0) {
                spdlog::error("Could not render tile: {}", SDL_GetError());
                continue;
            }
        }

        // RENDER SEARCH HIGHLIGHT BOXES

        SDL_SetRenderDrawColor(renderer, 255, 255, 0, 64);

        auto [cur_pnum, cur_idx] = last_selected_search;
        if (are_searches_highlighted) {
            for (const auto &tile : tiles) {
                const auto &quads = last_search_results[tile.pnum];
                for (int i = 0; i < (int)quads.size(); i++) {
                    auto quad = quads[i];

                    if (!doc.can_see_quad(pos, tile.pnum, quad, winw, winh)) {
                        continue;
                    }

                    auto highlight_rect = doc.map_to_screen(pos, tile.pnum, quad).as_sdl_rect();

                    bool is_selected = tile.pnum == cur_pnum && i == cur_idx;
                    if (is_selected) {
                        SDL_SetRenderDrawColor(renderer, 64, 255, 180, 64);
                    }

                    ok = SDL_RenderFillRect(renderer, &highlight_rect);
                    if (ok < 0) {
                        spdlog::error("Could not draw rectangle for highlighting: {}", SDL_GetError());
                        continue;
                    }

                    SDL_SetRenderDrawColor(renderer, 128, 128, 128, 128);
                    ok = SDL_RenderDrawRect(renderer, &highlight_rect);
                    if (ok < 0) {
                        spdlog::error("Could not draw rectangle border for highlighting: {}", SDL_GetError());
                        continue;
                    }

                    SDL_SetRenderDrawColor(renderer, 255, 255, 0, 64);
                }
            }
        }

        SDL_SetRenderDrawColor(renderer, 0, 0, 0, SDL_ALPHA_OPAQUE);

        // HANDLE VIRTUAL VIEWPORT FOR REFERENCES

        if (recalculate_ref.has_value()) {
            int  x = recalculate_ref.value().x, y = recalculate_ref.value().y;
            auto ref = doc.reference_at(x, y, pos, tiles);
            if (ref.has_value()) {
                auto [ref_pnum, ref_x, ref_y] = ref.value();
                ref_pos      = {ref_pnum, doc.scaling() * ref_x, doc.scaling() * ref_y, static_cast<float>(winw) / 2,
                                static_cast<float>(winh) / 2};
                cur_ref_winw = doc.width(ref_pnum);

                // the reference viewport is always single tiled
                doc.centralize_x_with_tile_mode(ref_pos, winw, TileMode::Single);

                spdlog::debug("Referenced position: page {} ({}, {})", std::get<0>(ref.value()),
                              std::get<1>(ref.value()), std::get<2>(ref.value()));
                should_render_refs = true;

                // Initialize the reference viewport
                ref_viewport = {winw / 2 - cur_ref_winw / 2, winh / 2 - 512 / 2, cur_ref_winw, 512};
            } else {
                spdlog::error("Could not find a valid reference");
                should_render_refs = false;
            }
        }

        if (should_render_refs) {
            auto ref_tiles = doc.tile1(winw, winh, ref_pos, VERTICAL_GAP);

            ok = SDL_RenderSetClipRect(renderer, &ref_viewport);
            if (ok < 0) spdlog::error("Could not set clip rectangle to draw reference viewport: {}", SDL_GetError());

            SDL_SetRenderDrawColor(renderer, 0, 0, 0, SDL_ALPHA_OPAQUE);
            ok = SDL_RenderFillRect(renderer, &ref_viewport);
            if (ok < 0) spdlog::error("Could not clear viewport: {}", SDL_GetError());

            for (const auto &tile : ref_tiles) {
                SDL_Texture *tex = doc.render(renderer, tile).value();
                SDL_Rect     src = tile.src.as_sdl_rect();
                SDL_Rect     dst = tile.dst.as_sdl_rect();
                if (SDL_RenderCopy(renderer, tex, &src, &dst) < 0) {
                    spdlog::error("Could not render tile: {}", SDL_GetError());
                    continue;
                }
            }

            ok = SDL_RenderSetClipRect(renderer, nullptr);
            if (ok < 0)
                spdlog::error("Could not unset clip rectangle after drawing reference viewport: {}", SDL_GetError());

            ok = SDL_RenderDrawRect(renderer, &ref_viewport);
            if (ok < 0) spdlog::error("Could not draw rectangle around viewport: {}", SDL_GetError());
        }

        // HANDLE TEXT BAR

        text_bar.set_width(winw);
        // @speed: no need to format this on every loop
        text_bar.set_right(fmt::format("[{}/{}]", pos.pnum, doc.count_pages()));
        if (is_searching) {
            text_bar.set_left("search:" + query_str);
        } else {
            text_bar.set_left(filename_full);
        }
        auto _bar = text_bar.get_texture(renderer);
        if (!_bar.has_value()) {
            spdlog::error(_bar.error().desc);
            exit(EXIT_FAILURE);
        }
        SDL_Texture *bar_tex = _bar.value();
        assert(bar_tex != nullptr);

        int bar_w, bar_h;
        ok = SDL_QueryTexture(bar_tex, nullptr, nullptr, &bar_w, &bar_h);
        if (ok < 0) spdlog::error("Could not query texture for text bar", SDL_GetError());

        SDL_Rect bar_src{0, 0, bar_w, bar_h};
        SDL_Rect bar_dst{0, winh - bar_h, bar_w, bar_h};
        SDL_RenderCopy(renderer, bar_tex, &bar_src, &bar_dst);
        if (ok < 0) spdlog::error("Could not render text bar: {}", SDL_GetError());

        // END OF LOOP LOGIC

        SDL_RenderPresent(renderer);
    }

quit:
    SDL_DestroyWindow(window);
    SDL_Quit();

    return EXIT_SUCCESS;
}
