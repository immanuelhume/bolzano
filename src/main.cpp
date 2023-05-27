#include <SDL2/SDL_render.h>
#include <SDL2/SDL_surface.h>
#define DOCTEST_CONFIG_DISABLE

#include "main.hpp"
#include "spdlog/cfg/env.h"
#include <iostream>
#include <unistd.h>

const int VERTICAL_GAP   = 1;
const int HORIZONTAL_GAP = 1;
const int SCROLL_Y       = 48;
const int SCROLL_X       = 48;

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

    PositionTracker      pos{init_pnum, 0, 0, 0, 0};
    std::deque<PageRect> tiles;

    int             cur_ref_winw = -1;
    SDL_Rect        ref_viewport;
    PositionTracker ref_pos;

    bool should_render_refs = false;

    int ok = 0; // just a store for error codes

    SDL_Event e;
    while (true) {
        int winw, winh;
        SDL_GetWindowSize(window, &winw, &winh);

        int mousex, mousey;
        SDL_GetMouseState(&mousex, &mousey);

        bool is_ref_viewport_focused = should_render_refs && is_inside(ref_viewport, mousex, mousey);

        std::optional<SDL_MouseButtonEvent> recalculate_ref = std::nullopt;

        while (SDL_PollEvent(&e)) {
            if (e.type == SDL_QUIT) {
                goto quit;
            } else if (e.type == SDL_TEXTINPUT) {
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
                    doc.try_centralize(pos, winw, winh);

                    spdlog::debug("{}", pos);
                } else if (INPUT("K")) { // goto previous page
                    doc.skip_one(true, pos);
                    doc.try_centralize(pos, winw, winh);
                    spdlog::debug("{}", pos);

                } else if (INPUT("G")) { // @todo: goto bottom

                } else if (INPUT("+")) { // zoom in
                    doc.scale_by(ZOOM_IN_FACTOR);

                } else if (INPUT("-")) { // zoom out
                    doc.scale_by(ZOOM_OUT_FACTOR);

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
                }

            } else if (e.type == SDL_MOUSEBUTTONDOWN) {
                if (e.button.button == SDL_BUTTON_RIGHT) {
                    spdlog::debug("Mouse click at ({}, {})", e.button.x, e.button.y);
                    recalculate_ref = e.button;
                }

            } else if (e.type == SDL_KEYDOWN) {
                if (e.key.keysym.sym == SDLK_ESCAPE) {
                    if (should_render_refs) should_render_refs = false;
                }
            }
        }

        SDL_SetRenderDrawColor(renderer, 0, 0, 0, 1);
        SDL_RenderClear(renderer);

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

        if (recalculate_ref.has_value()) {
            int  x = recalculate_ref.value().x, y = recalculate_ref.value().y;
            auto ref = doc.reference_at(x, y, pos, tiles);
            if (ref.has_value()) {
                auto [ref_pnum, ref_x, ref_y] = ref.value();
                ref_pos = {ref_pnum, doc.scaling() * ref_x, doc.scaling() * ref_y, static_cast<float>(winw) / 2,
                           static_cast<float>(winh) / 2};
                // cur_ref      = ref_pos_new;
                cur_ref_winw = doc.width(ref_pnum);

                spdlog::debug("Referenced position: page {} ({}, {})", std::get<0>(ref.value()),
                              std::get<1>(ref.value()), std::get<2>(ref.value()));
                should_render_refs = true;
            } else {
                spdlog::error("Could not find a valid reference");
                should_render_refs = false;
            }
        }

        if (should_render_refs) {
            int ref_winw = cur_ref_winw;
            int ref_winh = 512; // @todo: make this constant or configurable

            // refresh_ref_pos();
            auto ref_tiles = doc.tile1(winw, winh, ref_pos, VERTICAL_GAP);

            ref_viewport = {winw / 2 - ref_winw / 2, winh / 2 - ref_winh / 2, ref_winw, ref_winh};

            ok = SDL_RenderSetClipRect(renderer, &ref_viewport);
            if (ok < 0) spdlog::error("Could not set clip rectangle to draw reference viewport: {}", SDL_GetError());

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

        SDL_RenderPresent(renderer);
    }

quit:
    SDL_DestroyWindow(window);
    SDL_Quit();

    return EXIT_SUCCESS;
}