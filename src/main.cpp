#define DOCTEST_CONFIG_DISABLE

#include "main.hpp"
#include "fmt/core.h"
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

    PositionTracker pos{init_pnum, 0, 0, 0, 0};

    SDL_Event e;
    while (true) {
        int winw, winh;
        SDL_GetWindowSize(window, &winw, &winh);

        while (SDL_PollEvent(&e)) {
            if (e.type == SDL_QUIT) {
                goto quit;
            } else if (e.type == SDL_TEXTINPUT) {
#define INPUT(str) strcmp(e.text.text, str) == 0
                if (INPUT("j")) { // scroll down
                    spdlog::debug("{}", pos);
                    doc.scroll_y(pos, SCROLL_Y, VERTICAL_GAP);
                } else if (INPUT("k")) { // scroll up
                    spdlog::debug("{}", pos);
                    doc.scroll_y(pos, -SCROLL_Y, VERTICAL_GAP);
                } else if (INPUT("l")) { // scroll right
                    doc.scroll_x(pos, SCROLL_X, HORIZONTAL_GAP);
                } else if (INPUT("h")) { // scroll left
                    doc.scroll_x(pos, -SCROLL_X, HORIZONTAL_GAP);
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
            }
        }

        SDL_SetRenderDrawColor(renderer, 0, 0, 0, 1);
        SDL_RenderClear(renderer);

        std::vector<PageRect> tiles = doc.tile(winw, winh, pos, VERTICAL_GAP, HORIZONTAL_GAP);
        for (const PageRect &tile : tiles) {
            SDL_Texture *tex = doc.render(renderer, tile).value();
            SDL_Rect     src = tile.src.as_sdl_rect();
            SDL_Rect     dst = tile.dst.as_sdl_rect();
            if (SDL_RenderCopy(renderer, tex, &src, &dst) < 0) {
                spdlog::error("Could not render tile: {}", SDL_GetError());
                continue;
            }
        }

        SDL_RenderPresent(renderer);
    }

quit:
    SDL_DestroyWindow(window);
    SDL_Quit();

    return EXIT_SUCCESS;
}