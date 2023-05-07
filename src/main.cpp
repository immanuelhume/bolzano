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

    window = SDL_CreateWindow("Bolzano", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, 400, 800,
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
                if (INPUT("j")) {
                    doc.scroll_y(pos, SCROLL_Y, VERTICAL_GAP);
                } else if (INPUT("k")) {
                    doc.scroll_y(pos, -SCROLL_Y, VERTICAL_GAP);
                } else if (INPUT("l")) {
                    doc.scroll_x(pos, SCROLL_X, HORIZONTAL_GAP);
                } else if (INPUT("h")) {
                    doc.scroll_x(pos, -SCROLL_X, HORIZONTAL_GAP);
                } else if (INPUT("J")) {
                    if (pos.pnum < doc.count_pages() - 1) {
                        pos.pnum++;
                        doc.try_centralize(pos, winw, winh);
                    }
                } else if (INPUT("K")) {
                    if (pos.pnum > 0) {
                        pos.pnum--;
                        doc.try_centralize(pos, winw, winh);
                    }
                } else if (INPUT("+")) {
                    doc.scale_by(ZOOM_IN_FACTOR);
                } else if (INPUT("-")) {
                    doc.scale_by(ZOOM_OUT_FACTOR);
                } else if (INPUT("q")) {
                    goto quit;
                }
            }
        }

        SDL_SetRenderDrawColor(renderer, 0, 0, 0, 1);
        SDL_RenderClear(renderer);

        // spdlog::debug("Current position: page {} ({},{})", pos.pnum, pos.page_xoff,
        // pos.page_yoff);

        auto tiles = doc.tile1(winw, winh, pos, VERTICAL_GAP);
        for (const PageRect &tile : tiles) {
            SDL_Rect     src_rect = sdl_from_fz_rect(tile.src);
            SDL_Texture *tex      = doc.render(renderer, tile).value();
            if (SDL_RenderCopy(renderer, tex, &src_rect, &tile.dst) < 0) {
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