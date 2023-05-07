CXX       := clang++
SRC_DIR   := src
OUT_DIR   := build
DEBUG_BIN := bolzano_debug
TEST_BIN  := bolzano_test

BUILD_CMD := 

debug:
	@echo "Making debug build..."
	@mkdir -p $(OUT_DIR)
	@cat compile_flags.txt | xargs $(CXX) $(SRC_DIR)/main.cpp -o $(OUT_DIR)/$(DEBUG_BIN)
	@echo "Build completed at $(OUT_DIR)/$(DEBUG_BIN)."

.PHONY: debug

file ?= analysis.pdf
page ?= 83

run-debug:
	@echo "Running..."
	@ASAN_OPTIONS=detect_leaks=1 SPDLOG_LEVEL=debug ./$(OUT_DIR)/$(DEBUG_BIN) -f $(file) -p $(page)
	@echo "Run completed!"

.PHONY: run-debug

test:
	@echo "Building test binary..."
	@mkdir -p $(OUT_DIR)
	@cat compile_flags.txt | xargs $(CXX) $(SRC_DIR)/test.cpp -o $(OUT_DIR)/$(TEST_BIN)
	@echo "Build completed at $(OUT_DIR)/$(TEST_BIN)."
	@./$(OUT_DIR)/$(TEST_BIN)

.PHONY: test
