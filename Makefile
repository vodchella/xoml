OCAMLC      = ocamlopt
OCAMLFIND   = ocamlfind
PKGS        = unix
FLAGS       = -g -w +a-40-42-70
BUILD_DIR   = build
TARGET      = $(BUILD_DIR)/xoml

SOURCES     = src/logger.ml    \
			  src/common.ml    \
			  src/benchmark.ml \
			  src/input.ml     \
			  src/board.ml     \
			  src/engine.ml    \
			  src/xo.ml
CMX         = $(patsubst src/%.ml,$(BUILD_DIR)/%.cmx,$(SOURCES))

all: $(TARGET)

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

# $< : Путь к исходному файлу. Здесь .ml
# $@ : Имя цели. Здесь .cmx
$(BUILD_DIR)/%.cmx: src/%.ml | $(BUILD_DIR)
	$(OCAMLFIND) $(OCAMLC) $(FLAGS) -package $(PKGS) -I $(BUILD_DIR) -c $< -o $@

$(TARGET): $(CMX)
	$(OCAMLFIND) $(OCAMLC) $(FLAGS) -linkpkg -package $(PKGS) \
	  -I $(BUILD_DIR) $(CMX) -o $(TARGET)

run: $(TARGET)
	$(TARGET)

clean:
	rm -rf $(BUILD_DIR)

.PHONY: all clean run
