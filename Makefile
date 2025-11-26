OCAMLC      = ocamlopt
OCAMLFIND   = ocamlfind
PKGS        = unix
FLAGS       = -g -w +a-40-42-70
BUILD_DIR   = build

SOURCES     = src/common.ml src/input.ml src/board.ml src/engine.ml src/xo.ml
CMX         = $(patsubst src/%.ml,$(BUILD_DIR)/%.cmx,$(SOURCES))

all: $(BUILD_DIR)/xoml

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

$(BUILD_DIR)/%.cmx: src/%.ml | $(BUILD_DIR)
	$(OCAMLFIND) $(OCAMLC) $(FLAGS) -package $(PKGS) -I $(BUILD_DIR) -c $< -o $@

$(BUILD_DIR)/xoml: $(CMX)
	$(OCAMLFIND) $(OCAMLC) $(FLAGS) -linkpkg -package $(PKGS) \
	  -I $(BUILD_DIR) $(CMX) -o $(BUILD_DIR)/xoml

clean:
	rm -rf $(BUILD_DIR)

.PHONY: all clean
