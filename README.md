# XOml

[![OCaml](https://img.shields.io/badge/OCaml-%23EC6813.svg?logo=ocaml&logoColor=white)](https://ocaml.org/) [![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT) [![Nix flakes](https://img.shields.io/badge/Flakes-enabled-blue?logo=nixos&logoColor=white)](https://wiki.nixos.org/wiki/Flakes)

**XOml** is a very simple (easy-to-win) **Tic-Tac-Toe** game — _a 5-in-a-row variant on a 10×10 board_ — for fans of the terminal and Linux, implemented in [OCaml](https://ocaml.org/).

## How to play

- Unpack the archive downloaded from the [Releases](https://github.com/vodchella/xoml/releases) section (or build it yourself from the source), and run `./xoml` in the terminal. The game supports several optional command-line parameters:
  - `-s X` or `-s=X` is used to specify the board side length. `X` here is an integer from 5 to 10. For example, with `-s=7` the game will start with a 7x7 board. By default, the board size is 10×10.
  - `-O` is used to make the computer perform the first move; by default, the human goes first. The computer’s moves are marked with the symbol `O`, hence the name of the flag.
- To make a move, you need to enter the coordinates of a free cell and press Enter. For example, `e5` or `f4`. The case of all commands does not matter.
- To display help, enter `h` or `help`.
- To exit the program, enter `q`, `e`, `x`, `quit`, or `exit`.

## How to build

- [Install](https://nix.dev/install-nix.html) [Nix](https://github.com/NixOS/nix) with `curl -L https://nixos.org/nix/install | sh -s -- --daemon`
- Enter to the development shell with `nix develop`. You may need to provide additional `--extra-experimental-features nix-command --extra-experimental-features flakes` flags for this command.
- Build it with `dune`:
  - `dune build` to build.
  - `dune exec xoml` to build and run.
  - `dune clean` to clean up build artifacts.
- Or with `make`:
  - `make` to build.
  - `make run` to build and run.
  - `make clean` to clean up build artifacts.
