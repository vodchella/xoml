# XOml GTP protocol

**GTP** is a text-based protocol for communicating with the `XOml` engine in a request-response manner. It is inspired by the [Go Text Protocol](https://en.wikipedia.org/wiki/Go_Text_Protocol).

Start `xoml` with the `-GTP` flag to enable communication over this protocol. Once enabled, the engine accepts commands as plain text through `stdin` and writes responses to `stdout`.

The end of an input command is determined by the newline character (`\n`). The end of an engine response is indicated by two consecutive newline characters (`\n\n`). Responses can be either successful or error responses. A successful response begins with `= `, while an error response begins with `? `.

Supported commands:

* **NAME**
  - Returns the name of the engine.
  - Example request: `NAME\n`
  - Example response: `= XOml\n\n`

* **VERSION**
  - Returns the engine version.
  - Example request: `VERSION\n`
  - Example response: `= 0.3.0\n\n`

* **PROTOCOL_VERSION**
  - Returns the protocol version.
  - Example request: `PROTOCOL_VERSION\n`
  - Example response: `= 1\n\n`

* **CLEAN_BOARD**
  - Clears the board and starts a new game.
  - Example request: `CLEAN_BOARD\n`
  - Example response: `= CLEAN_BOARD\n\n`

* **SHOW_BOARD**
  - Displays the current board state in a human-readable format.
  - Example request: `SHOW_BOARD\n`
  - The response format is implementation-specific and should not be relied upon.

* **WINNER**
  - Returns the current winner.
  - Example request: `WINNER\n`
  - Example responses:
    * `= ?\n\n` if the game has not finished yet.
    * `= <P>\n\n` if the game has finished. `<P>` can be either `X` or `O` and denotes the winning player.
    * `= draw\n\n` if the game ended in a draw.

* **PLAY**
    - Plays a move by placing a piece on the board.
    - Example request: `PLAY <P> <CELL>\n`, where `<P>` denotes the player (`X` or `O`) and `<CELL>` denotes a board cell (`D5`, `C3`, etc.).
    - Example success response: `= PLAY\n\n`
    - Example error response: `? Some error message\n\n`

* **GEN_MOVE**
    - Computes the best move for the specified player and places the piece on the board.
    - Example request: `GEN_MOVE <P>\n`, where `<P>` denotes the player.
    - Example success response: `= <CELL>\n\n`, where `<CELL>` is the selected move.
    - Example error response: `? draw\n\n` if the game has already ended in a draw.

* **BOARD_SIZE**
    - Sets a new board size and starts a new game.
    - Example request: `BOARD_SIZE <N>\n`, where `<N>` is an integer from 5 to 10 specifying the number of cells on each side of the board. The board is always square.
    - Example response: `= BOARD_SIZE\n\n`

* **QUIT**
    - Terminates the engine.
    - Example request: `QUIT\n`
    - No response is returned.

