{
  description = "OCaml dev shell";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }: {
    devShell = {
      default = let
        pkgs = import nixpkgs { system = builtins.currentSystem; };
      in
        pkgs.mkShell {
          buildInputs = with pkgs; [
            ocaml
            dune
            ocamlPackages.findlib
          ];
        };
    };
  };
}
