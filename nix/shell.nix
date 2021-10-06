{ pkgs ? import <nixpkgs> {} }:
with pkgs;

let



  
in 
#idris_sdl
stdenv.mkDerivation {
  name = "idris2-env";
  buildInputs = [
    gmp
    (import "/home/jan/github.com/tcat/y.nix")
    yarn
    sass
    nodejs
    SDL
    SDL_gfx
     (with nodePackages; [
                    browserify
                    ])
    
  ];
}
