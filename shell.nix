{ pkgs ? import <nixpkgs> {} }:
with pkgs;

let

   p="mufum";
  
in 
#idris_sdl2 idris_sdl

stdenv.mkDerivation {
  name = "idris-env";
  LD_LIBRARY_PATH="${pkgs.postgresql_10.lib}/lib";
  buildInputs = [
    #postgresql_10
    gmp
    yarn
    yarn2nix
    sass
    nodejs
     (with nodePackages; [
       browserify
       js-beautify
       #foundation-sites
       bower
       gulp
       
     ])
    #SDL2.dev
    zulip.out
    SDL2.dev
    SDL2_image.out
    #postgresql_10.lib
    #postgresql_10
#    postgresql_10.out    
    pkg-config.out
    libpqxx
    ];
}
