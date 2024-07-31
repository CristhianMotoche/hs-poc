{ pkgs, lib, config, inputs, ... }:

{
  packages = [
    pkgs.git
    pkgs.sqlite
  ];

  languages.haskell.enable = true;
  languages.haskell.languageServer = pkgs.haskell-language-server;
  languages.haskell.package = pkgs.haskell.compiler.ghc964;

  pre-commit.hooks.ormolu.enable = true;
}
