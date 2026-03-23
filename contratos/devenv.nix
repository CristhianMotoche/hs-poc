{
  pkgs,
  lib,
  config,
  ...
}:
{
  # https://devenv.sh/languages/
  languages.haskell = {
    enable = true;
    package = pkgs.ghc;
  };

  packages = [
    pkgs.bashInteractive
  ];

  # See full reference at https://devenv.sh/reference/options/
}
