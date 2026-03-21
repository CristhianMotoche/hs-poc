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

  # See full reference at https://devenv.sh/reference/options/
}
