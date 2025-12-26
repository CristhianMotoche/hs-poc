{
  pkgs,
  lib,
  config,
  ...
}:
{
  languages.haskell = {
    enable = true;
    stack =  {
      enable = true;
    };
  };
}
