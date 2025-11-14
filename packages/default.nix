{
  pkgs,
  pkgs-2411,
  lib,
  ...
}:
rec {
  isw = pkgs.callPackage ./isw.nix {};

  # peroxide uses buildGo122Module which has been deprecated in nixpkgs-25.05
  # TODO: use Hydroxide or fork peroxide
  peroxide = pkgs-2411.callPackage ./peroxide.nix {};

  edge-tts = pkgs.callPackage ./edge-tts.nix {};
  lue = pkgs.callPackage ./lue.nix {inherit edge-tts;};

  # TODO: stop overriding when https://github.com/Alexays/Waybar/pull/4400 gets merged
  waybar = pkgs.waybar.overrideAttrs (_: {
    src = pkgs.fetchFromGitHub {
      owner = "Alexays";
      repo = "Waybar";
      rev = "559079e9a6afda77754afaf7c8d3f588c1d6206d";
      hash = "sha256-ttmz2FOvDXNgvOMBXwvYY91yfc1v6n+LOfXCj56QdLo=";
    };
    patches = [
      ../patches/waybar-sort-workspaces-by-output.patch
    ];
  });

  # https://github.com/neovim/neovim
  neovim =
    pkgs.neovim-unwrapped.overrideAttrs
    (_: {
      version = "0.12.0-dev";
      src = pkgs.fetchFromGitHub {
        owner = "neovim";
        repo = "neovim";
        rev = "8d6963a6a0f083350549d259606c388ae07b266f";
        hash = "sha256-yVMEGlnqYyEf5tBcL2BLwDSIVbbZ4bLthiZ8+BnPAYE=";
      };
    });
}
// (import ./scripts.nix {inherit pkgs lib;})
