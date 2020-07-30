let
  sources = import ./nix/sources.nix;

  reddup = import sources.reddup;
  nixpkgs = import sources.nixpkgs { config.allowUnfree = true; };
  pkgs = nixpkgs.pkgs;
  amethyst = import ./dependencies/amethyst.nix { pkgs = pkgs; };
in
{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # This might not be required.
  # nixpkgs.config.nixpkgs.config.allowUnfree = true

  home.packages = with pkgs; [
    # TODO: remove from nix-env and uncomment
    haskellPackages.niv

    ## CLI
    jq
    yq
    httpie
    fish
    ripgrep
    reddup

    ## Dev
    neovim
    emacs
    hasklig
    lorri

    ## Nix
    nixfmt

    ## Haskell
    stack

    ## Other
    nodejs
    # haskellPackages.Agda
    # Idris
    # AgdaStdlib broken iirc

    ## Scala
    sbt

    ## Latex
    tectonic
    texlab

    # UI
    amethyst
  ];

  home.sessionVariables = {
    EDITOR = "nvim";
  };

    programs.bat = {
    enable = true;
    config = {
      theme = "TwoDark";
      pager = "less -FR";
    };
  };

  programs.broot = {
    enable = true;
    enableFishIntegration = true;
  };

    programs.direnv = {
    enable = true;
    enableFishIntegration = true;
    # package = pkgs.direnv;
  };

  programs.fish = {
    enable = true;
    package = pkgs.fish;
  };

  programs.fzf = {
    enable = true;
  };

  programs.starship = {
    enable = true;
    enableFishIntegration = true;
    settings = {
      add_newline = true;

      character = {
        symbol = "λ";
        error_symbol = "✗";
        use_symbol_for_status = true;
      };

      cmd_duration = {
        min_time = 100;
        prefix = "underwent ";
      };

      haskell = {
        symbol = " ";
        disabled = false;
      };
    };
  };

  programs.git = {
    enable = true;

    aliases = {
      lol = "log --graph --decorate --oneline --abbrev-commit";
      lola = "log --graph --decorate --oneline --abbrev-commit --all";
      hist = "log --pretty=format:'%h %ad | %s%d [%an]' --graph --date=short";
      lg = "log --color --graph --pretty=format:'%Cred%h$Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit --";
      recent = "for-each-ref --sort=committerdate refs/heads/ --format='%(HEAD) %(color:yellow)%(refname:short)%(color:reset) - %(color:red)%(objectname:short)%(color:reset) - %(contents:subject) - %(authorname) (%(color:green)%(committerdate:relative)%(color:reset))'";
      work = "log --pretty=format:'%h%x09%an%x09%ad%x09%s'";
    };

    ignores = [ "TAGS" ".bloop/" ];

    userEmail = "vladimir-gabriel.ciobanu@ing.com";
    userName = "Vladimir Ciobanu";

    delta = {
      enable = true;
      options = [ "--dark" ];
    };
  };

  manual.html.enable = true;

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "20.03";
}
