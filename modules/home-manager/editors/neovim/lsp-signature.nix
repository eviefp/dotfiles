{...}: {
  config = {
    programs.nixvim = {
      plugins.lsp-signature = {
        enable = true;
        settings = {
          select_signature_key = "<C-n>";
        };
      };
    };
  };
}
