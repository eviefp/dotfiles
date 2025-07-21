/****************************************************************************
  * Email module
  *
  * Set up email account, imap sync, notmuch mail management.
  **************************************************************************/
{ lib, config, pkgs, osConfig, ... }:
let
  cfg = config.evie.system.email;
in
{
  options.evie.system.email = {
    enable = lib.mkEnableOption "calendar defaults";
  };

  config = lib.mkIf cfg.enable {
    accounts.email = {

      maildirBasePath = "/home/evie/mail";

      accounts = {
        eevie = {
          primary = true;
          address = "me@eevie.ro";
          userName = "evieciobanu..thelxinoe";
          realName = "Evie Ciobanu";

          gpg = {
            key = "6A9BDD4C9EE01C020EDD1F6E272D83521C488CCD";
            signByDefault = false;
          };

          signature = {
            text = ''

              -- Evie
            '';
            showSignature = "append";
          };

          passwordCommand = "${pkgs.coreutils}/bin/cat ${osConfig.sops.secrets.peroxide_thelxinoe.path}";

          imap = {
            host = "thelxinoe";
            port = 1143;
            tls = {
              enable = true;
              useStartTls = true;
              certificatesFile = "${osConfig.sops.secrets.evie_certificate.path}";
            };
          };

          smtp = {
            host = "thelxinoe";
            port = 1025;
            tls = {
              enable = true;
              useStartTls = true;
              certificatesFile = "${osConfig.sops.secrets.evie_certificate.path}";
            };
          };

          mbsync = {
            enable = true;
            create = "both";
            remove = "both";
          };

          msmtp = {
            enable = true;
            tls.fingerprint = "BC:74:78:C0:F2:33:58:9E:D4:7F:03:D9:D4:48:97:E4:12:AB:FC:B9:A7:02:A6:A7:5B:A8:33:CB:42:C0:B9:71";
          };

          notmuch.enable = true;
          astroid.enable = true;
        };

        gmail-primary = {
          primary = false;
          address = "alexa.eviest@gmail.com";
          userName = "alexa.eviest@gmail.com";
          realName = "Evie Ciobanu";

          gpg = {
            key = "6A9BDD4C9EE01C020EDD1F6E272D83521C488CCD";
            signByDefault = false;
          };

          signature = {
            text = ''

              -- Evie
            '';
            showSignature = "append";
          };

          passwordCommand = "${pkgs.coreutils}/bin/cat ${osConfig.sops.secrets.gmail_password.path}";

          imap = {
            host = "imap.gmail.com";
            port = 993;
            tls = {
              enable = true;
              # useStartTls = true;
            };
          };

          smtp = {
            host = "smtp.gmail.com";
            port = 587;
            tls = {
              enable = true;
              useStartTls = true;
            };
          };

          mbsync = {
            enable = true;
            create = "both";
            remove = "both";
          };

          msmtp = {
            enable = true;
          };

          notmuch.enable = true;
          astroid.enable = true;
        };
      };
    };

    home.file.".mailcap".text = ''
      text/html;  w3m -dump -o document_charset=%{charset} '%s'; nametemplate=%s.html; copiousoutput
    '';

    home.file.".config/astroid/keybindings".text = ''
      # searching in main window
      main_window.search_tag=?
      main_window.search=/
      main_window.show_help=H

      # switching between buffers
      main_window.next_page=K
      main_window.previous_page=J
      thread_index.next_page=K
      thread_index.previous_page=J
      help.next_page=K
      help.previous_page=J
      thread_view.scroll_up=u
      thread_view.scroll_down=d
      help.scroll_up=u
      help.scroll_down=d
      help.page_down=u
      help.page_up=d
      thread_view.delete=D
      thread_index.scroll_up=u
      thread_index.scroll_down=d
      main_window.open_new_window=T
      main_window.jump_to_page=C-1

      # undo
      thread_index.undo=U

      # beginning/end of buffer
      thread_index.scroll_home=g
      thread_index.scroll_end=G
      thread_view.scroll_home=g
      thread_view.scroll_end=G

      # replying, composing, etc
      main_window.new_mail=c
      thread_index.reply=r
      thread_index.reply_all=R
      thread_view.reply=r
      thread_view.reply_all=R


      # tagging, etc
      main_window.mark_unread=N
      thread_index.multi.achive=a
      thread_index.delete=D
      thread_index.label=l

      # closing app, tabs
      main_window.quit_ask=q # Quit astroid, default: q
      main_window.quit=Q # Quit astroid (without asking), default: Q
      main_window.close_page=x # Close mode (or window if other windows are open), default: C-w
    '';

    programs = {
      mbsync = {
        enable = true;
      };
      msmtp = {
        enable = true;
      };
      notmuch = {
        enable = true;
        new.tags = [ "unread" "unsorted" "inbox" ];
        hooks = {
          postNew = ''
            notmuch tag +evie -unsorted -- to:*@eevie.ro
            notmuch tag +evie -unsorted -- to:me@eevie.ro
            notmuch tag +evie -unsorted -- to:'Evie Ciobanu'
            notmuch tag +meowing -unsorted -- to:*@group-meowing.ro
            notmuch tag +gmail -unsorted -- to:*evie*@gmail.com
            notmuch tag +hf -unsorted -- from:*@haskell.foundation
            notmuch tag +hf -unsorted -- to:*@haskell.foundation
            notmuch tag +sent -unsorted -- from:*@eevie.ro
            notmuch tag --remove-all +junk -- to:glamira@eevie.ro
          '';
        };
      };

      astroid = {
        enable = false;
        externalEditor = "kitty -e nvim %1";
        extraConfig = {
          editor = {
            attachment_directory = "~/Downloads/mail";
          };
          startup.queries = {
            "a_important" = "tag:important";
            "b_unread" = "tag:unread";
            "c_inbox" = "tag:gmail or tag:evie";
            "d_unsorted" = "tag:unsorted";
            "e_evie" = "tag:evie";
            "g_gmail" = "tag:gmail";
          };
          thread_view = {
            expand_flagged = true;
            indent_messages = true;
            open_html_part_external = true;
            # preferred_html_only = true;
          };
        };
      };
    };

    services = {
      mbsync = {
        enable = true;
        postExec = "${pkgs.notmuch}/bin/notmuch new";
        verbose = true;
      };
    };
  };
}
