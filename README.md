TODO:
- [ ] rename branch to main
- [ ] niv update
- [ ] rename `home` to `config` or something
- [ ] Create a Makefile
    - [ ] format all (nix) files
	- [ ] check they are formatted (for CI?)
	- [ ] niv update script?
	- [ ] is there a way to test they... "compile"?
- [ ] setup home-manager via configuration.nix
- [ ] export NIX_PATH from configuration.nix and use that 'globally'
- [ ] chromium settings & plugin settings
- [ ] Look into nixops

Emacs stuff:
- [x] emacs 28
- [x] figure out why the `emacsWithPackagesFromUsePackage` does not work (i.e. does not load `init.el`)
- [x] use display-fill-column-indicator-mode
- [ ] allow different font per machine
- [ ] haskell-mode config
- [ ] emacsclient thing
- [ ] extra packages to try out / install:
	- [ ] latex stuff (tex-site & company-acutex?)
	- [ ] idris & agda modes
	- [ ] org stuff (evil-org, org-bullets, org-tree-slide)
	- [ ] projectile
	- [ ] diminish
	- [ ] yasnippet
    - [ ] ws-butler # websockets I think
    - [ ] restclient
    - [ ] ranger
    - [ ] edit-indirect # galaxy brain figure it out later