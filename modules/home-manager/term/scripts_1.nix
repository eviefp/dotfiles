{ pkgs, ... }:
{
  crypto = pkgs.concatScript "crypto" [ ../../scripts/crypto.nu ];
}
