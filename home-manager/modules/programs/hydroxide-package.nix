{ lib, buildGoModule, fetchFromGitHub, fetchpatch }:

buildGoModule rec {
  pname = "hydroxide";
  version = "0.2.22";

  src = fetchFromGitHub {
    owner = "emersion";
    repo = pname;
    rev = "3c5283705389a690e1631dbe771aef4b31702109";
    sha256 = "sha256-fF+pQnqAWBktc4NdQFTHeB/sEg5bPTxXtdL1x5JuXU8=";
  };

  vendorSha256 = "sha256-M5QlhF2Cj1jn5NNiKj1Roh9+sNCWxQEb4vbtsDfapWY=";

  doCheck = false;

  subPackages = [ "cmd/hydroxide" ];

  meta = with lib; {
    description = "A third-party, open-source ProtonMail bridge";
    homepage = "https://github.com/emersion/hydroxide";
    license = licenses.mit;
    maintainers = with maintainers; [ Br1ght0ne ];
    platforms = platforms.unix;
  };
}
