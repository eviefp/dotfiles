{ lib, buildGoModule, fetchFromGitHub, fetchpatch }:

buildGoModule rec {
  pname = "hydroxide";
  version = "0.2.23";

  src = fetchFromGitHub {
    owner = "emersion";
    repo = pname;
    rev = "654253a423d66ebc7d3ea1c27d1a4510df784749";
    sha256 = "sha256:0jnpi1mb9myqyksj79chkdw1wm6k8hqn7smfbb7jj83hp7rgik8x";
  };

  vendorSha256 = "sha256:0rm5v8vv1vgnw8dh3icns2q7w7x2a4yjlqnkwkkmi3w2bn22b51k";

  doCheck = false;

  subPackages = [ "cmd/hydroxide" ];

  meta = with lib; {
    description = "A third-party, open-source ProtonMail bridge";
    homepage = "https://ithub.com/emersion/hydroxide";
    license = licenses.mit;
    maintainers = with maintainers; [ Br1ght0ne ];
    platforms = platforms.unix;
  };
}
