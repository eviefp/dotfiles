{
  lib,
  python3Packages,
  fetchFromGitHub,
}:
python3Packages.buildPythonPackage rec {
  pname = "edge-tts";
  version = "7.2.0";
  pyproject = true;

  src = fetchFromGitHub {
    owner = "rany2";
    repo = "edge-tts";
    rev = version;
    hash = "sha256-HnMMh3N9mUF8ALRpgx1wjrF2RL2ntRyVOOz4RcJyRMI=";
  };

  build-system = with python3Packages; [
    setuptools
  ];

  propagatedBuildInputs = with python3Packages; [
    aiohttp
    certifi
    tabulate
    typing-extensions
  ];

  pythonImportsCheck = [
    "edge_tts"
    "edge_playback"
  ];

  meta = with lib; {
    description = "Use Microsoft Edge's online text-to-speech service from Python";
    homepage = "https://github.com/rany2/edge-tts";
    license = licenses.lgpl3Only;
    maintainers = [];
    mainProgram = "edge-tts";
  };
}
