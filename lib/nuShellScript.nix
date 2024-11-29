{ lib, nushell, writeTextFile }:
let
  toNu = v: "(\"${lib.escape ["\"" "\\"] (builtins.toJSON v)}\" | from json)";

  makeBinPathArray = pkgs:
    let
      binOutputs = builtins.filter (x: x != null) (map (pkg: lib.getOutput "bin" pkg) pkgs);
    in
    map (output: output + "/bin") binOutputs;
in
{ name, text, runtimeInputs ? [ ], runtimeEnv ? null, meta ? { }, checkPhase ? null }:
writeTextFile {
  inherit name meta;
  executable = true;
  destination = "/bin/${name}";
  allowSubstitutes = true;
  preferLocalBuild = false;
  text = ''
    #!${nushell}/bin/nu
  '' + lib.optionalString (runtimeEnv != null) ''

    load-env ${toNu runtimeEnv}
  '' + lib.optionalString (runtimeInputs != [ ]) ''

    $env.PATH = ${toNu (makeBinPathArray runtimeInputs)} ++ $env.PATH
  '' + ''

    ${text}
  '';
  checkPhase =
    if checkPhase == null then ''
      runHook preCheck
      ${nushell}/bin/nu --commands "nu-check --debug '$target'"
      runHook postCheck
    ''
    else checkPhase;
}
