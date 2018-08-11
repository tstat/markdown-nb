{ mkDerivation, lens, mitchell-stdlib, safe-exceptions, stdenv
, text, websockets, yi-rope
}:
mkDerivation {
  pname = "markdown-nb-server";
  version = "0";
  src = ./..;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    lens mitchell-stdlib safe-exceptions text websockets yi-rope
  ];
  license = stdenv.lib.licenses.unfree;
  hydraPlatforms = stdenv.lib.platforms.none;
}
