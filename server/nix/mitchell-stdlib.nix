{ mkDerivation, aeson, base, bytestring, containers, fetchgit, lens
, reactive-banana, stdenv, text, unix, unliftio
, unordered-containers
}:
mkDerivation {
  pname = "mitchell-stdlib";
  version = "0.1.0";
  src = fetchgit {
    url = "https://github.com/mitchellwrosen/mitchell-stdlib.git";
    sha256 = "0mywfvy0y4a57pcdghal2zyf8lly3vfh27px8h7fz9j85xlwjm8d";
    rev = "a64f3606fd68c432db9c3591cd14448dfa4e47ad";
  };
  configureFlags = [
    "-fdep-aeson" "-fdep-lens" "-fdep-reactive-banana" "-fdep-unix"
    "-fdep-unliftio" "-fdep-unordered-containers"
  ];
  libraryHaskellDepends = [
    aeson base bytestring containers lens reactive-banana text unix
    unliftio unordered-containers
  ];
  license = stdenv.lib.licenses.bsd3;
}
