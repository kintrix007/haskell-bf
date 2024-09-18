{ mkDerivation, array, base, lib }:
mkDerivation {
  pname = "bf";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ array base ];
  homepage = "https://github.com/kintrix007/bf#readme";
  license = lib.licenses.bsd3;
  mainProgram = "bf";
}
