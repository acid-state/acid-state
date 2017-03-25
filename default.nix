{ mkDerivation, array, base, bytestring, cereal, containers
, directory, extensible-exceptions, filepath, mtl, network
, safecopy, stdenv, stm, template-haskell, unix
}:
mkDerivation {
  pname = "acid-state";
  version = "0.14.2";
  src = ./.;
  libraryHaskellDepends = [
    array base bytestring cereal containers directory
    extensible-exceptions filepath mtl network safecopy stm
    template-haskell unix
  ];
  homepage = "https://github.com/acid-state/acid-state";
  description = "Add ACID guarantees to any serializable Haskell data structure";
  license = stdenv.lib.licenses.publicDomain;
}
