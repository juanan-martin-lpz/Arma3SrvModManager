{ mkDerivation, aeson, aeson-pretty, base, base-compat
, base-encoding, bytestring, cmdargs, directory, directory-tree
, filepath, mtl, path, path-io, process, stdenv, text, unix-compat
, xxhash-ffi
}:
mkDerivation {
  pname = "armasrvmm";
  version = "0.2.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty base base-compat base-encoding bytestring
    cmdargs directory directory-tree filepath path path-io process text
    unix-compat xxhash-ffi
  ];
  executableHaskellDepends = [
    base cmdargs directory directory-tree filepath mtl text unix-compat
  ];
  description = "Gestor de Mods para servidores del juego ARMA 3";
  license = stdenv.lib.licenses.gpl3;
}
