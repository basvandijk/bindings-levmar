{ mkDerivation, base, bindings-DSL, pkgs, stdenv }:
mkDerivation {
  pname = "bindings-levmar";
  version = "HEAD";
  src = ./.;
  libraryHaskellDepends = [ base bindings-DSL ];
  librarySystemDepends = with pkgs; [ blas liblapack ];
  homepage = "https://github.com/basvandijk/bindings-levmar";
  description = "Low level bindings to the C levmar (Levenberg-Marquardt) library";
  license = "unknown";
}
