{ cabal, bindingsDSL, blas, liblapack }:

cabal.mkDerivation (self: {
  pname = "bindings-levmar";
  version = "1.1.0.2";
  src = ./.;
  buildDepends = [ bindingsDSL ];
  extraLibraries = [ blas liblapack ];
  meta = {
    homepage = "https://github.com/basvandijk/bindings-levmar";
    description = "Low level bindings to the C levmar (Levenberg-Marquardt) library";
    license = "unknown";
    platforms = self.ghc.meta.platforms;
  };
})
