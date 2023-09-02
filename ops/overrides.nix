{
dev = {
};
ghc92 = {
};
ghc94 = {
};
ghc96 = {
  polysemy = {
  meta = {
    sha256 = "0hh99vxhf1cyfpzyj6j9s5l2yz7gdp1m10bk98fh2mjqxm207s4k";
    ver = "1.9.1.1";
  };
  drv = { mkDerivation, async, base, Cabal, cabal-doctest, containers
, doctest, first-class-families, hspec, hspec-discover
, inspection-testing, lib, mtl, stm, syb, template-haskell
, th-abstraction, transformers, type-errors, unagi-chan
}:
mkDerivation {
  pname = "polysemy";
  version = "1.9.1.1";
  src = /nix/store/cw511s4qyq4vkgxssrc448vh7i1qy6l8-source;
  setupHaskellDepends = [ base Cabal cabal-doctest ];
  libraryHaskellDepends = [
    async base containers first-class-families mtl stm syb
    template-haskell th-abstraction transformers type-errors unagi-chan
  ];
  testHaskellDepends = [
    async base containers doctest first-class-families hspec
    hspec-discover inspection-testing mtl stm syb template-haskell
    th-abstraction transformers type-errors unagi-chan
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/polysemy-research/polysemy#readme";
  description = "Higher-order, low-boilerplate free monads";
  license = lib.licenses.bsd3;
}
;
}
;
};
hls = {
};
min = {
};
}