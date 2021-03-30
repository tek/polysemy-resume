{
  description = "Polysemy error tracking";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/cfed29bfcb28259376713005d176a6f82951014a;
    flake-utils.url = github:numtide/flake-utils;
    tryp-hs.url = github:tek/tryp-hs;
    tryp-hs.inputs.nixpkgs.follows = "nixpkgs";
    polysemy.url = github:polysemy-research/polysemy;
    polysemy-test.url = github:tek/polysemy-test;
  };

  outputs = { self, nixpkgs, tryp-hs, flake-utils, ... }@inputs:
  flake-utils.lib.eachSystem ["x86_64-linux"] (system:
    let
      project = tryp-hs.project {
        inherit system;
        base = ./.;
        compiler = "ghc8102";
        packages = {
          polysemy-resume = "packages/resume";
        };
        overrides = import ./ops/nix/overrides.nix inputs;
        ghci = {
          basicArgs = ["-Wall" "-Werror"];
        };
        ghcid.prelude = "packages/resume/lib/Prelude.hs";
        packageDir = "packages";
      };
    in {
      defaultPackage = project.ghc.polysemy-resume;
      devShell = project.ghcid-flake.shell;
      legacyPackages = {
        run = project.ghcid-flake.run;
        cabal = project.cabal;
        tags = project.tags.projectTags;
        hpack = project.hpack-script {};
      };
      packages = {
        polysemy-log = project.ghc.polysemy-resume;
      };
      checks = {
        polysemy-log = project.ghc.polysemy-resume;
      };
    }
  );
}
