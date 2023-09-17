{
  description = "Polysemy error tracking";

  inputs = {
    hix.url = "git+https://git.tryp.io/tek/hix";
    polysemy-test.url = "git+https://git.tryp.io/tek/polysemy-test";
  };

  outputs = {hix, polysemy-test, ...}: hix.lib.pro {
    ghcVersions = ["ghc92" "ghc94" "ghc96"];
    hackage.versionFile = "ops/version.nix";
    deps = [polysemy-test];
    compiler = "ghc94";
    gen-overrides.enable = true;

    cabal = {
      license = "BSD-2-Clause-Patent";
      license-file = "LICENSE";
      author = "Torsten Schmits";
      prelude = {
        enable = true;
        package = {
          name = "incipit-core";
          version = ">= 0.4 && < 0.6";
        };
        module = "IncipitCore";
      };
      meta = {
        maintainer = "hackage@tryp.io";
        category = "Error";
        github = "tek/polysemy-resume";
        extra-source-files = ["readme.md" "changelog.md"];
      };
    };

    packages.polysemy-resume = {
      src = ./packages/resume;

      cabal.meta.synopsis = "Polysemy error tracking";

      library = {
        enable = true;
        dependencies = [
          "polysemy ^>= 1.9"
          "transformers"
        ];
      };

      test = {
        enable = true;
        dependencies = [
          "polysemy ^>= 1.9"
          "polysemy-plugin ^>= 0.4.3"
          "polysemy-test >= 0.6 && < 0.9"
          "stm"
          "tasty ^>= 1.4"
        ];
      };

    };

  };
}
