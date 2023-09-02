{
  description = "Polysemy error tracking";

  inputs.hix.url = "git+https://git.tryp.io/tek/hix";

  outputs = {hix, ...}: hix.lib.pro {
    ghcVersions = ["ghc92" "ghc94" "ghc96"];
    hackage.versionFile = "ops/version.nix";
    gen-overrides.enable = true;

    envs.ghc96.overrides = {hackage, jailbreak, ...}: {
      polysemy = hackage "1.9.1.1" "0hh99vxhf1cyfpzyj6j9s5l2yz7gdp1m10bk98fh2mjqxm207s4k";
      type-errors = jailbreak;
    };

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
