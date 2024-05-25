{
  description = "Polysemy error tracking";

  inputs.hix.url = "git+https://git.tryp.io/tek/hix";

  outputs = {hix, ...}: let
    overrides = {jailbreak, unbreak, ...}: {
      polysemy-test = jailbreak unbreak;
    };
  in hix.lib.pro {
    ghcVersions = ["ghc92" "ghc94" "ghc96" "ghc98"];
    compat.versions = ["ghc94" "ghc96"];
    hackage.versionFile = "ops/version.nix";
    gen-overrides.enable = true;
    managed = {
      enable = true;
      lower.enable = true;
      envs.solverOverrides = overrides;
      latest.compiler = "ghc98";
    };

    inherit overrides;
    envs.latest.overrides = overrides;
    envs.lower.overrides = overrides;

    cabal = {
      license = "BSD-2-Clause-Patent";
      license-file = "LICENSE";
      author = "Torsten Schmits";
      prelude = {
        enable = true;
        package = "incipit-core";
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
          "polysemy"
          "transformers"
        ];
      };

      test = {
        enable = true;
        dependencies = [
          "polysemy"
          "polysemy-plugin"
          "polysemy-test"
          "stm"
          "tasty"
        ];
      };

    };

  };
}
