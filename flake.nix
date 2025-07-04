{
  description = "Polysemy error tracking";

  inputs.hix.url = "git+https://git.tryp.io/tek/hix";

  outputs = {hix, ...}: hix.lib.pro {

    ghcVersions = ["ghc92" "ghc94" "ghc96" "ghc98" "ghc910"];
    hackage.versionFile = "ops/version.nix";
    gen-overrides.enable = true;

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

    managed = {
      enable = true;
      lower.enable = true;
    };

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


    overrides = {jailbreak, unbreak, hackage, ...}: {
      polysemy-test = unbreak;
    };

    envs.ghc910.overrides = {hackage, jailbreak, ...}: {
      incipit-base = jailbreak;
      incipit-core = jailbreak;
      polysemy-test = jailbreak;
    };

  };
}
