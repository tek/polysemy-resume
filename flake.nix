{
  description = "Polysemy error tracking";

  inputs.hix.url = "git+https://git.tryp.io/tek/hix";

  outputs = { hix, ... }: hix.lib.pro {
    ghcVersions = ["ghc810" "ghc90" "ghc92" "ghc94"];
    hackage.versionFile = "ops/version.nix";
    gen-overrides.enable = true;

    overrides = { hackage, ... }: {
      incipit-base = hackage "0.5.1.0" "0hkqnqpdw8rvg4xzslw9sp3684ggyk9n4hr0lczwm8b0pzakzs0l";
      incipit-core = hackage "0.5.1.0" "04lyzycvqxyjqcd703cd33lnlk5va9wj3czpsybah0ybydnrwabd";
      polysemy = hackage "1.9.1.0" "05mhzjz6hz0dnxsn3cc0l6yyj5ch35gn8xfnx0a1gn3q8yljfg2a";
      polysemy-plugin = hackage "0.4.5.0" "0v2k0l42zaangwv050xfv5jdqfrbvdxfr533291ndsxalv8n3xi8";
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
          "polysemy-resume"
          "polysemy-test >= 0.6"
          "stm"
          "tasty ^>= 1.4"
        ];
      };

    };

  };
}
