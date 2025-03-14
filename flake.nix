{
  description = "Polysemy error tracking";

  inputs.hix.url = "git+https://git.tryp.io/tek/hix";

  outputs = {hix, ...}: hix.lib.pro {
    ghcVersions = ["ghc92" "ghc94" "ghc96" "ghc98" "ghc910"];
    hackage.versionFile = "ops/version.nix";
    gen-overrides.enable = true;
    managed = {
      enable = true;
      lower.enable = true;
      envs.solverOverrides = {hackage, jailbreak, unbreak, ...}: {
        polysemy-test = jailbreak unbreak;
        incipit-base = jailbreak;
        incipit-core = jailbreak;
      };
      latest.compiler = "ghc910";
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

    overrides = {jailbreak, unbreak, hackage, ...}: {
      polysemy-test = unbreak;
    };

    envs.ghc910.overrides = {hackage, jailbreak, ...}: {
      incipit-base = hackage "0.6.1.0" "0iyyvxpyyybn5ygr875pav6g5hbs00wa9jbr7qslszqpkfpy5x33";
      incipit-core = hackage "0.6.1.0" "144c239nxl8zi2ik3ycic3901gxn8rccij3g609n2zgnn3b6zilj";
      polysemy-test = jailbreak;
    };

  };
}
