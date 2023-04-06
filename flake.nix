{
  description = "Polysemy error tracking";

  inputs.hix.url = "git+https://git.tryp.io/tek/hix";

  outputs = { hix, ... }: hix.lib.pro {
    ghcVersions = ["ghc810" "ghc90" "ghc92" "ghc94"];
    hackage.versionFile = "ops/version.nix";

    overrides = { hackage, ... }: {
      incipit-base = hackage "0.4.0.0" "0g04mw1si70g5kkgz9gnk460d4pvm65i30hd9abrg6g0ryizixqf";
      incipit-core = hackage "0.4.0.0" "168m94c1480y8lhin1sbrwzr14dq13ixkgkcl7ikq78vcq267521";
      polysemy = hackage "1.9.0.0" "1af07cppnjpv5v56wanya1mhkvbfnyynf5447mnkcf4zc4k23pyk";
      polysemy-plugin = hackage "0.4.4.0" "08ry72bw78fis9iallzw6wsrzxnlmayq2k2yy0j79hpw4sp8knmg";
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
