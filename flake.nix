{
  description = "Polysemy Error Tracking";

  inputs.hix.url = github:tek/hix;
  inputs.incipit-core.url = github:tek/incipit-core;

  outputs = { hix, incipit-core, ... }:
  let

    ghc921 = { hackage, jailbreak, notest, ... }: {
    };

    all = { hackage, ... }: {
      polysemy-test = hackage "0.4.0.1" "038n31xxid72vrckr3afgkvbsvqhf9q4b912agg24ppjzckq2s15";
    };

  in
  hix.lib.flake {
    base = ./.;
    packages.polysemy-resume = ./packages/resume;
    overrides = { inherit all ghc921; };
    deps = [incipit-core];
    hackage.versionFile = "ops/hpack/shared/meta.yaml";
    ghci.preludePackage = "incipit-core";
  };
}
