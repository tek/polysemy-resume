{
  description = "Polysemy Error Tracking";

  inputs.hix.url = github:tek/hix;

  outputs = { hix, ... }:
  let

    ghc921 = { hackage, jailbreak, notest, ... }: {
      polysemy = hackage "1.7.1.0" "0qwli1kx3hk68hqsgw65mk81bx0djw1wlk17v8ggym7mf3lailyc";
      polysemy-plugin = hackage "0.4.3.0" "1r7j1ffsd6z2q2fgpg78brl2gb0dg8r5ywfiwdrsjd2fxkinjcg1";
      type-errors = notest;
    };

    all = { hackage, ... }: {
      polysemy-test = hackage "0.4.0.1" "038n31xxid72vrckr3afgkvbsvqhf9q4b912agg24ppjzckq2s15";
      polysemy = hackage "1.6.0.0" "15k51ysrfcbkww1562g8zvrlzymlk2rxhcsz9ipsb0q6h571qgvf";
      polysemy-plugin = hackage "0.4.0.0" "0pah1a8h8ckbv2fq20hrikrd1p5a3bdxr03npkyixc6mv5k1rmck";
      incipit-base = hackage "0.1.0.1" "0bcygln28zhrp0jqsm1z8p45k7faas5yamwddz2narsgpkzirx4y";
      incipit-core = hackage "0.1.0.1" "1bdkw0q4db3k73i3jjhil96p3rz3gw7mq9jcpcphamld72f4f5ni";
    };

  in
  hix.lib.flake {
    base = ./.;
    packages.polysemy-resume = ./packages/resume;
    overrides = { inherit all ghc921; };
    hackage.versionFile = "ops/hpack/shared/meta.yaml";
    ghci.preludePackage = "incipit-core";
  };
}
