{
  description = "Polysemy Error Tracking";

  inputs.polysemy-test.url = github:tek/polysemy-test;

  outputs = { polysemy-test, ... }:
  let
    inherit (polysemy-test.inputs) tryp-hs;

    common = { hackage, source, jailbreak, ... }: {
      polysemy = hackage "1.5.0.0" "1xl472xqdxnp4ysyqnackpfn6wbx03rlgwmy9907bklrh557il6d";
      polysemy-plugin = hackage "0.3.0.0" "1frz0iksmg8bpm7ybnpz9h75hp6hajd20vpdvmi04aspklmr6hj0";
    };

    main = { hackage, source, ... }: {
      path = hackage "0.8.0" "0isldidz2gypw2pz399g6rn77x9mppd1mvj5h6ify4pj4mpla0pb";
      tasty-hedgehog = hackage "1.1.0.0" "0cs96s7z5csrlwj334v8zl459j5s4ws6gmjh59cv01wwvvrrjwd9";
      relude = hackage "1.0.0.1" "164p21334c3pyfzs839cv90438naxq9pmpyvy87113mwy51gm6xn";
      polysemy-test = source.sub polysemy-test "packages/polysemy-test";
    };

    compat = { hackage, source, only, ... }: {
      polysemy-test = hackage "0.3.1.1" "0x0zg1kljr7a1mwmm3zrmha5inz3l2pkldnq65fvsig8f3x8rsar";
    };
  in
  tryp-hs.flake {
    base = ./.;
    packages.polysemy-resume = "packages/resume";
    overrides = tryp-hs.composeCabal [common main];
    compatOverrides = tryp-hs.composeCabal [common compat];
    versionFile = "ops/hpack/shared/meta.yaml";
  };
}
