{
  description = "Polysemy Error Tracking";

  inputs.hix.url = github:tek/hix;
  inputs.polysemy.url = path:../polysemy;
  inputs.polysemy-test.url = path:../polysemy-test;

  outputs = { hix, polysemy, polysemy-test, ... }:
  let
    compat901 = { hackage, source, jailbreak, minimal, noHpack, ... }: {
      type-errors-pretty = jailbreak;
    };

    compat = { hackage, source, minimal, ... }: {
      polysemy = minimal (source.root polysemy);
      polysemy-plugin = minimal (source.sub polysemy "polysemy-plugin");
      polysemy-test = source.package polysemy-test "polysemy-test";
    };

    common = { hackage, jailbreak, ... }: {
      path = hackage "0.9.0" "14symzl1rszvk5zivv85k79anz7xyl5gaxy0sm4vhhzsgxc59msv";
      path-io = jailbreak (hackage "1.6.3" "05hcxgyf6kkz36mazd0fqwb6mjy2049gx3vh8qq9h93gfjkpp2vc");
      relude = hackage "1.0.0.1" "164p21334c3pyfzs839cv90438naxq9pmpyvy87113mwy51gm6xn";
    };

    main = { hackage, ... }: {
      tasty-hedgehog = hackage "1.1.0.0" "0cs96s7z5csrlwj334v8zl459j5s4ws6gmjh59cv01wwvvrrjwd9";
    };
  in
  hix.flake {
    base = ./.;
    packages.polysemy-resume = ./packages/resume;
    overrides = [compat common main];
    compatOverrides = { all = compat; ghc901 = [common compat901]; };
    versionFile = "ops/hpack/shared/meta.yaml";
    ghcid.easy-hls = false;
    ghci.extraArgs = ["-fplugin=Polysemy.Plugin"];
  };
}
