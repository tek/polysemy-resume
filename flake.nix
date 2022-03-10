{
  description = "Polysemy Error Tracking";

  inputs.hix.url = github:tek/hix;
  inputs.incipit-core.url = github:tek/incipit-core;

  outputs = { hix, incipit-core, ... }:
  let

    all = { hackage, ... }: {
      incipit-base = hackage "0.2.0.0" "12979prkjk1kr1556mwsgf1v04rzd67xg68x6q9pnvm41pxbvk5w";
      incipit-core = hackage "0.2.0.0" "1v4xrqwcylbk32b6hzl6i7k0964varw2iy73s7mkjxpxpdg432ci";
      polysemy = hackage "1.6.0.0" "15k51ysrfcbkww1562g8zvrlzymlk2rxhcsz9ipsb0q6h571qgvf";
      polysemy-plugin = hackage "0.4.1.0" "117g92l1ppsqd3w0rqjrxfk0lx6yndd54rpymgxljilnv43zg29s";
      polysemy-test = hackage "0.5.0.0" "0lzbf7bfmcima8ib4hv68bjciy2n5s4x493g0a5cmdjj6pcg2d2k";
    };

  in
  hix.lib.flake {
    base = ./.;
    packages.polysemy-resume = ./packages/resume;
    overrides = { inherit all; };
    deps = [incipit-core];
    hackage.versionFile = "ops/hpack/shared/meta.yaml";
    ghci.preludePackage = "incipit-core";
  };
}
