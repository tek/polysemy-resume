{
  description = "Polysemy error tracking";

  inputs.hix.url = git+https://git.tryp.io/tek/hix;
  inputs.polysemy.url = github:polysemy-research/polysemy?ref=torsten/scoped-switch-interpreter;

  outputs = { hix, polysemy, ... }:
  let

    all = { hackage, source, ... }: {
      incipit-base = hackage "0.4.0.0" "0g04mw1si70g5kkgz9gnk460d4pvm65i30hd9abrg6g0ryizixqf";
      incipit-core = hackage "0.4.0.0" "168m94c1480y8lhin1sbrwzr14dq13ixkgkcl7ikq78vcq267521";
      polysemy = source.root polysemy;
      polysemy-plugin = hackage "0.4.4.0" "08ry72bw78fis9iallzw6wsrzxnlmayq2k2yy0j79hpw4sp8knmg";
    };

    dev = { hackage, ... }: {
      polysemy-test = hackage "0.7.0.0" "1m6ncbihr742765rshz6w7dn450f3d2ip6ci3qah27lnz7yrwmp6";
    };

  in
  hix.lib.pro ({ config, lib, ... }: {
    packages.polysemy-resume = ./packages/resume;
    overrides = { inherit all dev; };
    hpack.packages = import ./ops/hpack.nix { inherit config lib; };
    hackage.versionFile = "ops/version.nix";
    ghci = {
      preludePackage = "incipit-core";
      preludeModule = "IncipitCore";
    };
  });
}
