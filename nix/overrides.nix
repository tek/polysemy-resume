niv:
{
  pkgs,
  hackage,
}:
self: super:
let
  inherit (hackage) pack thunk cabal2nix subPkg github;

  versions = [
    (pack "first-class-families" "0.8.0.0" "0266lqagnxmd80n9i0f1xsh4zfrmab5aazyp4ii5nqch3474gpm6")
    (pack "string-interpolate" "0.3.0.1" "0fmzv54xx56hm85kj01n3zp6cd7j39flmzdkha0ldw804xlas3lv")
    (pack "path" "0.8.0" "0isldidz2gypw2pz399g6rn77x9mppd1mvj5h6ify4pj4mpla0pb")
    (pack "path-io" "0.3.1" "07m7q36pdkqk18bmf0lkafjc9npksym7dhn2am1m9c1rvj3b26qf")
    (pack "relude" "0.7.0.0" "0flrwzxdd9bd3knk48zkhadwlad01msskjby1bfv4snr44q5xfqd")
  ];
  versionOverrides = builtins.listToAttrs versions;

  custom = {
    chronos = cabal2nix "chronos" niv.chronos;
    polysemy = cabal2nix "polysemy" niv.polysemy;
    polysemy-plugin = subPkg "polysemy-plugin" "polysemy-plugin" niv.polysemy;
    polysemy-test = subPkg "packages/polysemy-test" "polysemy-test" niv.polysemy-test;
  };
in
  versionOverrides // custom
