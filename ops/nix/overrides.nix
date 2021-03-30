niv:
{
  pkgs,
  hackage,
}:
self: super:
let
  inherit (hackage) pack thunk cabal2nix cabal2nixNoHpack subPkg subPkgNoHpack github;

  versions = [
  ];
  versionOverrides = builtins.listToAttrs versions;

  custom = {
    polysemy = cabal2nixNoHpack "polysemy" niv.polysemy;
    polysemy-plugin = subPkgNoHpack "polysemy-plugin" "polysemy-plugin" niv.polysemy;
    polysemy-test = subPkg "packages/polysemy-test" "polysemy-test" niv.polysemy-test;
  };
in
  versionOverrides // custom
