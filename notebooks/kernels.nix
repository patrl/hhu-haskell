{pkgs, ...}: {
  kernel.haskell.minimal = {
    enable = true;
    extraHaskellPackages = ps: [ps.containers];
      };
}
