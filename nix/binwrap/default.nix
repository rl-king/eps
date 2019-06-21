# actual nix expressions, except for default.nix
# are generated using node2nix tool by the
#
# it needs node to be on its path
{pkgs, system, nodejs ? pkgs."nodejs-8_x"}:
let node-defs = builtins.attrValues (import ./binwrap.nix {
    inherit pkgs;
    inherit system;
    inherit nodejs;
  });
  binwrap =
    if builtins.length node-defs != 1
      then abort "binwrap only 1 node packages expected got ${builtins.length node-defs}"
      else builtins.head node-defs;
in
binwrap.overrideAttrs (oldAttrs: rec {
    buildInputs = oldAttrs.buildInputs ++ [pkgs.makeWrapper];
    postInstall = ''
      mv $out/bin/binwrap-install $out/bin/.binwrap-install-wrapped
      makeWrapper $out/bin/.binwrap-install-wrapped $out/bin/binwrap-install --prefix PATH ":" '${nodejs}/bin'
    '';
})
