# actual nix expressions, except for default.nix
# are generated using node2nix tool by the
#
# it needs node to be on its path
{pkgs, system, nodejs ? pkgs."nodejs-6_x", binwrap}:
let node-defs = builtins.attrValues (import ./elm-test.nix {
    inherit pkgs;
    inherit system;
    inherit nodejs;
  });
  elm-test =
    if builtins.length node-defs != 1
      then abort "elm-test only 1 node packages expected got ${builtins.length node-defs}"
      else builtins.head node-defs;
in
elm-test.overrideAttrs (oldAttrs: rec {
    buildInputs = oldAttrs.buildInputs ++ [pkgs.makeWrapper binwrap];
    postInstall = let 
      libPath = pkgs.lib.makeLibraryPath [
        pkgs.gmp
      ];
    in
    ''
      patchelf \
        --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" \
        --set-rpath "${libPath}" \
        $out/lib/node_modules/elm-test/node_modules/elmi-to-json/unpacked_bin/elmi-to-json
      mv $out/bin/elm-test $out/bin/.elm-test-wrapped
      makeWrapper $out/bin/.elm-test-wrapped $out/bin/elm-test --prefix PATH ":" '${nodejs}/bin'
    '';
    bypassCache = true;
    # lock file causes install to fail since npm tries to reinstall packages
    preRebuild = ''
      rm package-lock.json
    '';
})
