let
  pkgs = import <nixpkgs> { };

in
  { 
    xml-prettify-text = pkgs.haskellPackages.callPackage ./xml-prettify-text.nix { };
  }
