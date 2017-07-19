{ compiler ? "ghc802", profiling ? false, haddock ? true }:
  let
   config = {
     packageOverrides = pkgs: with pkgs.haskell.lib; {
       haskell = pkgs.haskell // {
         packages = pkgs.haskell.packages // {
	    ${compiler} = pkgs.haskell.packages.${compiler}.override {
               overrides = self: super: {
                 mkDerivation = args: super.mkDerivation (args // {
                   doHaddock = haddock;
		   enableLibraryProfiling = profiling;
		 });
               acid-state = self.callPackage ./acid-state.nix { };
            };
          };
        };
      };
    };
  };
in with (import <nixpkgs> { inherit config; }).haskell.packages.${compiler}; {
  inherit acid-state;
}
