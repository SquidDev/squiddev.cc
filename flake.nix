{
  description = "My personal website, hosted at squiddev.cc and joncoates.co.uk";
  inputs.utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, utils }:
    let
      name = "squiddev-cc";
      compiler = "ghc924";
    in utils.lib.eachSystem ["x86_64-linux"] (system:
      let
        pkgs = import nixpkgs { inherit system; };

        # Packages which we need at runtime. These are injected into the path of
        # the ./site binary and into the dev shell.
        runtimePkgs = [
          pkgs.graphviz
          pkgs.nodePackages.katex
        ];

        # Create a package just with the site generator using a minimal set of
        # sources. This saves us rebuilding it every time.
        siteBin =
          let
            src = pkgs.lib.sourceFilesBySuffices ./. [".hs" ".cabal" "LICENSE"];
            pkg = pkgs.haskell.packages."${compiler}".callCabal2nix "${name}-bin" src {};
          in
          pkgs.haskell.lib.overrideCabal pkg {
            buildTools = [pkgs.makeBinaryWrapper];
            postFixup = ''
              wrapProgram $out/bin/site --set PATH ${pkgs.lib.makeBinPath runtimePkgs}
            '';
          };

        # The actual site contents - just invoke the site generator with the main
        # source!
        siteContents = pkgs.stdenv.mkDerivation {
          inherit name;
          src = ./.;

          buildPhase = "${siteBin}/bin/site build";
          installPhase = ''
            cp -r _site $out
          '';

          doCheck = true;
          checkPhase = "${siteBin}/bin/site check --internal-links";
        };
      in
      {
        packages.default = siteContents;

        # Exposing this as an app allows us to do nix run . watch, etc...
        apps.default = utils.lib.mkApp { drv = siteBin; exePath = "/bin/site"; };

        devShells.default = pkgs.mkShell {
          packages = runtimePkgs ++ [pkgs.cabal-install];
          inputsFrom = [siteBin.env];
        };
      }
    );
}
