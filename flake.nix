{
  description = "My personal website, hosted at squiddev.cc and joncoates.co.uk";

  inputs.utils.url = "github:numtide/flake-utils";
  # We depend on the latest Pandoc release, as 3.0 isn't in nixpkgs yet.
  inputs.doctemplates     = { url = "github:jgm/doctemplates/0.11"; flake = false; };
  inputs.gridtables       = { url = "github:quarto-dev/gridtables/v0.1.0.0"; flake = false; };
  inputs.hakyll           = { url = "github:jaspervdj/Hakyll/v4.16.0.0"; flake = false; };
  inputs.jira-wiki-markup = { url = "github:tarleb/jira-wiki-markup/eddba79137d71261ba1a3800a611ef098e2ab655"; flake = false; };
  inputs.mime-types       = { url = "github:yesodweb/wai/mime-types-0.1.1.0"; flake = false; };
  inputs.pandoc           = { url = "github:jgm/pandoc/3.1.2"; flake = false; };
  inputs.pandoc-types     = { url = "github:jgm/pandoc-types/1.23"; flake = false; };
  inputs.texmath          = { url = "github:jgm/texmath/0.12.7.1"; flake = false; };

  outputs = { self, nixpkgs, utils, ... } @ inputs:
    let
      name = "squiddev-cc";
    in utils.lib.eachSystem ["x86_64-linux"] (system:
      let
        pkgs = import nixpkgs { inherit system; };

        # Packages which we need at runtime. These are injected into the path of
        # the ./site binary and into the dev shell.
        runtimePkgs = [
          pkgs.graphviz
          pkgs.nodePackages.katex
        ];

        # Build a haskell package without docs or tests, for a slimmer build.
        noJunk = x: pkgs.haskell.lib.overrideCabal x {
          doCheck = false;
          doHaddock = false;
          testHaskellDepends = [];
        };

        # Create a package just with the site generator using a minimal set of
        # sources. This saves us rebuilding it every time.
        siteBin =
          let
            src = pkgs.lib.sourceFilesBySuffices ./. [".hs" ".cabal" "LICENSE"];
            pkg = (pkgs.haskellPackages.override {
              overrides = self: super: {
                doctemplates        = noJunk (self.callCabal2nix "doctemplates"     inputs.doctemplates {});
                gridtables          = noJunk (self.callCabal2nix "gridtables"       inputs.gridtables {});
                hakyll              = noJunk (self.callCabal2nix "hakyll"           inputs.hakyll  {});
                jira-wiki-markup    = noJunk (self.callCabal2nix "jira-wiki-markup" inputs.jira-wiki-markup {});
                mime-types          = noJunk (self.callCabal2nix "mime-types"       (inputs.mime-types + "/mime-types") {});
                pandoc              = noJunk (self.callCabal2nix "pandoc"           inputs.pandoc {});
                pandoc-types        = noJunk (self.callCabal2nix "pandoc-types"     inputs.pandoc-types {});
                texmath             = noJunk (self.callCabal2nix "texmath"          inputs.texmath {});
              };
            }).callCabal2nix "${name}-bin" src {};
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
          packages = runtimePkgs ++ [
            pkgs.cabal-install
            pkgs.haskell-language-server
          ];
          inputsFrom = [siteBin.env];
        };
      }
    );
}
