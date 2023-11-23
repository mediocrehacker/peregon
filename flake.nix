{
  description = "Peregon: convert latex to pdf";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    systems.url = "github:nix-systems/default";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.treefmt-nix.flakeModule
      ];

      perSystem = { self', system, lib, config, pkgs, ... }:
        let
          tex = (pkgs.texlive.combine {
            inherit (pkgs.texlive) scheme-small babel setspace fontspec
              chktex enumitem xifthen ifmtarg filehook
              upquote tools ms geometry graphics oberdiek
              fancyhdr lastpage xcolor etoolbox unicode-math
              ucharcat sourcesanspro tcolorbox pgf environ
              trimspaces parskip hyperref url euenc
              collection-fontsrecommended ragged2e
              framed paralist titlesec paratype inter
              koma-script pbox background xkeyval everypage
              changepage cm-unicode xunicode collection-langcyrillic
              lh lm
              ;
          }
          );
        in
        {
          # Our only Haskell project. You can have multiple projects, but this template
          # has only one.
          # See https://github.com/srid/haskell-flake/blob/master/example/flake.nix
          haskellProjects.default = {
            # The base package set (this value is the default)
            # basePackages = pkgs.haskellPackages;

            # Packages to add on top of `basePackages`
            packages = {
              # Add source or Hackage overrides here
              # (Local packages are added automatically)
              /*
            aeson.source = "1.5.0.0" # Hackage version
            shower.source = inputs.shower; # Flake input
              */
            };

            # Add your package overrides here
            settings = {
              /*
            peregon = {
              haddock = false;
            };
            aeson = {
              check = false;
            };
              */
            };

            # Development shell configuration
            devShell = {
              hlsCheck.enable = false;
            };

            # What should haskell-flake add to flake outputs?
            autoWire = [ "packages" "apps" "checks" ]; # Wire all but the devShell
          };

          # Auto formatters. This also adds a flake check to ensure that the
          # source tree was auto formatted.
          treefmt.config = {
            projectRootFile = "flake.nix";

            programs.ormolu.enable = true;
            programs.nixpkgs-fmt.enable = true;
            programs.cabal-fmt.enable = true;
            programs.hlint.enable = true;

            # We use fourmolu
            programs.ormolu.package = pkgs.haskellPackages.fourmolu;
            settings.formatter.ormolu = {
              options = [
                "--ghc-opt"
                "-XImportQualifiedPost"
              ];
            };
          };

          # Default package & app.
          packages.default = pkgs.haskell.lib.justStaticExecutables self'.packages.peregon;
          # docker load -i $(nix build .#dockerImage --print-out-paths)
          # docker run -v /etc/ssl:/etc/ssl -p 127.0.0.1:8081:8081 peregon
          # docker images -a | grep none | awk '{ print $3; }' | xargs docker rmi --force
          packages.dockerImage = pkgs.dockerTools.buildImage {
            name = "mediocrehacker/peregon";
            tag = "latest";
            # created = "now";
            copyToRoot = pkgs.buildEnv {
              name = "peregon-root";
              paths = with pkgs; [
                coreutils
                bash
                tex
              ];
              pathsToLink = [ "/bin" "/tmp" "/static" "/texmf" ];
            };
            config = {
              # WorkingDir = "/data";
              Cmd = [ "${pkgs.lib.getExe self'.packages.default}" ];
              ExposedPorts = { "8081/tcp" = { }; };
            };
          };

          # Default shell.
          devShells.default = pkgs.mkShell {
            name = "peregon";
            meta.description = "Haskell development environment";
            # See https://zero-to-flakes.com/haskell-flake/devshell#composing-devshells
            inputsFrom = [
              config.haskellProjects.default.outputs.devShell
              config.treefmt.build.devShell
            ];
            nativeBuildInputs = with pkgs; [
              tex
              just
            ];
          };
        };
    };
}
