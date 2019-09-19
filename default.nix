let
  # See https://nixos.wiki/wiki/FAQ/Pinning_Nixpkgs
  pinned-pkgs = import (builtins.fetchGit {
    url = "https://github.com/NixOS/nixpkgs-channels.git";
    # If you remove "rev = ..." you end up using the latest from ref,
    # personally I would use the latest
    # (see (https://github.com/NixOS/nixpkgs-channels/tree/nixos-19.03)
    # once, and then pin it at that revision so it doesn't
    # keep downloading new packages (and potentially requiring a recompile,
    # if it downloads a newer version of the library you are using)
    rev = "147bd882fc66dda061f2eda14b3358f295320615";
    ref = "nixos-19.03";
  }) {};

  ctsrd-pinned-pkgs = import (builtins.fetchGit {
    name = "CTSRD-nix-packages";
    url = "git@github.com:KoviRobi/CTSRD-nix.git";
    # Uncomment "rev = ..." to pin to a specific version
    # rev = "b792113808e1e80af07ab736e3d459cc1b15fe9b";
    ref = "master";
  }) {};
in

# This allows overriding pkgs by passing `--arg pkgs ...`
{ pkgs ? pinned-pkgs, ctsrd-pkgs ? ctsrd-pinned-pkgs }:

# This imports everything from the pkgs, e.g. gcc48 and gnumake is the same as
# pkgs.gcc48 and pkgs.gnumake
with pkgs;

stdenv.mkDerivation {
  name = "BlueBasics";
  buildInputs = [ ctsrd-pkgs.bluespec ];
  src = [./BlueBasics.bsv ./Dict.bsv ./ListExtra.bsv ./MasterSlave.bsv ./Monoid.bsv ./SourceSink.bsv ./Virtualizable.bsv];
  builder = ./builder.sh;

  # For BlueSpec
  shellHook = "source bluespec-setup";
}

