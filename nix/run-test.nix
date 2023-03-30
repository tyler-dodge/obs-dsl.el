{ emacsWithPackages }:
let
  pkgs = import <nixpkgs> {};
  versioned_emacs = emacsWithPackages (epkgs: with epkgs; [
    ert-async
    el-mock
    ert-runner
    uuid
    s
    deferred
    ht
    dash
    websocket
  ]);
in derivation rec {
  name = "obs-dsl";
  baseInputs = [];
  builder = "${pkgs.bash}/bin/bash";
  args = [ ./builder.sh ];
  setup = ./setup.sh;
  buildInputs = [
    versioned_emacs pkgs.coreutils];
  emacs = versioned_emacs;
  obs_dsl = ../obs-dsl.el;
  test_target = ../test;
  system = builtins.currentSystem;
}

  
