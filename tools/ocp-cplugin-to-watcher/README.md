
# Usage:

ocp-watcher-monitor [OPTIONS] COMMAND

ocp-watcher-monitor can be used to register the OCaml commands called during
a build with ocp-watcher. The OCaml runtime must support use of CAML_CPLUGINS,
and especially the central_monitor.so plugin must be in use.
This program also needs a standalone version of ocp-watcher, which has not been
released yet.

Available options:
  -o FILE    Store results in FILE
  --all      Print all messages
  -- COMMAND Command to call
  -help      Display this list of options
  --help     Display this list of options

# Depends

You need support for `CAML_CPLUGINS`. For that, you can use the
`4.02.3+ocp` compiler in `github.com/lefessan/opam-repository-perso`

You also need:
* ocplib-concur
* ocp-build
* ocplib-watcher-api (Currently unavailable)

# Build

{{{
make opam-deps
make
}}}



