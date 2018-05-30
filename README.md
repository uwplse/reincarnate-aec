[![Build Status](https://travis-ci.com/uwplse/reincarnate.svg?token=zg5AwxhqBFTyg3rjZuV9&branch=master)](https://travis-ci.com/uwplse/reincarnate)

## CAD compilation and synthesis in Ocaml

### Setup

1. Install system dependencies. On macOS with [Homebrew](https://brew.sh/):
```
  $ brew install coreutils autoconf gnu-time gawk parallel git git-lfs graphviz gnuplot
  $ brew cask install openscad
  $ brew install ocaml opam
```

On Linux without root, using [Linuxbrew](http://linuxbrew.sh/):
```
  $ brew install autoconf parallel git git-lfs graphviz
  $ # ... depending on the Linux distro, install [OpenSCAD](http://www.openscad.org/downloads.html)
  $ # ... build gnuplot from source
  $ brew install ocaml opam
```

On Linux with root and `apt`:
```
  $ apt-get install autoconf parallel git git-lfs graphviz gnuplot openscad
  $ apt-get install ocaml opam
```

2. Install [opam](https://opam.ocaml.org/) packages:
```
  $ opam init
  $ eval `opam config env`
  $ opam install mlgmpidl zarith hashcons menhir js_of_ocaml
```

3. Run GNU parallel once interactively and acknowledge that you will cite the authors:
```
  $ parallel --citation

```
4. Build the compiler and synthesis tool:
```
  $ cd reincarnate/src
  $ make
```
