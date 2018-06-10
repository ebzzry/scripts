scripts
=======


Overview
--------

This is a collection of scripts written in Common Lisp. This repo started as a fork of
[fare-scripts](http://github.com/fare/fare-scripts).

The mksum subsystem was implemented by [zhaqenl](https://github.com/zhaqenl).


Dependencies
------------

- sbcl
- cl-launch
- make


Building
--------

To install the scripts to `~/bin`, run:

```
$ make install
```


Miscellany
----------

The file `default.nix` is used with [Nix](https://nixos.org/nix) to spawn a shell containing
programs and library dependencies conducive to development.

To load it, change to this repository’s directory then run:

```
$ nix-shell
```
