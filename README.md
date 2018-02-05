scripts
=======


Overview
--------

This is a collection of scripts written in Common Lisp. This repo started as my fork of
[Fare’s scripts](http://github.com/fare/fare-scripts).


Dependencies
------------

- sbcl
- cl-launch
- make


Building
--------

To install the scripts to `~/bin`, run:

```
$ make
```


Miscellany
----------

The file `default.nix` is used with [Nix](https://nixos.org/nix) to spawn a shell containing
programs and library dependencies conducive to development.

To load it, change to this repository’s directory then run:

```
$ nix-shell
```
