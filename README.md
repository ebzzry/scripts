scripts
=======


Overview
--------

This is a collection of scripts written in Common Lisp. This repo started as a fork of
[fare-scripts](http://github.com/fare/fare-scripts). The mksum subsystem was implemented by
[zhaqenl](https://github.com/zhaqenl).


System dependencies
-------------------

- sbcl
- cl-launch
- make

Use your system package manager to install the aboe.


Lisp dependencies
-----------------

- inferior-shell
- local-time
- ironclad
- clon
- cl-launch
- fare-utils
- cl-scripting
- mof

You may install the above with:

```
$ sbcl
* (dolist (x '(:inferior-shell :local-time :ironclad :clon :cl-launch :fare-utils :cl-scripting)) (ql:quickload x))
* (quit)
```

Mof can be fetched with:

```
$ cd ~/common-lisp
$ git clone https://github.com/ebzzry/mof.git
```


Building
--------

To install the scripts to `~/bin`, run:

```
$ make install
```


Miscellany
----------

The file `default.nix` is used with [Nix](https://nixos.org/nix) to spawn a shell containing
programs and library dependencies conducive to Common Lisp development.

To load it, change to this repository’s directory then run:

```
$ nix-shell
```
