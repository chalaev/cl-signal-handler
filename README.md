
# Table of Contents

1.  [Description](#org1281d43)
2.  [Prerequisites](#org85dde69)
3.  [Quick start – usage example](#org0cfa78a)
4.  [Files](#orgd2f8b75)
5.  [Motivation](#orgd86d960)
6.  [License](#orgda40b44)

Simple kill-signall message acceptor for [sbcl](http://www.sbcl.org/).


<a id="org1281d43"></a>

# Description

A simple and reliable way to send messages to LISP services.


<a id="org85dde69"></a>

# Prerequisites

This code needs:

1.  `lfp.h` (provided by `libfixposix-dev` package in [Debian](https://www.debian.org/)),
2.  [simple-log](https://github.com/chalaev/cl-simple-logger)  – needed for debugging/logging, and
3.  [lisp-goodies](https://github.com/chalaev/lisp-goodies) (package named `:shalaev`),

so LISP should be able to `require` these two packages.

For example, if `quicklisp` resides in `~/quicklisp/`,
these two packages can be installed as follows:

tar xjfv [cl-shalaev.tbz](https://github.com/chalaev/lisp-goodies/raw/master/packaged/cl-shalaev.tbz) &#x2013;directory=$HOME/quicklisp/local-projects/   
tar xjfv [simple-log.tbz](https://github.com/chalaev/cl-simple-logger/raw/master/packaged/simple-log.tbz) &#x2013;directory=$HOME/quicklisp/local-projects/


<a id="org0cfa78a"></a>

# Quick start – usage example

`make` compiles the binary `example.bin` and launches it in background.
It is a server that will "listen" for `kill` signals for a few seconds.

LISP services may register one or more `hooks` using `(sh:register dir func)`,
where `dir` is the name of the lock (sub)directory.

The shell script [tell](generated/tell) is a client that

1.  creates lock directory to let other clients know that they should not interfere until the current conversation with the server is over,
2.  saves a message in a file "by", and
3.  sends `kill` signal to the server thus letting it know that the message is ready to be read.

After receiving the `kill` signal, the LISP code checks every hook it has.
A hook is a `cons`; its `car` is the sub-directory name `sdir`.
If this sub-directory exists, the hook is activated, and its `cdr` (which is a function)
is called with a string argument read from the file named "by" residing in `sdir`.

After that the file "by" is erased by the LISP code.
In this way the sender is notified that the message has been received,
and now the sender must delete the sub-directory `sdir` in order to allow others to send messages to LISP code.


<a id="orgd2f8b75"></a>

# Files

1.  [signal-handler.org](signal-handler.org) is the main file containing most of the code with comments,
2.  [Makefile](Makefile) compiles [generated/example.lisp](generated/example.lisp) into `generated/example.bin` and launches it,
3.  [make.org](make.org) contains log messages displayed by sucessfull `make` command,
4.  [helpers/\*](helpers/) assist compilation, and
5.  [packaged/signal-handler.tbz](packaged/signal-handler.tbz) is the package archive.


<a id="orgd86d960"></a>

# Motivation

[dbus](https://github.com/death/dbus) might have many fancy features, but on my computer `(ql:quickload :dbus)` downloads 55 other packages to satisfy [dbus](https://github.com/death/dbus)'es requirements,
which is much more than I need to satisfy my modest needs: I just need simple text message exchange between `sbcl` and other programs (including shell scripts).


<a id="orgda40b44"></a>

# License

This code is released under [MIT license](https://mit-license.org/).

