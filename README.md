
# Table of Contents

1.  [Description](#org191cb96)
2.  [Prerequisites](#orgac461e8)
3.  [Quick start – usage example](#org35ed014)
4.  [Files](#orgd5542ae)
5.  [Motivation](#org7cd101f)
6.  [License](#org2d49fc4)

Simple kill-signall message acceptor for [sbcl](http://www.sbcl.org/).


<a id="org191cb96"></a>

# Description

A simple way to send messages to LISP services.


<a id="orgac461e8"></a>

# Prerequisites

This code needs:

1.  `lfp.h` (provided by `libfixposix-dev` package in [Debian](https://www.debian.org/)),
2.  [simple-log](https://github.com/chalaev/cl-simple-logger)  – needed for debugging/logging, and
3.  [lisp-goodies](https://github.com/chalaev/lisp-goodies) (CL package named `:shalaev` and [version.el](https://github.com/chalaev/lisp-goodies/blob/master/packaged/version.el)),

so LISP should be able to `require` these two packages.

For example, if `quicklisp` resides in `~/quicklisp/`,
these two packages can be installed as follows:

tar xjfv [cl-shalaev.tbz](https://github.com/chalaev/lisp-goodies/raw/master/packaged/cl-shalaev.tbz) &#x2013;directory=$HOME/quicklisp/local-projects/   
tar xjfv [simple-log.tbz](https://github.com/chalaev/cl-simple-logger/raw/master/packaged/simple-log.tbz) &#x2013;directory=$HOME/quicklisp/local-projects/


<a id="org35ed014"></a>

# Quick start – usage example

`make` compiles the binary `example.bin` and launches it in background.
It is a server that will "listen" for `kill` signals for a few seconds.
[Here is a typical make output.](make.log)

LISP services may register one or more `hooks` using `(sh:register dir func)`, where `dir` is the name of the lock (sub)directory.

The shell script [tell](generated/tell) is a client that

1.  creates lock directory to let other clients know that they should not interfere until the current conversation with the server is over,
2.  saves a message in a file "by", and
3.  sends `kill` signal to the server thus letting it know that the message is ready to be read.

After receiving the `kill` signal, `signal-handler` package checks every hook it has. A hook is a `cons`; its `car` is the sub-directory name.
If this sub-directory exists, the hook is activated, and its `cdr` (which is a function) is called with a single argument: the sub-directory name.

After that the file "by" is erased by the LISP code (client that uses `signal-handler` package).
In this way the client (shell script  [tell](generated/tell)) is notified that the message has been received, and now the server is ready to accept another request.
The sender deletes the sub-directory `sdir` in order to allow others to send messages to LISP code.


<a id="orgd5542ae"></a>

# Files

1.  [signal-handler.org](signal-handler.org) is the main file containing most of the code with comments,
2.  [Makefile](Makefile) compiles [generated/example.lisp](generated/example.lisp) into `generated/example.bin` and launches it, and
3.  [packaged/signal-handler.tbz](packaged/signal-handler.tbz) is the package archive.


<a id="org7cd101f"></a>

# Motivation

[dbus](https://github.com/death/dbus) might have many fancy features, but on my computer `(ql:quickload :dbus)` downloads 55 other packages to satisfy [dbus](https://github.com/death/dbus)'es requirements,
which is much more than I need to satisfy my modest needs: I just need simple text message exchange between `sbcl` and other programs (including shell scripts).


<a id="org2d49fc4"></a>

# License

This code is released under [MIT license](https://mit-license.org/).

