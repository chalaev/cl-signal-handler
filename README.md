
# Table of Contents

1.  [The scope](#org966d9ca)
2.  [Prerequisites](#org73415ed)
3.  [Quick start – usage example](#org043de66)
4.  [Files](#org9b832c0)
5.  [Motivation](#org1784b43)
6.  [License](#orgafe5e08)
7.  [Support](#orge7a5ed9)

Simple kill-signall message acceptor for [sbcl](http://www.sbcl.org/).


<a id="org966d9ca"></a>

# The scope

A simple and reliable way to send messages to LISP services.


<a id="org73415ed"></a>

# Prerequisites

While this code is still not mature, [simple-log](https://github.com/chalaev/cl-simple-logger) package is needed for debugging, so `quicklisp` should be able to find its files.

For example, I have `quicklisp` installed in `~/quicklisp/` so [simple-log](https://github.com/chalaev/cl-simple-logger) is available in `~/quicklisp/local-projects/simple-log/`.


<a id="org043de66"></a>

# Quick start – usage example

Then `make` compiles the binary `example.bin` and launches it in background.
It is a server that will "listen" for `kill` signals for a few seconds.

LISP services may register one or more `hooks` using `(sh:register sdir func)`,
where `dir` is the name of the lock (sub)directory.

The shell script `generated/tell` is a client that

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


<a id="org9b832c0"></a>

# Files

1.  `signal-handler.org` is the main file containing most of the code with comments.
2.  The files in [goodies/](goodies/) are copied from the [lisp-goodies](https://notabug.org/shalaev/lisp-goodies) project.
3.  `Makefile` compiles `generated/example.lisp` into `generated/example.bin` and launches it.
4.  Other files in `generated/` can be produced by emacs from `signal-handler.org` using either `make` or `M-x org-babel-tangle`.
5.  `make.log` shows the log messages displayed by `make` command.


<a id="org1784b43"></a>

# Motivation

[dbus](https://github.com/death/dbus) might have many fancy features, but on my computer `(ql:quickload :dbus)` downloads 55 other packages to satisfy [dbus](https://github.com/death/dbus)'es requirements,
which is much more than I need to satisfy my modest needs: I just need simple text message exchange between `sbcl` and other programs (including shell scripts).


<a id="orgafe5e08"></a>

# License

This code is released under [MIT license](https://mit-license.org/).


<a id="orge7a5ed9"></a>

# Support

You can support this project by sending

1.  comments/questions to [oleg@chalaev.com](mailto:oleg@chalaev.com) and
2.  donations via [liberapay](https://liberapay.com/shalaev/donate) or [paypal](https://www.paypal.com/paypalme/chalaev).

