
# Table of Contents

1.  [The scope](#org32f6edd)
2.  [Prerequisites](#org638fc4b)
3.  [Quick start – usage example](#orgd2c3ec5)
4.  [Files](#orge0c45da)
5.  [Motivation](#org9bef6e1)
6.  [License](#orga99efc6)
7.  [Support](#org14b198d)

Simple kill-signall message acceptor for [sbcl](http://www.sbcl.org/).


<a id="org32f6edd"></a>

# The scope

A simple and reliable way to send messages to LISP services.


<a id="org638fc4b"></a>

# Prerequisites

Although this code is quite mature, [simple-log](https://github.com/chalaev/cl-simple-logger) package is needed for debugging, so `quicklisp` should be able to find its files.

For example, I have `quicklisp` installed in `~/quicklisp/` so [simple-log](https://github.com/chalaev/cl-simple-logger) is available in `~/quicklisp/local-projects/simple-log/`.


<a id="orgd2c3ec5"></a>

# Quick start – usage example

`make` compiles the binary `example.bin` and launches it in background.
It is a server that will "listen" for `kill` signals for a few seconds.

LISP services may register one or more `hooks` using `(sh:register sdir func)`,
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


<a id="orge0c45da"></a>

# Files

1.  [signal-handler.org](signal-handler.md) is the main file containing most of the code with comments.
2.  [Makefile](Makefile) compiles [generated/example.lisp](generated/example.lisp) into `generated/example.bin` and launches it.
3.  Other files in [generated/](generated) can be produced by `emacs` from [signal-handler.org](signal-handler.md) using either `make` or `M-x org-babel-tangle`.
4.  [make.org](make.md) shows the log messages displayed by `make` command.
5.  [helpers/\*](helpers/) assist compilation.


<a id="org9bef6e1"></a>

# Motivation

[dbus](https://github.com/death/dbus) might have many fancy features, but on my computer `(ql:quickload :dbus)` downloads 55 other packages to satisfy [dbus](https://github.com/death/dbus)'es requirements,
which is much more than I need to satisfy my modest needs: I just need simple text message exchange between `sbcl` and other programs (including shell scripts).


<a id="orga99efc6"></a>

# License

This code is released under [MIT license](https://mit-license.org/).


<a id="org14b198d"></a>

# Support

You can support this project by sending

1.  comments/questions to [oleg@chalaev.com](mailto:oleg@chalaev.com) and
2.  donations via [liberapay](https://liberapay.com/shalaev/donate) or [paypal](https://www.paypal.com/paypalme/chalaev).

