
# Table of Contents

1.  [The scope](#org5639878)
2.  [Prerequisites](#orgb4af67f)
3.  [Quick start – usage example](#org34aed28)
4.  [Files](#orgb38177a)
5.  [Motivation](#org7b816fe)
6.  [License](#orge596808)
7.  [Support](#org2165c99)

Simple kill-signall message acceptor for [sbcl](http://www.sbcl.org/).


<a id="org5639878"></a>

# The scope

A simple and reliable way to send messages to LISP services.


<a id="orgb4af67f"></a>

# Prerequisites

This code uses packages:

1.  [simple-log](https://github.com/chalaev/cl-simple-logger)  – needed for debugging, and
2.  [lisp-goodies](https://github.com/chalaev/lisp-goodies) (shalaev),

so lisp should be able to `require` them.

For example, if `quicklisp` resides in `~/quicklisp/`,
these two packages can be installed as follows:

tar xjfv [shalaev.tbz](https://github.com/chalaev/lisp-goodies/blob/master/generated/shalaev.tbz) &#x2013;directory=$HOME/quicklisp/local-projects/   
tar xjfv [simple-log.tbz](https://github.com/chalaev/cl-simple-logger/blob/master/generated/simple-log.tbz) &#x2013;directory=$HOME/quicklisp/local-projects/


<a id="org34aed28"></a>

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


<a id="orgb38177a"></a>

# Files

1.  [signal-handler.org](signal-handler.md) is the main file containing most of the code with comments,
2.  [Makefile](Makefile) compiles [generated/example.lisp](generated/example.lisp) into `generated/example.bin` and launches it,
3.  [make.org](make.md) contains log messages displayed by sucessfull `make` command, and
4.  [helpers/\*](helpers/) assist compilation.


<a id="org7b816fe"></a>

# Motivation

[dbus](https://github.com/death/dbus) might have many fancy features, but on my computer `(ql:quickload :dbus)` downloads 55 other packages to satisfy [dbus](https://github.com/death/dbus)'es requirements,
which is much more than I need to satisfy my modest needs: I just need simple text message exchange between `sbcl` and other programs (including shell scripts).


<a id="orge596808"></a>

# License

This code is released under [MIT license](https://mit-license.org/).


<a id="org2165c99"></a>

# Support

You can support this project by sending

1.  comments/questions to [oleg@chalaev.com](mailto:oleg@chalaev.com) and
2.  donations via [liberapay](https://liberapay.com/shalaev/donate) or [paypal](https://www.paypal.com/paypalme/chalaev).

