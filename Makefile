SH=/bin/sh
# The parameters SBCL and quicklispDir should probably be different on your computer:
# SBCL is the SBCL excecutable. You may use the standard one (/usr/bin/sbcl), but it is better to compile it manually.
SBCL = ~/local/bin/sbcl
# quicklispDir is the Quicklisp directory where my local packages are stored:
quicklispDir = ~/quicklisp/local-projects/signal-handler/

# simple: README.md generated/example.bin generated/description.org

tell: generated/example.bin README.md
	-rm -r /tmp/sbcl.lock/acceptor
	generated/example.bin & echo "Makefile--> PID=$$!"
	generated/tell

generated/example.bin: $(quicklispDir)example.bin
	cp -a $< $@

# компилирование пакета :signal-handler после каждого обновления:
generated/signal-handler.lisp: signal-handler.org
	emacsclient -e '(org-babel-tangle-file "$<")'
	-chmod a-x generated/*.lisp
	rsync -au goodies generated/*.lisp generated/*.asd description.org $(quicklispDir)
	cd $(quicklispDir) ; $(SBCL) --quit --eval '(progn (asdf:clear-system :signal-handler) (asdf:clear-system :signal-handler/example) (require :signal-handler/example))' ; cd -

generated/description.org: description.org
	rsync -au $< $@
	-chgrp tmp $@
	-chmod a-x $@

README.md: README.org
	emacsclient -e '(progn (find-file "README.org") (org-md-export-to-markdown))'
	-chgrp tmp $@
	-chmod a-x $@

clean:
	-rm -r generated/* $(quicklispDir)* ~/.cache/common-lisp/sbcl-2.0.10-linux-x64/home/shalaev/quicklisp/local-projects/signal-handler/

.PHONY: clean tell

$(quicklispDir)example.bin: generated/signal-handler.lisp generated/description.org
	@echo "*** COMPILING THE BINARY***"
	$(SBCL) --quit --load generated/signal-handler.asd --eval "(asdf:make :signal-handler/example)"
	-chgrp tmp $@
