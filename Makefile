SBCL = ~/local/bin/sbcl
# where my local packages are stored:
quicklispDir = $$HOME/quicklisp/local-projects/signal-handler

LFNs = signal-handler example
LISPs = $(addsuffix .lisp, $(LFNs))
package = $(LISPs) signal-handler.asd description.org version.org

OFNs = signal-handler packaging
ORGs = $(addsuffix .org, $(OFNs))

#SH=/bin/sh

all: quicklisp README.md generated/signal-handler.tbz $(addprefix generated/from/, $(ORGs)) $(quicklispDir)/example.bin demo git
quicklisp: $(quicklispDir)/ $(addprefix $(quicklispDir)/, $(package)) $(addprefix generated/from/, $(ORGs))

demo: $(quicklispDir)/example.bin generated/from/signal-handler.org
	-rm -r /tmp/sbcl.lock/acceptor
	$(quicklispDir)/example.bin & echo "Makefile--> PID=$$!"
	generated/tell

$(quicklispDir)/example.bin: quicklisp generated/description.org
	@echo "*** COMPILING THE BINARY ***"
	$(SBCL) --quit --eval "(asdf:make :signal-handler/example)"
	@echo "\n*** COMPILED THE BINARY, ***\nwill launch the DEMO now\n*****"
	-@chgrp tmp $@

generated/signal-handler.tbz: quicklisp
	tar jcfv $@ --directory=$(quicklispDir)/..  signal-handler
	-@chgrp tmp $@

generated/description.org: description.org
	cat $< > $@
	-@chgrp tmp $@

$(quicklispDir)/%.lisp: generated/from/signal-handler.org generated/from/packaging.org
	cat generated/headers/$(notdir $@) generated/$(notdir $@) > $@
	-@chgrp tmp $@

$(quicklispDir)/%.asd: generated/from/packaging.org
	cat generated/$(notdir $@) > $@
	-@chgrp tmp $@

$(quicklispDir)/%.org: %.org
	cat $< > $@
	-@chgrp tmp $@

generated/from/%.org: %.org generated/from/ generated/headers/
	echo `emacsclient -e '(printangle "$<")'` | sed 's/"//g' > $@
	-@chgrp tmp $@ `cat $@`

README.md: README.org
	emacsclient -e '(progn (find-file "README.org") (org-md-export-to-markdown))'
	-@chgrp tmp $@
	-@chmod a-x $@

clean:
	-$(SBCL) --quit --eval '(progn (asdf:clear-system :signal-handler) (asdf:clear-system :signal-handler/example))'
	-rm -r $(quicklispDir) generated

.PHONY: clean quicklisp all git demo

%/:
	[ -d $@ ] || mkdir -p $@

git: generated/signal-handler.tbz next-commit.txt README.md
	@echo "===="
	-@echo "git commit -am '"`head -n1 next-commit.txt`"'"
	@echo "git push origin master"


version.org: change-log.org helpers/derive-version.el
	emacsclient -e '(progn (load "$(CURDIR)/helpers/derive-version.el") (format-version "$<"))' | sed 's/"//g' > $@
	@echo "← generated `date '+%m/%d %H:%M'` from [[file:$<][$<]]" >> $@
	@echo "by [[file:helpers/derive-version.el][derive-version.el]]" >> $@
	-@chgrp tmp $@
