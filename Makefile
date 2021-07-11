# Uncomment for debugging:
# OLD_SHELL := $(SHELL)
# SHELL = $(warning Building $@$(if $<, (from $<))$(if $?, ($? newer)))$(OLD_SHELL)

SBCL = ~/local/bin/sbcl
# where my local packages are stored:
quicklispDir = $$HOME/quicklisp/local-projects/signal-handler

LFNs = signal-handler example macros def-SH def-example
LISPs = $(addsuffix .lisp, $(LFNs))
package = $(LISPs) signal-handler.asd description.org version.org

OFNs = signal-handler packaging macros def-SH def-example
ORGs = $(addsuffix .org, $(OFNs))

genFroms=$(addprefix generated/from/, $(ORGs))

#SH=/bin/sh

all: quicklisp README.md packaged/signal-handler.tbz $(genFroms) $(quicklispDir)/example.bin demo
quicklisp: $(quicklispDir)/ $(addprefix $(quicklispDir)/, $(package)) $(genFroms)

demo: $(quicklispDir)/example.bin
	-rm -r /tmp/sbcl.lock/acceptor
	@$(quicklispDir)/example.bin & echo "Makefile--> PID=$$!"
	@echo "\n*** will launch the DEMO now ***\n"
	generated/tell

$(quicklispDir)/example.bin: quicklisp generated/description.org
	@echo "*** COMPILING THE BINARY ***"
	$(SBCL) --quit --eval "(asdf:make :signal-handler/example)" 2> generated/example.bin.2.log > generated/example.bin.1.log
	@echo "\n*** COMPILED THE BINARY ***\n"
	-@chgrp tmp $@

packaged/signal-handler.tbz: quicklisp packaged/
	tar jcfv $@ --directory=$(quicklispDir)/..  --exclude=example.bin signal-handler/
	-@chgrp tmp $@

generated/description.org: description.org
	cat $< > $@
	-@chgrp tmp $@

$(quicklispDir)/%.lisp: $(genFroms)
	cat generated/$(notdir $@) > $@
	-@chgrp tmp $@

$(quicklispDir)/%.asd: generated/from/packaging.org
	cat generated/$(notdir $@) > $@
	-@chgrp tmp $@

$(quicklispDir)/%.org: %.org
	cat $< > $@
	-@chgrp tmp $@

generated/from/%.org: %.org generated/from/
	echo `emacsclient -e "(progn (require 'version) (printangle \"$<\"))"` | sed 's/"//g' > $@
	-@chgrp tmp $@ `cat $@`

README.md: README.org
	emacsclient -e '(progn (find-file "README.org") (org-md-export-to-markdown))'
	@sed -i "s/\.md)/.org)/g"  $@
	-@chgrp tmp $@
	-@chmod a-x $@

clean:
	-rm -r $(quicklispDir) generated

.PHONY: clean quicklisp all demo

%/:
	[ -d $@ ] || mkdir -p $@

version.org: change-log.org
	emacsclient -e "(progn (require 'version) (format-version \"$<\"))" | sed 's/"//g' > $@
	@echo "← generated `date '+%m/%d %H:%M'` from [[file:$<][$<]]" >> $@
	@echo "by [[https://github.com/chalaev/lisp-goodies/blob/master/packaged/version.el][version.el]]" >> $@
	-@chgrp tmp $@
