# do not delete these intermediate files (thus ignoring implicit rules):
.PRECIOUS: generated/from/%.org generated/%.lisp generated/from generated/from/make

# Uncomment for debugging:
# OLD_SHELL := $(SHELL)
# SHELL = $(warning Building $@$(if $<, (from $<))$(if $?, ($? newer)))$(OLD_SHELL)

SBCL = ~/local/bin/sbcl
# where my local packages are stored:
quicklispDir = $$HOME/quicklisp/local-projects/signal-handler
bareLispFNs = def-example def-SH example macros signal-handler
QLlispFNs = $(addprefix $(quicklispDir)/, version.org description.org signal-handler.asd $(addsuffix .lisp, $(bareLispFNs)))

quicklisp: $(QLlispFNs)
	@echo "to test the binary: make demo"

$(quicklispDir)/%.asd: generated/from/package.org $(quicklispDir)/
	cat generated/$(notdir $@) > $@
	-@chgrp tmp $@

$(quicklispDir)/%.lisp: generated/%.lisp $(quicklispDir)/
	cat $< > $@
	-@chgrp tmp $@

generated/%.lisp: generated/from/%.org
	-@chgrp tmp $@

generated/from/%.org: %.org generated/from/
	echo `emacsclient -e "(progn (require 'version) (printangle \"$<\"))"` | sed 's/"//g' > $@
	-@chgrp tmp $@ `cat $@`

$(quicklispDir)/%.org: %.org $(quicklispDir)/
	cat $< > $@
	-@chgrp tmp $@

version.org: change-log.org
	emacsclient -e "(progn (require 'version) (format-version \"$<\"))" | sed 's/"//g' > $@
	@echo "← generated `date '+%m/%d %H:%M'` from [[file:$<][$<]]" >> $@
	@echo "by [[https://github.com/chalaev/lisp-goodies/blob/master/packaged/version.el][version.el]]" >> $@
	-@chgrp tmp $@

#################### pre-07/17 code →

demo: $(quicklispDir)/example.bin
	-[ -f /tmp/sbcl.lock/pid ] && awk '{print $$1}' /tmp/sbcl.lock/pid | xargs kill -9
	-rm -r /tmp/sbcl.lock
	@$(quicklispDir)/example.bin & echo "Makefile--> PID=$$!"
	@echo "\n*** will launch the DEMO now ***\n"
	generated/tell

$(quicklispDir)/example.bin: quicklisp
	@echo "*** COMPILING THE BINARY ***"
	$(SBCL) --quit --eval "(progn (asdf:load-system :signal-handler) (asdf:make :signal-handler/example))" 2> generated/example.bin.2.log > generated/example.bin.1.log
	@echo "\n*** COMPILED THE BINARY ***\n"
	-@chgrp tmp $@

packaged/signal-handler.tbz: quicklisp packaged/
	tar jcfv $@ --directory=$(quicklispDir)/..  --exclude=example.bin signal-handler/
	-@chgrp tmp $@

README.md: README.org
	emacsclient -e '(progn (find-file "README.org") (org-md-export-to-markdown))'
	@sed -i "s/\.md)/.org)/g"  $@
	-@chgrp tmp $@
	-@chmod a-x $@

clean:
	-$(SBCL) --quit --eval '(asdf:clear-system :signal-handler)'
	-rm -r $(quicklispDir) generated version.org $$HOME/.cache/common-lisp/sbcl-*$(quicklispDir)

.PHONY: clean

%/:
	mkdir -p $@

