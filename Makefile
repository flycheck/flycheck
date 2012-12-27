EMACS = emacs
EMACSFLAGS =

OBJECTS = flycheck.elc

.PHONY: build
build : $(OBJECTS)

.PHONY: clean
clean :
	rm -f $(OBJECTS)

%.elc : %.el
	$(EMACS) --no-site-file --no-site-lisp --batch $(EMACSFLAGS) \
		-f batch-byte-compile $<
