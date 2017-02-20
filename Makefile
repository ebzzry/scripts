.PHONY: all multi symlinks

BINARY=$$HOME/bin/multi

multi:
	cl-launch --output $(BINARY) --dump ! --lisp sbcl --quicklisp --dispatch-system cl-scripts/touchpad --system cl-scripts

symlinks: multi
	$(BINARY) create-symlinks

all: multi symlinks
