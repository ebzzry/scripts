.PHONY: all

all:
	cl-launch --output $$HOME/bin/multi --dump ! --lisp sbcl --quicklisp --dispatch-system cl-scripts/toggle-touchpad --system cl-scripts
	$$HOME/bin/multi help
