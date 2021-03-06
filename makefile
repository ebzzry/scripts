NAME=scripts
DEST=$(HOME)/bin
BINARY=$(DEST)/$(NAME)
SCRIPT=$(PWD)/$(NAME)
CL=cl-launch

.PHONY: all $(NAME) clean

all: $(NAME)

$(NAME):
	@$(CL) --output $(NAME) --dump ! --lisp sbcl \
	--quicklisp --dispatch-system $(NAME)/main --system $(NAME)

install: $(NAME)
	@ln -sf $(SCRIPT) $(BINARY)
	@$(SCRIPT) create-symlinks $(BINARY)

clean:
	@rm -f $(NAME)
