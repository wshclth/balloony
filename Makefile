FC 	?= gfortran
FFLAGS	:= -O3 -Wall -Wextra -Wc-binding-type -Werror -Waliasing -pedantic-errors $(FFLAGS)

export FC $(FC)
export FFLAGS $(FFLAGS)

.PHONY: all
all:
	$(MAKE) -C src/ $(MAKECMDGOALS)

.PHONY: all
clean:
	find . -name "*.mod" | xargs rm || true
	$(MAKE) -C src/ $(MAKECMDGOALS)
