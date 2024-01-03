%.o: %.f90
	gfortran -Wall -Werror -O3 -Wall -Werror -c -o $@ $<

__artifacts =

balloon-optimizer-objs	:= balloon.o constrain.o standard_atmosphere.o
balloon.o: standard_atmosphere.o
balloon-optimizer: $(balloon-optimizer-objs)
	gfortran -O3 -Wall -Werror balloon.o constrain.o standard_atmosphere.o -o balloon-optimizer
__artifacts	+= balloon.o constrain.o standard_atmosphere.o
__artifacts	+= balloon-optimizer

.PHONY: all
all: balloon-optimizer

.PHONY: clean
clean:
	rm -f $(__artifacts)
