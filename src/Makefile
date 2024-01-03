%.o: %.f90
	gfortran -Wall -Werror -O3 -Wall -Werror -c -o $@ $<

__artifacts =

balloon-optimizer-objs	:= balloon.o constrain.o standard_atmosphere.o newton_rhapson.o
constrain.o: newton_rhapson.o
balloon.o: standard_atmosphere.o
balloon-optimizer: $(balloon-optimizer-objs)
	gfortran -O3 -Wall -Werror $(balloon-optimizer-objs) -o balloon-optimizer
__artifacts	+= balloon-optimizer

.PHONY: all
all: balloon-optimizer

.PHONY: clean
clean:
	rm -rf *.o
	rm -f $(__artifacts)
