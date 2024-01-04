%.o: %.f90
	$(FC) $(FFLAGS) -c -o $@ $<

__artifacts =

OBJS	:= balloon.o constrain.o standard_atmosphere.o newton_rhapson.o
constrain.o: newton_rhapson.o
balloon.o: standard_atmosphere.o
optimizer: $(OBJS)
	$(FC) $(FFLAGS) $(OBJS) -o optimizer
__artifacts	+= optimizer

.PHONY: all
all: optimizer

.PHONY: clean
clean:
	rm -rf *.o
	rm -f $(__artifacts)
