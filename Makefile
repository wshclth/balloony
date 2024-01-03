FC 	?= gfortran

export $(FC)

.PHONY: all
all:
	$(MAKE) -C src/ $(MAKECMDGOALS)

.PHONY: all
clean:
	find . -name "*.mod" | xargs rm || true
	$(MAKE) -C src/ $(MAKECMDGOALS)
