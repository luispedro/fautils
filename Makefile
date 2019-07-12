all:
	stack build

check: test

test:
	stack build --test

bench:
	mkdir -p test_data
	stack build ---bench

.PHONY: bench
