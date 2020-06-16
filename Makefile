all: nitta-backend nitta-frontend


configure: configure-stack configure-npm

configure-stack:
	stack setup
	stack build --only-dependencies --haddock --test

configure-npm:
	cd web && npm ci


build: build-nitta-backend build-nitta-frontend

build-nitta-backend:
  stack build
	stack build --test --haddock --copy-bins
	stack exec nitta-api-gen

build-nitta-frontend:
	cd web && npm run build

clean:
	stack clean
	rm -rf web/build
	rm a.out
	rm *.vcd


nitta: build
	if [ -z "${sim}" ]; \
	then \
		stack exec nitta -- --web examples/fibonacci.lua \
	else \
		stack exec nitta -- --web examples/${sim}.lua \
	fi;

hdl-spi: hdl/spi/bounce_filter_tb.v
	iverilog hdl/spi/bounce_filter_tb.v hdl/spi/bounce_filter.v
	test 0 -eq $(shell ./a.out | grep -c FAIL) # number of FAIL in output
