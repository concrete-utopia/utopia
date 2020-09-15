REPORTER = dot

build:
	@./node_modules/.bin/coffee -b -l -o lib src/*.litcoffee

test: build
	@NODE_ENV=test ./node_modules/.bin/mocha --compilers coffee:coffee-script/register \
		--reporter $(REPORTER)

.PHONY: test
