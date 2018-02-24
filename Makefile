.PHONY: publish test coveralls lint

ESLINT=./node_modules/.bin/eslint
COVERALLS=./node_modules/.bin/coveralls
JEST=./node_modules/.bin/jest

publish:
	npm publish --access=public

test:
	$(JEST)

coveralls:
	cat ./coverage/lcov.info | $(COVERALLS)

lint:
	$(ESLINT) index.js spec/lips.spec.js
