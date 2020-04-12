.PHONY: publish test coveralls lint

VERSION={{VERSION}}
BRANCH=`git branch | grep '^*' | sed 's/* //'`
DATE=`date -uR`
SPEC_CHECKSUM=`md5sum spec/lips.spec.js | cut -d' ' -f 1`
COMMIT=`git rev-parse HEAD`
URL=`git config --get remote.origin.url`

GIT=git
CD=cd
SED=sed
CP=cp
RM=rm
TEST=test
CAT=cat
NPM=npm
ESLINT=./node_modules/.bin/eslint
COVERALLS=./node_modules/.bin/coveralls
JEST=./node_modules/.bin/jest
NPM=npm
UGLIFY=./node_modules/.bin/uglifyjs
ROLLUP=./node_modules/.bin/rollup

ALL: Makefile  package.json .$(VERSION) dist/lips.js dist/lips.min.js README.md

dist/lips.js: src/lips.js .$(VERSION) rollup.config.js
	$(ROLLUP) -c
	$(SED) -i '/^\s*\/\*\*/,/^\s*\*\//d' dist/lips.js
	$(CAT) src/banner.js dist/lips.js > dist/tmp.js
	$(CP) dist/tmp.js dist/lips.js
	$(RM) dist/tmp.js
	$(GIT) branch | grep '* devel' > /dev/null && $(SED) -i -e "s/{{VER}}/DEV/g" -e "s/{{DATE}}/$(DATE)/g" \
	dist/lips.js || $(SED) -i -e "s/{{VER}}/$(VERSION)/g" -e "s/{{DATE}}/$(DATE)/g" \
	dist/lips.js

dist/lips.min.js: dist/lips.js .$(VERSION)
	$(UGLIFY) -o dist/lips.min.js --comments --mangle -- dist/lips.js

Makefile: templates/Makefile
	$(SED) -e "s/{{VER""SION}}/"$(VERSION)"/" templates/Makefile > Makefile

package.json: templates/package.json .$(VERSION)
	$(SED) -e "s/{{VER}}/"$(VERSION)"/" templates/package.json > package.json || true

README.md: templates/README.md .$(VERSION)
	$(GIT) branch | grep '* devel' > /dev/null && $(SED) -e "s/{{VER}}/DEV/g" -e \
	"s/{{BRANCH}}/$(BRANCH)/g" -e "s/{{CHECKSUM}}/$(SPEC_CHECKSUM)/g" \
	-e "s/{{COMMIT}}/$(COMMIT)/g" < templates/README.md > README.md || \
	$(SED) -e "s/{{VER}}/$(VERSION)/g" -e "s/{{BRANCH}}/$(BRANCH)/g" -e \
	"s/{{CHECKSUM}}/$(SPEC_CHECKSUM)/g" -e "s/{{COMMIT}}/$(COMMIT)/g" < templates/README.md > README.md

.$(VERSION): Makefile
	touch .$(VERSION)

publish:
	$(GIT) clone $(URL) --depth 1 npm
	$(CD) npm && $(NPM) publish --access=public
	$(RM) -rf npm

test: dist/lips.js scheme-tests
	@$(JEST) --coverage spec/*.spec.js

scheme-tests:
	@$(NPM) run test

coveralls:
	$(CAT) ./coverage/lcov.info | $(COVERALLS)

lint:
	$(ESLINT) src/lips.js spec/lips.spec.js
