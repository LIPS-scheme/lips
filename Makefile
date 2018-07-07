.PHONY: publish test coveralls lint

VERSION=0.5.0
BRANCH=`git branch | grep '^*' | sed 's/* //'`
DATE=`date -uR`
SPEC_CHECKSUM=`md5sum spec/lips.spec.js | cut -d' ' -f 1`
COMMIT=`git rev-parse HEAD`

GIT=git
SED=sed
RM=rm
TEST=test
CAT=cat
NPM=npm
ESLINT=./node_modules/.bin/eslint
COVERALLS=./node_modules/.bin/coveralls
JEST=./node_modules/.bin/jest
UGLIFY=./node_modules/.bin/uglifyjs
BABEL=./node_modules/.bin/babel


ALL: Makefile .$(VERSION) dist/lips.js dist/lips.min.js README.md package.json

dist/lips.js: src/lips.js .$(VERSION)
	$(GIT) branch | grep '* devel' > /dev/null && $(SED) -e "s/{{VER}}/DEV/g" -e "s/{{DATE}}/$(DATE)/g" src/lips.js > dist/lips.tmp.js || $(SED) -e "s/{{VER}}/$(VERSION)/g" -e "s/{{DATE}}/$(DATE)/g" src/lips.js > dist/lips.tmp.js
	$(BABEL) dist/lips.tmp.js > dist/lips.js
	$(RM) dist/lips.tmp.js

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
	$(NPM) publish --access=public

test:
	$(JEST)

coveralls:
	$(CAT) ./coverage/lcov.info | $(COVERALLS)

lint:
	$(ESLINT) src/lips.js spec/lips.spec.js
