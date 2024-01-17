.PHONY: publish test coveralls lint zero coverage

VERSION={{VERSION}}
VERSION_DASH=`echo -n "{{VERSION}}" | sed "s/-/%E2%80%93/"`
BRANCH=`git branch | grep '^*' | sed 's/* //'`
DATE=`date -uR`
YEAR=`date +%Y`
DATE_SHORT=`date +%Y-%m-%d`
TESTS_CHECKSUM=`cat tests/test.js tests/*.scm | md5sum | cut -d' ' -f 1`
COMMIT=`git rev-parse HEAD`
URL=`git config --get remote.origin.url`
UNICODE=https://unicode.org/Public/UNIDATA/UnicodeData.txt

MAKE=make
GIT=git
CD=cd
SED=sed
CP=cp
RM=rm
TEST=test
CAT=cat
NPM=npm
NODE=node
WGET=wget
ESLINT=./node_modules/.bin/eslint
COVERALLS=./node_modules/.bin/coveralls
JEST=./node_modules/.bin/jest
MERMAID=./node_modules/.bin/mmdc
NPM=npm
UGLIFY=./node_modules/.bin/uglifyjs
ROLLUP=./node_modules/.bin/rollup
LIPS=./bin/lips.js

ALL: Makefile  package.json .$(VERSION) assets/classDiagram.svg dist/version.js dist/lips.js dist/lips.esm.js dist/lips.min.js dist/lips.esm.min.js README.md dist/std.min.scm dist/std.xcb

dist/version.js: src/lips.js
	$(CAT) src/banner.js src/lips.js > dist/version.js
	$(GIT) branch | grep '* devel' > /dev/null && $(SED) -i -e "s/{{VER}}/DEV/g" -e "s/{{DATE}}/$(DATE)/g" \
	dist/version.js || $(SED) -i -e "s/{{VER}}/$(VERSION)/g" -e "s/{{DATE}}/$(DATE)/g" -e "s/{{YEAR}}/${YEAR}/" \
	dist/version.js

dist/lips.js dist/lips.esm.js: dist/version.js .$(VERSION) rollup.config.js
	$(ROLLUP) -c

dist/lips.min.js: dist/lips.js .$(VERSION)
	$(UGLIFY) -o dist/lips.min.js --comments --mangle -- dist/lips.js

dist/lips.esm.min.js: dist/lips.esm.js .$(VERSION)
	$(UGLIFY) -o dist/lips.esm.min.js --comments --mangle -- dist/lips.esm.js

dist/std.scm: lib/bootstrap.scm lib/R5RS.scm lib/byte-vectors.scm lib/R7RS.scm lib/init.scm
	$(CAT) lib/bootstrap.scm lib/R5RS.scm lib/byte-vectors.scm lib/R7RS.scm lib/init.scm > dist/std.scm

dist/std.xcb: dist/std.scm
	$(LIPS) -t --bootstrap dist/std.scm -c -q dist/std.scm

dist/std.min.scm: dist/std.scm
	$(LIPS) -t --bootstrap dist/std.scm ./scripts/minify.scm ./dist/std.scm > dist/std.min.scm

Makefile: templates/Makefile
	$(SED) -e "s/{{VER""SION}}/"$(VERSION)"/g" templates/Makefile > Makefile

package.json: .$(VERSION)
	$(SED) -i 's/"version": "[^"]\+"/"version": "$(VERSION)"/' package.json

assets/classDiagram.svg: assets/classDiagram
	$(MERMAID) -i assets/classDiagram -o assets/classDiagram.svg

README.md: templates/README.md dist/lips.js .$(VERSION)
	$(GIT) branch | grep '* devel' > /dev/null && $(SED) -e "s/{{VER}}/DEV/g" -e \
	"s/{{VER_DASH}}/$(VERSION_DASH)/g" -e "s#{{BRANCH}}#$(BRANCH)#g" -e "s/{{CHECKSUM}}/$(TESTS_CHECKSUM)/g" \
	-e "s/{{YEAR}}/${YEAR}/g"  -e "s/{{DATE}}/${DATE_SHORT}/" -e "s/{{COMMIT}}/$(COMMIT)/g" \
	< templates/README.md > README.md || \
	$(SED) -e "s/{{VER}}/$(VERSION)/g" -e "s#{{BRANCH}}#$(BRANCH)#g" -e "s/{{YEAR}}/${YEAR}/g" \
	-e "s/{{CHECKSUM}}/$(TESTS_CHECKSUM)/g" -e "s/{{COMMIT}}/$(COMMIT)/g" -e "s/{{DATE}}/${DATE_SHORT}/" \
	-e "s/{{VER_DASH}}/$(VERSION_DASH)/g" < templates/README.md > README.md

.$(VERSION): Makefile
	touch .$(VERSION)

publish-beta:
	$(GIT) clone $(URL) --depth 1 npm
	$(CD) npm && $(NPM) publish --access=public --tag beta
	$(RM) -rf npm

publish:
	$(GIT) clone $(URL) --depth 1 npm
	$(CD) npm && $(NPM) publish --access=public
	$(RM) -rf npm

jest-test: dist/lips.js
	@$(JEST) --coverage spec/*.spec.js

test: dist/lips.js dist/std.min.scm
	@$(NPM) run test

test-file: dist/lips.js dist/std.min.scm
	@$(NPM) run test -- -- -f $(FILE)

test-update: dist/lips.js dist/std.scm
	@$(NPM) run test-update

zero:
	@$(WGET) $(UNICODE) -O ./assets/UnicodeData.txt

unicode: assets/UnicodeData.txt
	@$(NODE) ./scripts/numerals.js

watch-test:
	@inotifywait -m -e close_write src/lips.js tests/*.scm | while read even; do $(MAKE) --no-print-directory test; done

watch-lint:
	@inotifywait -m -e close_write src/lips.js | while read even; do $(MAKE) --no-print-directory lint; done

watch-make:
	@inotifywait -m -e close_write src/lips.js | while read even; do $(MAKE) --no-print-directory; done

coverage:
	$(NPM) run coverage

lint:
	$(ESLINT) src/lips.js lib/js/bookmark.js
