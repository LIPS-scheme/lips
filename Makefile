.PHONY: publish test coveralls lint zero coverage codespell

VERSION=1.0.0-beta.19
VERSION_DASH=`echo -n "1.0.0-beta.19" | sed "s/-/%E2%80%93/"`
BRANCH=`git branch | grep '^*' | sed 's/* //'`
DATE=`date -uR`
YEAR=`date +%Y`
DATE_SHORT=`date +%Y-%m-%d`
TESTS_CHECKSUM=`cat tests/test.js tests/*.scm | md5sum | cut -d' ' -f 1`
COMMIT=`git rev-parse HEAD`
URL=`git config --get remote.origin.url`
UNICODE_ALL=https://unicode.org/Public/UNIDATA/UnicodeData.txt
UNICODE_FOLD=https://www.unicode.org/Public/UCD/latest/ucd/CaseFolding.txt
WORK_TREE=`git worktree list | cut -f1 -d' ' | grep -v "\`pwd\`$$" | xargs -I{} basename {} | tr $$'\n' ',' | sed 's/,$$//'`

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
CODESPELL=codespell
ESLINT=./node_modules/.bin/eslint
COVERALLS=./node_modules/.bin/coveralls
JEST=./node_modules/.bin/jest
MERMAID=./node_modules/.bin/mmdc
NPM=npm
UGLIFY=./node_modules/.bin/uglifyjs
ROLLUP=./node_modules/.bin/rollup
LIPS=./bin/lips.js

define ver_date
	$(GIT) branch | grep '* master' > /dev/null && $(SED) -i -e "s/{{VER}}/$(VERSION)/g" -e "s/{{DATE}}/$(DATE)/g" \
	-e "s/{{YEAR}}/${YEAR}/" $(1) || $(SED) -i -e "s/{{VER}}/DEV/g" -e "s/{{DATE}}/$(DATE)/g" $(1)
endef

ALL: Makefile package.json .$(VERSION) assets/classDiagram.svg dist/base.js dist/lips.js dist/lips.esm.js dist/lips.min.js dist/lips.esm.min.js README.md dist/std.min.scm dist/std.xcb docs/reference.json

dist/banner.js: src/banner.js src/lips.js .$(VERSION)
	$(CP) src/banner.js dist/banner.js
	$(call ver_date,dist/banner.js)

dist/base.js: src/lips.js .$(VERSION)
	$(SED) '/\/\*\*@license/,/\*\//d' < src/lips.js > dist/base.js
	$(call ver_date,dist/base.js)

dist/lips.js dist/lips.esm.js dist/lips.cjs: dist/banner.js dist/base.js .$(VERSION) rollup.config.js
	$(ROLLUP) -c

dist/lips.min.js: dist/lips.js .$(VERSION)
	$(UGLIFY) -o dist/lips.min.js --comments --mangle -- dist/lips.js

dist/lips.esm.min.js: dist/lips.esm.js .$(VERSION)
	$(UGLIFY) -o dist/lips.esm.min.js --comments --mangle -- dist/lips.esm.js

dist/std.scm: lib/bootstrap.scm lib/R5RS.scm lib/byte-vectors.scm lib/R7RS.scm lib/init.scm
	$(CAT) lib/bootstrap.scm lib/R5RS.scm lib/byte-vectors.scm lib/R7RS.scm lib/init.scm > dist/std.scm

dist/std.xcb: dist/std.scm
	$(LIPS) -t --bootstrap dist/std.scm -c -q dist/std.scm

docs/reference.json: dist/std.xcb src/lips.js
	$(NODE) ./scripts/reference.js > docs/reference.json

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

test: dist/lips.js dist/std.xcb
	@$(NPM) run test

test-file: dist/lips.js dist/std.xcb
	@$(NPM) run test -- -- -f $(FILE)

test-update: dist/lips.js dist/std.scm
	@$(NPM) run test-update

fold:
	@$(WGET) $(UNICODE_FOLD) -O ./assets/CaseFolding.txt

zero:
	@$(WGET) $(UNICODE_ALL) -O ./assets/UnicodeData.txt

watch-test:
	@inotifywait -m -e close_write src/lips.js tests/*.scm | while read even; do $(MAKE) --no-print-directory test; done

watch-lint:
	@inotifywait -m -e close_write src/lips.js | while read even; do $(MAKE) --no-print-directory lint; done

watch-make:
	@inotifywait -m -e close_write src/lips.js | while read even; do $(MAKE) --no-print-directory; done

coverage:
	$(NPM) run coverage

codespell:
	@$(CODESPELL) -S $(WORK_TREE)

lint:
	$(ESLINT) src/lips.js lib/js/bookmark.js bin/lips.js
