all: build

.PHONY: all build test

build: dist/setup-config tags
	@/bin/echo -e "CABAL\tbuild"
	cabal build

test: dist/setup-config tags
	@/bin/echo -e "CABAL\ttest"
	cabal test

dist/setup-config: haskell2package.cabal 
	cabal configure \
		--enable-tests \
		--disable-benchmarks \
		-v0 2>/dev/null || /bin/echo -e "CABAL\tinstall --only-dependencies" && cabal install --only-dependencies --enable-tests --disable-benchmarks
	@/bin/echo -e "CABAL\tconfigure"
	cabal configure \
		--enable-tests \
		--disable-benchmarks \
		--disable-library-profiling \
		--disable-executable-profiling


dist/build/%: dist/setup-config tags $(SOURCES)
	@/bin/echo -e "CABAL\tbuild $@"
	cabal build $(notdir $@)

SOURCES=$(shell find lib -name '*.hs' -type f) \
	$(shell find tests -name '*.hs' -type f) \
	$(shell find lib -name '*.hs' -type f)

HOTHASKTAGS=$(shell which hothasktags 2>/dev/null)
CTAGS=$(if $(HOTHASKTAGS),$(HOTHASKTAGS),/bin/false)

tags: $(SOURCES)
	if [ "$(HOTHASKTAGS)" ] ; then /bin/echo -e "CTAGS\ttags" ; fi
	-$(CTAGS) $^ > tags $(REDIRECT)

format: $(SOURCES)
	stylish-haskell -i $^

clean:
	@/bin/echo -e "CABAL\tclean"
	-cabal clean >/dev/null
	@/bin/echo -e "RM\ttemporary files"
	-rm -f tags
	-rm -f *.prof
	-rm -f lib/Package.hs

doc:
	cabal haddock

install:
	cabal install
