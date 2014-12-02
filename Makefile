.PHONY: repos.list

# Testing for now
borel.rpm:

repos.list:
	@python anchor_repos.py | sort > $@


%.rpm: %.cabal-build-deps
	@#echo "Making $@"
	@#echo "Do something with $^"
	@./parse_cabal_file.sh $(patsubst %.rpm,%,$@)

%.cabal-build-deps: %.cabal
	@#echo "Making $@"
	@./find_cabal_build_deps.sh $< > $@

clean:
	rm -f repos.list
