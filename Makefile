.PHONY: repos.list parse_cabal

repos.list: repos.json
	@python anchor_repos.py

parse_cabal:
	@./parse_cabal_file.sh

%.rpm: %.cabal-build-deps
	@echo "Making $@"
	@echo "Do something with $^"
	cat $^

%.cabal-build-deps: %.cabal
	@echo "Making $@"
	@./find_cabal_build_deps.sh $< > $@

clean:
	rm -f repos.list
