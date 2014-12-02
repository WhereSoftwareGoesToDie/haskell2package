.PHONY: repos.list

repos.list: repos.json
	@python anchor_repos.py


%.rpm: %.cabal-build-deps
	@echo "Making $@"
	@echo "Do something with $^"
	@./parse_cabal_file.sh $(patsubst %.rpm,%,$@)

%.cabal-build-deps: %.cabal
	@echo "Making $@"
	@./find_cabal_build_deps.sh $< > $@

clean:
	rm -f repos.list
