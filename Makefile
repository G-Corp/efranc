HAS_ELIXIR=1

include bu.mk

script: ## Generate command line tool
	$(verbose) $(REBAR) as escriptize escriptize

distclean::
	$(verbose) $(RM_RF) doc

changelog: ## Generate CHANGELOG
	$(verbose) github_changelog_generator

release: dist lint tag ## Tag and release to hex.pm
	$(verbose) $(REBAR) hex publish

