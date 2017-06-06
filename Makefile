HAS_ELIXIR=1

include bu.mk

script:
	$(verbose) $(REBAR) as escriptize escriptize

distclean::
	$(verbose) $(RM_RF) doc

changelog:
	$(verbose) github_changelog_generator
