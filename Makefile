
build:
	@cabal install

docs:
	@cabal haddock --executables --haddock-options="--ignore-all-exports"

deploy:
	@cabal install
	@./scripts/svd_ctl.sh restart antiblog

install:
	cabal install
	rm -f antiblog
	rm -f antisync
	ln -s .cabal-sandbox/bin/antiblog ./antiblog
	ln -s .cabal-sandbox/bin/antisync ./antisync
	cat schema/tables_v1.sql | schema/cli.sh
	cat schema/views.sql | ./schema/cli.sh
	cat schema/functions.sql | ./schema/cli.sh
	./scripts/svd_check.sh
	./scripts/svd_ctl.sh restart antiblog

