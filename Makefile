
build: antiblog/stamp antisync/stamp

anticore/stamp: anticore/src/Anticore/*
	cd anticore && cabal install
	touch anticore/stamp

antiblog/stamp: anticore/stamp antiblog/src/Antiblog/*
	cd antiblog && cabal install
	touch antiblog/stamp

antisync/stamp: anticore/stamp antisync/src/Antisync/*
	cd antisync && cabal install
	touch antisync/stamp

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

