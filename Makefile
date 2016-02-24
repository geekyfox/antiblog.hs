
.PHONY: build
build: antiwork/stamp antiblog/stamp antisync/stamp

.PHONY: anticore antihost
anticore: anticore/stamp
antihost: antihost/stamp

anticore/stamp: anticore/src/Anticore/**/*hs
	cd anticore && cabal install
	touch anticore/stamp

antihost/stamp: anticore/stamp antihost/src/Antihost/* antihost/src/*
	cd antihost && cabal install
	touch antihost/stamp

antiwork/stamp: anticore/stamp antihost/stamp antiwork/src/Antiwork/*
	cd antiwork && cabal install
	touch antiwork/stamp

antiblog/stamp: anticore/stamp antihost/stamp antiblog/src/Antiblog/*
	cd antiblog && cabal install
	touch antiblog/stamp

antisync/stamp: anticore/stamp antisync/src/Antisync/*
	cd antisync && cabal install
	touch antisync/stamp

clean-libs:
	cabal sandbox hc-pkg unregister antihost-0.1.0.1
	rm -f antihost/stamp
	cabal sandbox hc-pkg unregister anticore-0.1.0.1
	rm -f anticore/stamp

lint:
	@hlint anticore/src antihost/src antiblog/src antisync/src

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

