vendors:
	test ! -d $@
	mkdir vendors
	@./source.sh

blame.hvt.target: | vendors
	@echo " BUILD blame.exe"
	@dune build --root . --profile=release ./main.exe
	@echo " DESCR blame.exe"
	@$(shell dune describe location \
		--context solo5 --no-print-directory --root . --display=quiet \
		./main.exe 1> $@ 2>&1)

blame.hvt: blame.hvt.target
	@echo " COPY blame.hvt"
	@cp $(file < blame.hvt.target) $@
	@chmod +w $@
	@echo " STRIP blame.hvt"
	@strip $@

blame.install: blame.hvt
	@echo " GEN blame.install"
	@ocaml install.ml > $@

all: blame.install | vendors

.PHONY: clean
clean:
	if [ -d vendors ] ; then rm -fr vendors ; fi
	rm -f blame.hvt.target
	rm -f blame.hvt
	rm -f blame.install

install: blame.intall
	@echo " INSTALL blame"
	opam-installer blame.install
