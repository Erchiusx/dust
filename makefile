export SHELL=/bin/zsh
.PHONY: lint


define CABAL
	cabal --with-compiler=wasm32-wasi-ghc --with-hc-pkg=wasm32-wasi-ghc-pkg --with-hsc2hs=wasm32-wasi-hsc2hs $(1) $(2) $(3)
endef

define WIZER
	env -i GHCRTS=-H64m $$HOME/.ghc-wasm/wasmtime/bin/wizer \
		--allow-wasi \
		--wasm-bulk-memory true \
		--inherit-env true \
		--init-func _initialize \
		-o $(2) \
		$(1) && \
	$$HOME/.ghc-wasm/binaryen/bin/wasm-opt $(2) -o $(2); \
	$$HOME/.ghc-wasm/wasmtime/bin/wasm-tools strip -o $(2) $(2);
endef


define NVM
	NVM_DIR="$$HOME/.nvm" \
	[ -s "$$NVM_DIR/nvm.sh" ] && \. "$$NVM_DIR/nvm.sh" \
	&& nvm $(1) $(2) $(3)
endef


DIST=release
TARGET=dustify
SRC=src

main: distpath $(DIST)/ffi.mjs

$(DIST)/ffi.mjs: $(DIST)/$(TARGET).wasm
	$(call NVM, run, stable) $$(realpath $$(wasm32-wasi-ghc --print-libdir)/post-link.mjs) -i $< -o $@

distpath:
	mkdir -p release

$(DIST)/$(TARGET).wasm: $(SRC)/Dustify.hs
	$(call CABAL, build, $(notdir $(basename $@)))
	cp $$($(call CABAL, list-bin, $(notdir $(basename $@)))) $@

lint:
	fourmolu --config ./fourmolu.yaml -i src/
	cabal-fmt dust.cabal -i
