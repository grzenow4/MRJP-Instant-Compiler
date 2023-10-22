BUILD_DIR = build
JVM_DIR = $(BUILD_DIR)/jvm
LLVM_DIR = $(BUILD_DIR)/llvm
BINARIES = ../insc_jvm ../insc_llvm

main:
	ghc --make -outputdir $(JVM_DIR) -o ../insc_jvm JVMMain.hs
	ghc --make -outputdir $(LLVM_DIR) -o ../insc_llvm LLVMMain.hs

clean:
	rm -rf $(BUILD_DIR)

distclean:
	rm -rf $(BUILD_DIR) $(BINARIES)
