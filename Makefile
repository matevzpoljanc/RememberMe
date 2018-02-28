INSTALL_ARGS := $(if $(PREFIX),--prefix $(PREFIX),)

default:
	jbuilder build @install

install:
	jbuilder install $(INSTALL_ARGS)

testAll:
	jbuilder build @install
	jbuilder install $(INSTALL_ARGS)
	jbuilder build "test/test.exe"
	./_build/default/test/test.exe