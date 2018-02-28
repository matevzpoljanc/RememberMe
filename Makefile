INSTALL_ARGS := $(if $(PREFIX),--prefix $(PREFIX),)

default:
	jbuilder build @install

install:
	jbuilder install $(INSTALL_ARGS)

test:
	jbuilder build "test/test.exe"