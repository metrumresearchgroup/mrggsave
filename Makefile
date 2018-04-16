SHELL := /bin/bash
#LIBDIR=${HOME}/Rlibs/lib
PACKAGE=mrggsave
VERSION=$(shell grep Version DESCRIPTION |awk '{print $$2}')
TARBALL=${PACKAGE}_${VERSION}.tar.gz
PKGDIR=.
CHKDIR=.
TESTDIR=../../software-qualification/mrggsave_qualification/testing/

move-testing:
	cp ${TARBALL} ${TESTDIR}

release:
	make all
	cp ${TARBALL} ~/ghe/software-qualification/mrggsave_qualification/testing/

cran:
	make doc
	make build
	R CMD CHECK --as-cran ${TARBALL} -o ${CHKDIR}

travis_build:
	make doc
	make build
	make install

readme:
	Rscript -e 'rmarkdown::render("README.Rmd")'

all:
	make doc
	make build
	make install


.PHONY: doc
doc:
	Rscript -e 'devtools::document(".")'

build:
	R CMD build --md5 $(PKGDIR)

install:
	R CMD INSTALL --install-tests ${TARBALL} -l ~/Rlibs

install-build:
	R CMD INSTALL --build --install-tests ${TARBALL}

check:
	make doc
	make build
	R CMD check ${TARBALL} -o ${CHKDIR}

test:
	make install
	Rscript -e 'testthat::test_dir("tests")'


