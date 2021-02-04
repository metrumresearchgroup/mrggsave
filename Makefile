SHELL := /bin/bash
#LIBDIR=${HOME}/Rlibs/lib
PACKAGE=mrggsave
VERSION=$(shell grep Version DESCRIPTION |awk '{print $$2}')
TARBALL=${PACKAGE}_${VERSION}.tar.gz
PKGDIR=.
CHKDIR=.
TESTDIR=../../qualification/mrggsave_qualification/testing/

covr:
	Rscript inst/script/covr.R

spelling:
	Rscript -e 'spelling::spell_check_package(".")'

pkgdown:
	Rscript -e "pkgdown::build_site()"

testing:
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
	Rscript -e 'testthat::test_dir("tests")'

bump-dev:
	Rscript -e 'usethis::use_version("dev")'

tag-version:
	git tag $(VERSION)
	git push origin $(VERSION)

