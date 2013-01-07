LS=lexicalStat
RNW=${LS}.Rnw
TEX=${LS}.tex
IDX=${LS}.idx

TOP=lexicalStat
SUITE=doRUnit.R
R=R

vignette:
	cd lexicalStat/inst/doc; \
	rm -f ${TEX}; \
	echo "library(\"utils\"); Sweave(\"${RNW}\")" | R --no-save --no-restore ; \
	pdflatex ${TEX}; \
	makeindex ${IDX}; \
	pdflatex ${TEX};

install:
	sudo R CMD INSTALL lexicalStat 



test: 
	cd ${TOP}/tests;\
	RCMDCHECK=FALSE ${R} --no-environ CMD BATCH ${SUITE}

clean:
	find . -name "*.Rout" -exec rm {} \;
	rm lexicalStat/inst/doc/lexicalStat.aux lexicalStat/inst/doc/lexicalStat.idx lexicalStat/inst/doc/lexicalStat.ilg lexicalStat/inst/doc/lexicalStat.ind lexicalStat/inst/doc/lexicalStat.log lexicalStat/inst/doc/lexicalStat.out lexicalStat/inst/doc/lexicalStat.pdf lexicalStat/inst/doc/lexicalStat.tex lexicalStat/inst/doc/lexicalStat.toc

