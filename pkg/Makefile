LS=lexicalStat
RNW=${LS}.Rnw
TEX=${LS}.tex
IDX=${LS}.idx

vignette:
	cd lexicalStat/inst/doc; \
	rm -f ${TEX}; \
	echo "library(\"utils\"); Sweave(\"${RNW}\")" | R --no-save --no-restore ; \
	pdflatex ${TEX}; \
	makeindex ${IDX}; \
	pdflatex ${TEX};

inst:
	sudo R CMD INSTALL lexicalStat 

