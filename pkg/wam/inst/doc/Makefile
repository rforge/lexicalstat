LS=wam
RNW=${LS}.Rnw
TEX=${LS}.tex
IDX=${LS}.idx

vignette:
	echo "library(\"utils\"); Sweave(\"${RNW}\")" | R --no-save --no-restore ; \
	pdflatex ${TEX}; \
	makeindex ${IDX}; \
	pdflatex ${TEX};

