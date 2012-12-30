TOP=../..
PKG=${shell cd ${TOP};pwd}
SUITE=doRUnit.R
R=R

all: test

# Install package
inst: 
	cd ${TOP}/..;\
	${R} CMD INSTALL ${PKG}

# Run unit tests
test: 
	cd ${TOP}/tests;\
	RCMDCHECK=FALSE ${R} --no-environ CMD BATCH ${SUITE}
