.PHONY: clean

all: report.pdf

report.blg: literature.bib
	pdflatex report.tex
	bibtex report.aux

report.pdf: report.tex report.blg makefile $(shell ls *.tex)
	pdflatex report.tex
	pdflatex report.tex

clean:
	rm -f report.aux report.bbl report.blg report.log report.out report.pdf
