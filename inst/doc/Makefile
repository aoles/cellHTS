all:	pdf clean

pdf:	twoChannels.tex cellhts.tex twoWay.tex
	pdflatex cellhts
	bibtex cellhts
	pdflatex cellhts
	pdflatex cellhts
	pdflatex twoChannels
	bibtex twoChannels
	pdflatex twoChannels
	pdflatex twoChannels
	pdflatex twoWay
	bibtex twoWay
	pdflatex twoWay
	pdflatex twoWay

clean:
	rm -f *.out *.bbl *.log *.aux *.blg *.brf *.toc *.tex
	rm -f *.dvi *.ps cellhts-* twoChannels-* twoWay-* tmp_*
	rm -rf raw KcViab logRatio 2Wraw 2Wnormalized KcViab.rda  DualChannelScreen.rda TwoWayAssays.rda


