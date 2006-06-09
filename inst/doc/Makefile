all:	pdf clean

pdf:	twoChannels.tex cellhts.tex
	pdflatex cellhts
	bibtex cellhts
	pdflatex cellhts
	pdflatex cellhts
	pdflatex twoChannels
	bibtex twoChannels
	pdflatex twoChannels
	pdflatex twoChannels

clean:
	rm -f *.out *.bbl *.log *.aux *.blg *.brf *.toc *.tex
	rm -f *.dvi *.ps cellhts-* twoChannels-* tmp_*
	rm -rf raw KcViab logRatio KcViab.rda  DualChannelScreen.rda


