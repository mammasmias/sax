all: users-guide.ps

clean:
	rm -f users-guide.aux users-guide.bbl users-guide.blg users-guide.dvi users-guide.end users-guide.log users-guide.ps

users-guide.dvi: users-guide.tex sax.bib
	latex users-guide
	bibtex users-guide
	latex users-guide
	latex users-guide

users-guide.ps: users-guide.dvi
	dvips users-guide -o users-guide.ps

users-guide.pdf: users-guide.ps
	ps2pdf users-guide.ps


