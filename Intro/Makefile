all: intro.pdf intro_2up.pdf

intro.pdf: intro.tex
	pdflatex intro

intro_bw.pdf: intro_bw.tex
	pdflatex intro_bw

intro_2up.pdf: intro_bw.pdf
	pdfnup intro_bw.pdf --nup 1x2 --no-landscape --paper letterpaper
	mv intro_bw-nup.pdf intro_2up.pdf 


