all: topics.pdf topics_2up.pdf

topics.pdf: topics.tex
	pdflatex topics

topics_bw.pdf: topics_bw.tex
	pdflatex topics_bw

topics_2up.pdf: topics_bw.pdf
	pdfnup topics_bw.pdf --nup 1x2 --no-landscape --paper letterpaper
	mv topics_bw-nup.pdf topics_2up.pdf
