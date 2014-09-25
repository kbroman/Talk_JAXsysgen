all: hk.pdf hk_2up.pdf

hk.pdf: hk.tex
	pdflatex hk

hk_bw.pdf: hk_bw.tex
	pdflatex hk_bw

hk_2up.pdf: hk_bw.pdf
	pdfnup hk_bw.pdf --nup 1x2 --no-landscape --paper letterpaper
	mv hk_bw-nup.pdf hk_2up.pdf 


