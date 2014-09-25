all: multiqtl.pdf multiqtl_2up.pdf

multiqtl.pdf: multiqtl.tex
	pdflatex multiqtl

multiqtl_bw.pdf: multiqtl_bw.tex
	pdflatex multiqtl_bw

multiqtl_2up.pdf: multiqtl_bw.pdf
	pdfnup multiqtl_bw.pdf --nup 1x2 --no-landscape --paper letterpaper
	mv multiqtl_bw-nup.pdf multiqtl_2up.pdf 


