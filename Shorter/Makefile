all: rqtltour2.pdf rqtltour2.R

rqtltour2.pdf: rqtltour2.tex
	latex rqtltour2.tex
	latex rqtltour2.tex
	dvipdf rqtltour2.dvi rqtltour2.pdf
	\rm *.dvi *.aux *.log rqtltour2.out

rqtltour2.R: rqtltour2.tex extractcommands.pl
	extractcommands.pl

test: rqtltour2.tex rqtltour2.R
	nice R CMD BATCH rqtltour2.R output.txt &

clean:
	\rm *~ *.bak output.txt Rplots.ps