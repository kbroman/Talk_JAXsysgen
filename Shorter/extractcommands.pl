#!/usr/bin/perl

# extract all of the R commands from the rqtl_tour.tex file
#     and create a script so that we may test that there
#     are no typographical errors.

$ifile = "rqtltour2.tex";
$ofile = "rqtltour2.R";
open(IN, $ifile) or die("Cannot read from $ifile");
open(OUT, ">$ofile") or die("Cannot write to $ofile");

$flag = 1;
while($line = <IN>) {
    chomp($line);
    if($line =~ /% the date/) {
	@v = split(/\s+/, $line);
	$thedate = join(" ", @v[0..2]);
	print OUT ("##############################################################\n");
	print OUT ("# R code for \"A shorter tour of R/qtl\"\n");
	print OUT ("#\n# Karl W Broman, kbroman\@biostat.wisc.edu\n");
	print OUT ("# University of Wisconsin Madison\n");
	print OUT ("#\n# http://www.rqtl.org\n#\n");
	print OUT ("# $thedate\n");
	print OUT ("##############################################################\n");
	$flag = 0;
    }

#    if($line =~ /data\(/) { 
#	print OUT ("\n\n"); 
#    }

    if($line =~ /textbf\{([-\w\s,]+)\}/) {
#	@v = split(/[{}]/, $line);
	$sec = $1;
	if($sec =~ /^Diag/ or $sec =~ /^References$/) { next; }
	print OUT ("############################################################\n");
	print OUT ("# $sec\n");
	print OUT ("############################################################\n");
#	$file = "R/example" . $1 . ".R";
#	open(OUT2, ">$file") or die("Cannot write to $file");
    }
    if($line =~ /\\verb\|/ and !($line=~/^%/)) {
	$flag = 0;
	@v = split(/\|/, $line); 
	if($v[1] =~ /q\(\)/) { next; }
	print OUT ("$v[1]\n");
#	print OUT2 ("$v[1]\n");
    }
    elsif(!$flag) {
	$flag = 1;
	print OUT ("\n");
#	print OUT2 ("\n");
    }
}
close(IN);

print OUT ("# end of rqtltour2.R\n");

close(OUT);
