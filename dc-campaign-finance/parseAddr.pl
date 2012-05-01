#!/usr/bin/perl

use Geo::StreetAddress::US;
use Text::Trim;

if($#ARGV < 2) {
    print "Usage: perl parseAddr.pl [/path/to/address_file with id column] [/path/to/newfile] [tab|csv|pipe]\n";
    print "Example: perl parseAddr.pl my_original_file.txt my_new_file.txt tab\n";
    exit;
}
my %d = ('tab' => "\t",
         'csv' => ",",
         'pipe' => "|");
if(!-e $ARGV[0]) { die "ERROR: $ARGV[0] does not exist! Stopped, "; }
if($ARGV[2] eq '') { die "ERROR: delimiter is empty! Stopped, "; }
elsif (!exists $d{$ARGV[2]}) { die "ERROR: delimiter is of bad type $ARGV[2]\n"; } 
my $delim = $d{$ARGV[2]};
open(IN, $ARGV[0]) or die "Cannot open $ARGV[0]- $!, stopped ";
open(OUT, ">$ARGV[1]") or die "$!, stopped ";

my $prefix = "perl_parsed";
my $header = <IN>;
$header =~ s/[\r\n]+//g;
my @order = ('number','prefix','street','type','suffix','sec_unit_type','sec_unit_num','city','state','zip');
foreach my $elem (@order) {
    $header .= $delim. "$prefix\_$elem";
}

print OUT "$header" . $delim . "$prefix\_combined\n";
my $i = 0;
while (my $line = <IN>) {
	$i++;
    if($i % 1000 ) { print "Parsing record $i\n"; }
    $line =~ s/[\r\n]+//g;
    (my $id,$line) = split($delim,$line);
    my $outline = '';
    my $combined = '';
    my $addr = Geo::StreetAddress::US->parse_location($line);
    foreach my $field (@order) {
		if(exists $addr->{$field} && defined $addr->{$field}) {
	    	my $elem = ($addr->{$field} ne '') ? trim($addr->{$field}) : '';
	    	$outline .= $delim . $elem;
	    	$combined .= ($addr->{$field} ne '') ? trim($addr->{$field}) . " " : '';
		}
		else {		
			$outline .= $delim;
		}
    }  
    chop($combined);
    print OUT $id . $delim . $line . uc($outline) . $delim . uc($combined) . "\n";	
}
