#!/usr/bin/perl -w

# Scrape information on primary campaign committees from DC OCF website and
# produce tab-separated file

# Copyright (c) 2012 Keith C. Ivey
#   
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights 
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#   
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
#
# Contact: keith at iveys dot org

use strict;
use ScraperBot;

my $bot = ScraperBot->new(
    stack_depth => 1,
    cookie_file => 'lwp_cookies.txt',
    cache_time => '48 hours',
);

my %column_map = (
    # 'Name of the Candidate' => 'candidate',
    'Date of Organization' => 'organization_date',
    # 'Name of Committee' => 'committee',
    'Party Affiliation' => 'party',
    'Office Sought' => 'office',
);

my @columns = qw(year committee candidate office party organization_date url);
open OUT, "> committees.tsv" or die "Can't open committees.tsv: $!";

my $base_url = 'http://ocf.dc.gov/registration_statements/pcc/' .
    'pcc_searchresult.asp?ftype=PCC';
my $grand_total = 0;
print OUT join("\t", @columns), "\n";
for my $year (2000 .. 2012) {
    print "$year \n";
    my $page = 1;
    my($num_pages, $total_records);
    my $records_found = 0;
    while (1) {
        my $url = "$base_url&ele_year=$year&whichpage=$page";
        $bot->get($url);
        if ($bot->content =~
            /Page (\d+) of (\d+)(?:&nbsp;)+Total Records : (\d+)/) {
            if ($num_pages) {
                if ($1 != $page or $2 != $num_pages or $3 != $total_records) {
                    die "Unexpected numbers ($1, $2, $3) for $url\n";
                }
            }
            else {
                $num_pages = $2;
                $total_records = $3;
                print "  $num_pages pages, $total_records records\n";
            }
        }
        elsif ($bot->content =~ /Your\s+search returned no results\./) {
            print "  no records\n";
            $num_pages = $total_records = 0;
            last;
        }
        else {
            die "Unexpected first page for $url\n";
        }
        my $form = $bot->tree->look_down(_tag => 'form',
            name => 'pcc_searchresult')->clone();
        my @rows = $form->look_down(_tag => 'table')->content_list();
        for my $row (@rows) {
            next unless defined $row->attr('bgcolor');
            my @cells = $row->content_list();
            my %r = (year => $year);
            $r{candidate} = $cells[0]->as_trimmed_text();
            $r{committee} = $cells[1]->as_trimmed_text();
            $r{url} = $cells[0]->look_down(_tag => 'a')->attr('href');
            $records_found++;
            next if $r{committee} eq 'N/A';
            $bot->get($r{url});
            my @subrows = $bot->tree
                ->look_down(_tag => 'form', name => 'CANCOM_summ')
                ->look_down(_tag => 'tr');
            for my $subrow (@subrows) {
                my @subcells = $subrow->content_list();
                next unless @subcells == 2 and
                    $subcells[0]->attr('width') eq '25%' and
                    $subcells[1]->attr('width') eq '75%';
                my $label = $subcells[0]->as_trimmed_text();
                my $value = $subcells[1]->as_trimmed_text();
                my $column = $column_map{$label} or next;
                if ($column =~ /_date$/) {
                    $value =~ s{^(\d\d?)/(\d\d?)/(\d{4})}
                    { sprintf '%d-%02d-%02d', $3, $1, $2 }ge;
                }
                $r{$column} = $value;
            }
            print OUT join("\t", @r{@columns}), "\n";
            #use Data::Dumper; print Dumper \%r; exit();
        }
        last if ++$page > $num_pages;
    }
    if ($records_found != $total_records) {
        die "$records_found records found -- should be $total_records\n";
    }
    $grand_total += $total_records;
}
print "TOTAL $grand_total records\n";
