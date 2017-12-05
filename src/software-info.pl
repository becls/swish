#!/usr/bin/perl

if ($#ARGV < 0)
{
    print "Usage: $0 fieldname\n\n";
    exit(-1);
}

$fieldname = $ARGV[0];

open INPUT, "software-info.ss";
@orig = <INPUT>;
$orig = join "", @orig;
close INPUT;

$_ = $orig;
if ( /\(define\s+$fieldname\s+\"([^\"]*)\"\)/ )
{
    print "$1\n";
    exit(0);
} else {
    print "ERROR: $fieldname not found in $filename\n";
    exit(1);
}
