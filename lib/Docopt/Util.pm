package Docopt::Util;
use strict;
use warnings;
use utf8;
use parent qw(Exporter);

our @EXPORT_OK = qw(repl class_name string_strip string_partition in);

use Data::Dumper; # serializer
use Scalar::Util ();

sub in {
    my ($val, $patterns) = @_;
    for (@$patterns) {
        if (defined $_) {
            return 1 if $val eq $_;
        } else {
            return 1 if not defined $val;
        }
    }
    return 0;
}

sub repl($) {
    my ($val) = @_;
    if (Scalar::Util::blessed($val) && $val->can('__repl__')) {
        $val->__repl__;
    } else {
        local $Data::Dumper::Terse=1;
        local $Data::Dumper::Indent=0;
        Dumper($val)
    }
}

sub class_name {
    my $name = ref $_[0] || $_[0];
    $name =~ s/^Docopt:://;
    $name;
}

sub string_strip($) {
    local $_ = shift;
    s/^\s+//;
    s/\s+$//;
    $_;
}

sub string_partition($$) {
    my ($str, $sep) = @_;
    if ($str =~ /\A(.*?)$sep(.*)\z/) {
        return ($1, $sep, $2);
    } else {
        return ($str, '', '');
    }
}

1;

