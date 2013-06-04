package Docopt;
use 5.008005;
use strict;
use warnings;
use Pod::Simple;

our $VERSION = "0.01";

sub docopt {
    my %args = @_;

    my $parser = Docopt::Parser->new();
    $parser->run();
}

package Docopt::Parser;

sub new {
    my $class = shift;
    my %args = @_==1 ? %{$_[0]} : @_;
    bless {
        positional => [],
        %args
    }, $class;
}

sub _defined_or {
    my ($a, $b) = @_;
    defined($a) ? $a : $b;
}

sub parse {
    my ($self, $src) = @_;
    $src =~ s/^.*Usage://i;
    for my $line (split /\n/, $src) {
        if ($line eq '') {
            last; # finished.
        } else {
            $line =~ s/^
                \s*
                \S+ # program name
                \s+
            //x or next;

            # <argument> ARGUMENT
            while ($line) {
                $line =~ s/^\s+// and next;

                $line =~ s/^(?:
                    <([a-z0-9]+)>
                    | ([A-Z]+)
                )/
                    push @{$self->{positional}}, _defined_or($1, $2);
                    ''
                /xe and next;
            }
        }
    }
}

sub evaluate {
    my ($self, $args) = @_;
    my $result = +{};
    for (my $i=0; $i<@$args; $i++) {
        if (@{$self->{positional}}) {
            my $name = shift @{$self->{positional}};
            $result->{$name} = $args->[$i];
        }
    }
    return $result;
}

1;
__END__

=encoding utf-8

=head1 NAME

Docopt - It's new $module

=head1 SYNOPSIS

    use Docopt;

    my $opts = docopt();

    __END__

    =head1 SYNOPSIS

        Usage: $ my-cmd [foo]

=head1 DESCRIPTION

Docopt is ...

=head1 LICENSE

Copyright (C) tokuhirom.

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=head1 AUTHOR

tokuhirom E<lt>tokuhirom@gmail.comE<gt>

=cut

