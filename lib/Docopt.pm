use 5.008005;
use strict;
use warnings FATAL => 'all';

package Docopt;

use Docopt::Util qw(string_partition);

our $DocoptExit = 1;

package Docopt::Pattern;

sub new {
    my $class = shift;
    bless [], $class;
}

sub fix {
    my $self = shift;
    # TODO: Do something
    return $self;
}

package Docopt;

sub transform { ... }

# Leaf/terminal node of a pattern tree
package Docopt::LeafPattern;
use parent -norequire, qw(Docopt::Pattern);

use Docopt::Util qw(repl class_name);

use Class::Accessor::Lite (
    rw => [qw(name value)],
);

sub new {
    my ($class, $name, $value) = @_;
    bless {
        name => $name,
        value => $value,
    }, $class;
}


sub __repl__ {
    my $self = shift;
    sprintf '%s(%s, %s)',
        class_name($self),
        repl($self->name),
        repl($self->value);
}
sub flat { ... }
sub match { ... }

package Docopt::BranchPattern;

use Carp;

use Docopt::Util qw(repl class_name);

sub new {
    my ($class, $children) = @_;
    Carp::confess "Children must be arrayref: $class, $children" unless ref $children eq 'ARRAY';

    # zjzj FIXME ad-hoc hack
    $children = [ map { ref($_) eq 'ARRAY' ? @$_ : $_ } @$children];

    bless {
        children => [@$children],
    }, $class;
}

sub __repl__ {
    my $self = shift;
    sprintf '%s(%s)',
        class_name($self),
        join(', ', map { repl($_) } @{$self->{children}});
}

package Docopt::Argument;
use parent -norequire, qw(Docopt::LeafPattern);

sub single_match { ... }

sub parse { ... }

package Docopt::Command;
use parent -norequire, qw(Docopt::Argument);

use Class::Accessor::Lite (
    rw => [qw(name value)]
);

sub new {
    my ($class, $name, $value) = @_;
    bless {
        name => $name,
        value => $value,
    }, $class;
}

sub single_match { ... }

package Docopt::Required;

use parent -norequire, qw(Docopt::BranchPattern);

sub match { ... }

package Docopt::Optional;

use parent -norequire, qw(Docopt::BranchPattern);

sub match { ... }

package Docopt::OptionsShortcut;
# Marker/placeholder for [options] shortcut.

use parent -norequire, qw(Docopt::Optional);

sub match { ... }

package Docopt::OneOrMore;

use parent -norequire, qw(Docopt::BranchPattern);

sub match { ... }

package Docopt::Either;

use parent -norequire, qw(Docopt::BranchPattern);

sub match { ... }

package Docopt::Tokens;

use Docopt::Util qw(repl);

sub new {
    my ($class, $source) = @_;
    bless [@$source], $class;
}

sub from_pattern {
    my ($class, $source) = @_;

    $source =~ s/([\[\]\(\)\|]|\.\.\.)/ $1 /g;
    my @source = grep { defined($_) && length $_ > 0 } split /\s+|(\S*<.*?>)/, $source;
    return Docopt::Tokens->new(\@source);
}

sub move {
    my $self = shift;
    shift @$self;
}

sub current {
    my $self = shift;
    $self->[0];
}

sub __repl__ {
    my $self = shift;
    '[' . join(', ', map { repl($_) } @$self) . ']';
}


package Docopt;

our $VERSION = "0.01";

# TODO: test



package Docopt::Option;

use Docopt::Util qw(repl strip);

use Class::Accessor::Lite 0.05 (
    rw => [qw(short long argcount value)],
);

sub new {
    my ($class, $short, $long, $argcount, $value) = @_;
    if (@_<= 3) { $argcount = 0 }

    return bless {
        short => $short,
        long => $long,
        argcount => $argcount,
        value => !defined($value) && $argcount ? undef : $value,
    }, $class;
}

sub parse {
    my ($class, $option_description) = @_;
    my ($short, $long, $argcount, $value) = (undef, undef, 0, undef);

    my ($options, undef, $description) = split /  /, strip($option_description);

    $options =~ s/,/ /g;
    $options =~ s/=/ /g;
    for my $s (split /\s+/, $options) {
        if ($s =~ /^--/) {
            $long = $s;
        } elsif ($s =~ /^-/) {
            $short = $s;
        } else {
            $argcount = 1;
        }
    }
    if ($argcount) {
        if ($description =~ /\[default: (.*)\]/) {
            $value = $1;
        }
    }
    return $class->new($short, $long, $argcount, $value);
}

sub __repl__ {
    my ($self) = @_;
    sprintf 'Option(%s, %s, %s, %s)',
        repl($self->{short}),
        repl($self->{long}),
        repl($self->{argcount}),
        repl($self->{value});
}

package Docopt;

# long ::= '--' chars [ ( ' ' | '=' ) chars ] ;
sub parse_long {
    my ($tokens, $options) = @_;
    ref($options) eq 'ARRAY' or die;

    my ($long, $eq, $value) = string_partition($tokens->move, '=');
    $long =~ /\A--/ or die;
    $value = $eq eq '' && $value eq '' ? undef : $value;
    my @similar = grep { $_->long && $_->long eq $long } @$options;
    if ($Docopt::DocoptExit && @similar == 0) { # if no exact match
        @similar = grep { $_->long && $_->long =~ /$long/ } @$options;
    }
    my $o;
    if (@similar > 1) { # might be simply specified ambiguously 2+ times?
        die sprintf '%s is not a unique prefix: %s?',
            $long, join(', ', map { $_->long } @similar);
    } elsif (@similar < 1) {
        my $argcount = $eq eq '=' ? 1 : 0;
        $o = Docopt::Option->new(undef, $long, $argcount);
        push @$options, $o;
        if ($Docopt::DocoptExit) {
            $o = Docopt::Option->new(undef, $long, $argcount, $argcount ? $value : \1);
        }
    } else {
        $o = Docopt::Option->new(
            $similar[0]->short,
            $similar[0]->long,
            $similar[0]->argcount,
            $similar[0]->value,
        );
        if ($o->argcount == 0) {
            if (defined $value) {
                die sprintf "%s must not have an argument", $o->long;
            }
        } else {
            if (not defined $value) {
                if (
                    (not defined $tokens->current() ) || $tokens->current eq '==') {
                    die sprintf "%s requires argument", $o->long
                } 
                $value = $tokens->move;
            }
        }
        if ($Docopt::DocoptExit) {
            $o->value(defined($value) ? $value : \1);
        }
    }
    return [$o];

#   long, eq, value = tokens.move().partition('=')
#   assert long.startswith('--')
#   value = None if eq == value == '' else value
#   similar = [o for o in options if o.long == long]
#   if tokens.error is DocoptExit and similar == []:  # if no exact match
#       similar = [o for o in options if o.long and o.long.startswith(long)]
#   if len(similar) > 1:  # might be simply specified ambiguously 2+ times?
#       raise tokens.error('%s is not a unique prefix: %s?' %
#                          (long, ', '.join(o.long for o in similar)))
#   elif len(similar) < 1:
#       argcount = 1 if eq == '=' else 0
#       o = Option(None, long, argcount)
#       options.append(o)
#       if tokens.error is DocoptExit:
#           o = Option(None, long, argcount, value if argcount else True)
#   else:
#       o = Option(similar[0].short, similar[0].long,
#                  similar[0].argcount, similar[0].value)
#       if o.argcount == 0:
#           if value is not None:
#               raise tokens.error('%s must not have an argument' % o.long)
#       else:
#           if value is None:
#               if tokens.current() in [None, '--']:
#                   raise tokens.error('%s requires argument' % o.long)
#               value = tokens.move()
#       if tokens.error is DocoptExit:
#           o.value = value if value is not None else True
#   return [o]
}

# shorts ::= '-' ( chars )* [ [ ' ' ] chars ] ;
sub parse_shorts {
    my ($tokens, $options) = @_;

    my $token = $tokens->move;
    (my $left = $token) =~ s/^-//;
    my @parsed;
    while ($left ne '') {
        my $o;
        $left =~ s/\A(.)//;
        my $short = '-' . $1;
        my @similar = grep { $_->short eq $short } @$options;
        if (@similar > 1) {
            die sprintf "%s is specified ambiguously %d times",
                $short, 0+@similar;
        } elsif (@similar < 1) {
            $o = Docopt::Option->new($short, undef, 0);
            push @$options, $o;
            if ($Docopt::DocoptExit) {
                $o = Docopt::Option->new($short, undef, 0, undef)
            }
        } else {
            # why copying is necessary here?
            $o = Docopt::Option->new($short, $similar[0]->long,
                $similar[0]->argcount, $similar[0]->value);
            my $value = undef;
            if ($o->argcount != 0) {
                if ($left eq '') {
                    if (not defined($tokens->current) || $tokens->current eq '--') {
                        die "$short requires argument";
                    }
                    $value = $tokens->move;
                } else {
                    $value = $left;
                    $left = '';
                }
            }
            if ($Docopt::DocoptExit) {
                $o->value(defined($value) ? $value : \1);
            }
        }
        push @parsed, $o;
    }
    return \@parsed;

# def parse_shorts(tokens, options):
#   token = tokens.move()
#   assert token.startswith('-') and not token.startswith('--')
#   left = token.lstrip('-')
#   parsed = []
#   while left != '':
#       short, left = '-' + left[0], left[1:]
#       similar = [o for o in options if o.short == short]
#       if len(similar) > 1:
#           raise tokens.error('%s is specified ambiguously %d times' %
#                              (short, len(similar)))
#       elif len(similar) < 1:
#           o = Option(short, None, 0)
#           options.append(o)
#           if tokens.error is DocoptExit:
#               o = Option(short, None, 0, True)
#       else:  # why copying is necessary here?
#           o = Option(short, similar[0].long,
#                      similar[0].argcount, similar[0].value)
#           value = None
#           if o.argcount != 0:
#               if left == '':
#                   if tokens.current() in [None, '--']:
#                       raise tokens.error('%s requires argument' % short)
#                   value = tokens.move()
#               else:
#                   value = left
#                   left = ''
#           if tokens.error is DocoptExit:
#               o.value = value if value is not None else True
#       parsed.append(o)
#   return parsed
}
use Docopt::Util qw(repl);

sub parse_pattern {
    my ($source, $options) = @_;
    my $tokens = Docopt::Tokens->from_pattern($source);
    my $result = parse_expr($tokens, $options);
    if (defined $tokens->current()) {
        die "Unexpected ending: " . repl(join(' ', $tokens));
    }
    return Docopt::Required->new(@$result);

#   def parse_pattern(source, options):
#       tokens = Tokens.from_pattern(source)
#       result = parse_expr(tokens, options)
#       if tokens.current() is not None:
#           raise tokens.error('unexpected ending: %r' % ' '.join(tokens))
#       return Required(*result)
}

# $tokens: Docopt::Tokens
# $options: ARRAY
sub parse_expr {
    # expr ::= seq ( '|' seq )* ;
    my ($tokens, $options) = @_;

    my $seq = parse_seq($tokens, $options);
    if (!defined($tokens->current) || $tokens->current ne '|') {
        return $seq;
    }

#   result = [Required(*seq)] if len(seq) > 1 else seq
    my @result = @$seq > 1 ? Docopt::Required->new($seq) : @$seq;
    while ($tokens->current eq '|') {
        $tokens->move();
        $seq = parse_seq($tokens, $options);
        push @result, @$seq > 1 ? Docopt::Required->new($seq) : @$seq;
    }
    # zjzj This map() is so bad. But i can't remove this correctly...
    return @result > 1 ? [Docopt::Either->new([map { ref $_ eq 'ARRAY' ? @$_ : $_ } @result])] : \@result;

#   seq = parse_seq(tokens, options)
#   if tokens.current() != '|':
#       return seq
#   result = [Required(*seq)] if len(seq) > 1 else seq
#   while tokens.current() == '|':
#       tokens.move()
#       seq = parse_seq(tokens, options)
#       result += [Required(*seq)] if len(seq) > 1 else seq
#   return [Either(*result)] if len(result) > 1 else result
}

*in = *Docopt::Util::in;

# seq ::= ( atom [ '...' ] )* ;
sub parse_seq {
    my ($tokens, $options) = @_;
    my @result;
    while (not in($tokens->current, [undef, ']', ')', '|'])) {
        my $atom = parse_atom($tokens, $options);
        if (defined($tokens->current) && $tokens->current eq '...') {
            $atom = Docopt::OneOrMore->new($atom);
            $tokens->move;
        }
        push @result, $atom;
    }
    return \@result;
#   def parse_seq(tokens, options):
#       """seq ::= ( atom [ '...' ] )* ;"""
#       result = []
#       while tokens.current() not in [None, ']', ')', '|']:
#           atom = parse_atom(tokens, options)
#           if tokens.current() == '...':
#               atom = [OneOrMore(*atom)]
#               tokens.move()
#           result += atom
#       return result
}

#  atom ::= '(' expr ')' | '[' expr ']' | 'options'
#        | long | shorts | argument | command ;
sub parse_atom {
    my ($tokens, $options) = @_;

    my $token = $tokens->current();
    my @result;
    if ($token eq '(' || $token eq '[') {
        $tokens->move;
        my ($matching, $pattern) = @{{
            '(' => [')', Docopt::Required::],
            '[' => [']', Docopt::Optional::]
        }->{$token}};
        my $expr = parse_expr($tokens, $options);
        my $result = $pattern->new($expr);
        if (($tokens->move ||'') ne $matching) {
            die "unmatched '$token'";
        }
        return [$result];
    } elsif ($token eq 'options') {
        $tokens->move;
        return [Docopt::OptionsShortcut->new];
    } elsif ($token =~ /^--/ && $token ne '--') {
        return parse_long($tokens, $options);
    } elsif ($token =~ /^-/ && ($token ne '-' && $token ne '--')) {
        return parse_shorts($tokens, $options);
    } elsif (($token =~ /^</ && $token =~ />$/) or $token =~ /\A[A-Z]+\z/) {
        return [Docopt::Argument->new($tokens->move)];
    } else {
        return [Docopt::Command->new($tokens->move)];
    }

#   token = tokens.current()
#   result = []
#   if token in '([':
#       tokens.move()
#       matching, pattern = {'(': [')', Required], '[': [']', Optional]}[token]
#       result = pattern(*parse_expr(tokens, options))
#       if tokens.move() != matching:
#           raise tokens.error("unmatched '%s'" % token)
#       return [result]
#   elif token == 'options':
#       tokens.move()
#       return [OptionsShortcut()]
#   elif token.startswith('--') and token != '--':
#       return parse_long(tokens, options)
#   elif token.startswith('-') and token not in ('-', '--'):
#       return parse_shorts(tokens, options)
#   elif token.startswith('<') and token.endswith('>') or token.isupper():
#       return [Argument(tokens.move())]
#   else:
#       return [Command(tokens.move())]
}

sub parse_argv { ... }

sub parse_defaults {
    my ($doc) = @_;

    my @defaults;

    for my $s (parse_section('options:', $doc)) {
        # FIXME corner case "bla: options: --foo"
        $s =~ s/\Aoptions://;
        my @split = split /\n *(-\S+?)/, "\n" . $s;
        shift @split;
        my @split2;
        for (my $i=0; $i<@split; $i+=2) {
            push @split2, $split[$i].$split[$i+1];
        }
#       options = [Option.parse(s) for s in split if s.startswith('-')]
        my @options;
        for my $s (grep /^-/, @split2) {
            push @options, Docopt::Option->parse($s);
        }
        push @defaults, @options;
    }
    return @defaults;
}

sub parse_section {
    my ($name, $source) = @_;
    my @s;
    while ($source =~ /^([^\n]*${name}[^\n]*\n?(?:[ \t].*?(?:\n|$))*)/img) {
        local $_ = $1;
        s/^\s+//;
        s/\s+$//;
        push @s, $_;
    }
    return @s;
}

sub formal_usage {
    my ($section) = @_;
    $section =~ s/^usage://;
    my @pu = grep { /\S/ } split /\s+/, $section;
    my $cmd = shift @pu;
    return '( ' . join(' ', map { $_ eq $cmd ? ') | (' : $_ } @pu) . ' )';
}


sub extras { ... }

sub docopt {
    my %args = @_;

    my $argv = exists($args{argv}) ? $args{argv} : \@ARGV;
    my $doc = $args{doc};
    my @usage_sections = parse_section('usage:', $doc);
    if (@usage_sections == 0) {
        die '"usage:" (case-insensitive) not found.';
    }
    if (@usage_sections == 1) {
        die 'More than one "usage:" (case-insensitive).';
    }

    my $options = parse_defaults($doc);
    my $pattern = parse_pattern(formal_usage($usage_sections[0]), $options);
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

