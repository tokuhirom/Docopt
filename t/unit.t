use strict;
use warnings;
use utf8;
use Test::More;
use Docopt;

sub Option { Docopt::Option->new(@_) }
sub Argument { Docopt::Argument->new(@_) }

sub Optional { Docopt::Optional->new(\@_) }
sub Either { Docopt::Either->new(\@_) }
sub Required { Docopt::Required->new(\@_) }
sub OneOrMore { Docopt::OneOrMore->new(\@_) }


subtest 'parse_section' => sub {
    my @ret = Docopt::parse_section('usage', <<'...');
    usage: myapp
...
    is_deeply(\@ret, [
        'usage: myapp'
    ]);
};

subtest 'parse_defaults' => sub {
    my $doc = <<'...';
options:
    -h, --help  Print help message.
    -o FILE     Output file.
    --verbose   Verbose mode.
...

    my @defaults = Docopt::parse_defaults($doc);
    is(0+@defaults, 3);
    is($defaults[0]->__repl__, "Option('-h', '--help', 0, undef)");
    is($defaults[1]->__repl__, "Option('-o', undef, 1, undef)");
    is($defaults[2]->__repl__, "Option(undef, '--verbose', 0, undef)");
};

subtest 'formal_usage' => sub {
    my $doc = <<'...';
usage:
    foo x y
    foo a b
...
    my $expected = '( x y ) | ( a b )';
    is(Docopt::formal_usage($doc), $expected);
};

subtest 'Tokens.from_pattern' => sub {
    subtest 'complex' => sub {
        is(Docopt::Tokens->from_pattern('(-h|-v[--file=<f>]N...)')->__repl__,
            q!['(', '-h', '|', '-v', '[', '--file=<f>', ']', 'N', '...', ')']!
        );
    };
    subtest 'simple' => sub {
        my $doc = <<'...';
usage:
    foo x y
    foo a b
...
        is(Docopt::Tokens->from_pattern(Docopt::formal_usage($doc))->__repl__,
            "['(', 'x', 'y', ')', '|', '(', 'a', 'b', ')']",
        );
    };
};

subtest 'parse_pattern' => sub {
    subtest 'simple' => sub {
        my $doc = <<'...';
usage:
    foo bar
...
        my $formal = Docopt::formal_usage($doc);
        note(Docopt::Tokens->from_pattern($formal)->__repl__);
        my $options = [];
        my $result = Docopt::parse_pattern($formal, $options);
        is($result->__repl__, q!Required(Required(Command('bar', undef)))!);
    };
    subtest 'test_doctest.py' => sub {
        local $Docopt::DocoptExit=0;
        my $o = [Option('-h'), Option('-v', '--verbose'), Option('-f', '--file', 1)];
        is(
           Docopt::parse_pattern('[ -h ]', $o)->__repl__,
           Required(Optional(Option('-h')))->__repl__
        );
        is(
           Docopt::parse_pattern('[ ARG ... ]', $o)->__repl__,
           Required(Optional(OneOrMore(Argument('ARG'))))->__repl__,
        );
        is(
            Docopt::parse_pattern('[ -h | -v ]', $o)->__repl__,
            Required(Optional(Either(Option('-h'),
                                     Option('-v', '--verbose'))))->__repl__,
        );
        is(
            Docopt::parse_pattern('[ --file <f> ]', $o)->__repl__,
            Required(Optional(Option('-f', '--file', 1, undef)))->__repl__,
        );
        is(
            Docopt::parse_pattern('( -h | -v [ --file <f> ] )', $o)->__repl__,
                Required(Required(
                    Either(Option('-h'),
                    Required(Option('-v', '--verbose'),
                    Optional(Option('-f', '--file', 1, undef))))))->__repl__,
        );
        is(
            Docopt::parse_pattern('(-h|-v[--file=<f>]N...)', $o)->__repl__,
            Required(Required(Either(Option('-h'),
                Required(Option('-v', '--verbose'),
                Optional(Option('-f', '--file', 1, undef)),
                OneOrMore(Argument('N'))))))->__repl__,
        );
    };
};

done_testing;

