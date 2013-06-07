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
sub OptionsShortcut() { Docopt::OptionsShortcut->new(\@_) }

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
        test_parse_pattern('[ -h ]',
           Required(Optional(Option('-h')))
        );
        test_parse_pattern('[ ARG ... ]',
           Required(Optional(OneOrMore(Argument('ARG')))),
        );
        test_parse_pattern('[ -h | -v ]',
            Required(Optional(Either(Option('-h'),
                                     Option('-v', '--verbose'))))
        );
        test_parse_pattern('[ --file <f> ]',
            Required(Optional(Option('-f', '--file', 1, undef)))
        );
        test_parse_pattern(
            '( -h | -v [ --file <f> ] )',
                Required(Required(
                    Either(Option('-h'),
                    Required(Option('-v', '--verbose'),
                    Optional(Option('-f', '--file', 1, undef))))))
        );
        test_parse_pattern('(-h|-v[--file=<f>]N...)',
            Required(Required(Either(Option('-h'),
                Required(Option('-v', '--verbose'),
                Optional(Option('-f', '--file', 1, undef)),
                OneOrMore(Argument('N')))))),
        );
        test_parse_pattern('(N [M | (K | L)] | O P)',
            Required(Required(Either(
                            Required(Argument('N'),
                                        Optional(Either(Argument('M'),
                                                        Required(Either(Argument('K'),
                                                                        Argument('L')))))),
                            Required(Argument('O'), Argument('P')))))
        );
        test_parse_pattern('[ -h ] [N]',
            Required(Optional(Option('-h')),
                     Optional(Argument('N'))));
        test_parse_pattern(
            '[options]',
            Required(Optional(OptionsShortcut())),
        );
        test_parse_pattern('[options] A',
            Required(Optional(OptionsShortcut()), Argument('A')));
        test_parse_pattern('-v [options]',
            Required(Option('-v', '--verbose'),
                                     Optional(OptionsShortcut())));
        test_parse_pattern('ADD',
            Required(Argument('ADD')));
        test_parse_pattern('<add>',
            Required(Argument('<add>')));
    };
};

done_testing;

sub test_parse_pattern {
    my ($input, $expected) = @_;
    local $Test::Builder::Level = $Test::Builder::Level + 1;
    local $Docopt::DocoptExit=0;
    my $o = [Option('-h'), Option('-v', '--verbose'), Option('-f', '--file', 1)];
    is(
        Docopt::parse_pattern($input, $o)->__repl__,
        $expected->__repl__,
        $input
    );
}
