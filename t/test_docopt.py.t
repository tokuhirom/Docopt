use strict;
use warnings;
use utf8;
use Test::More;
use Data::Dumper;
use Docopt;

sub Option { Docopt::Option->new(@_) }
sub Argument { Docopt::Argument->new(@_) }

sub Optional { Docopt::Optional->new(\@_) }
sub Either { Docopt::Either->new(\@_) }
sub Required { Docopt::Required->new(\@_) }
sub OneOrMore { Docopt::OneOrMore->new(\@_) }
sub OptionsShortcut() { Docopt::OptionsShortcut->new(\@_) }

sub None() { undef }

subtest 'test_pattern_flat' => sub {
    test_pattern_flat(
        Required(OneOrMore(Argument('N')),
                        Option('-a'), Argument('M')),
        [Argument('N'), Option('-a'), Argument('M')],
    );
    test_pattern_flat(
        Required(Optional(OptionsShortcut()),
                                Optional(Option('-a', None))),
        [OptionsShortcut()],
        Docopt::OptionsShortcut::
    );
};

subtest 'test_option' => sub {
    test_option(
        '-h',
        ['-h', None]
    );
    test_option(
        '--help',
        [None, '--help'],
    );
    test_option(
        '-h --help',
        ['-h', '--help'],
    );
    test_option(
        '-h, --help',
        ['-h', '--help'],
    );

    test_option(
        '-h TOPIC',
        ['-h', None, 1]
    );
    test_option(
        '--help TOPIC',
        [None, '--help', 1]
    );
    test_option(
        '-h TOPIC --help TOPIC',
        ['-h', '--help', 1]
    );
    test_option(
        '-h TOPIC, --help=TOPIC',
        ['-h', '--help', 1]
    );
    test_option(
        '-h  Description...',
        ['-h', None]
    );
    test_option(
        '-h --help  Description...',
        ['-h', '--help']
    );
    test_option(
        '-h TOPIC  Description...',
        ['-h', None, 1]
    );
    test_option(
        '    -h',
        ['-h', None]
    );


    test_option('-h TOPIC  Descripton... [default: 2]',
               ['-h', None, 1, '2']);
    test_option('-h TOPIC  Descripton... [default: topic-1]',
               ['-h', None, 1, 'topic-1']);
    test_option('--help=TOPIC  ... [default: 3.14]',
               [None, '--help', 1, '3.14']);
    test_option('-h, --help=DIR  ... [default: ./]',
               ['-h', '--help', 1, "./"]);
    test_option('-h TOPIC  Descripton... [dEfAuLt: 2]',
               ['-h', None, 1, '2']);
};

done_testing;

sub test_pattern_flat {
    my ($input, $expected, $types) = @_;
    local $Test::Builder::Level = $Test::Builder::Level + 1;
    local $Docopt::DocoptExit=0;
    my $got = $input->flat($types);
    is_deeply(
        $got,
        $expected,
    ) or diag Dumper($got);
}

sub test_option {
    my ($input, $expected) = @_;
    is_deeply(
        Docopt::Option->parse($input),
        Docopt::Option->new(@$expected),
        $input,
    );
}
