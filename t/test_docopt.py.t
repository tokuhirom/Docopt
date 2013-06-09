use strict;
use warnings;
use utf8;
use Test::More;
use Data::Dumper;
BEGIN { *CORE::GLOBAL::exit = sub (;$) { die bless {}, 'SystemExit' } };
use Docopt;
use Data::Dumper;
use boolean;
use Docopt::Util qw(repl serialize pyprint);
use Test::Fatal;
use t::Util;

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

subtest 'test_option_name' => sub {
    is(Option('-h', None)->name, '-h');
    is(Option('-h', '--help')->name, '--help');
    is(Option(None, '--help')->name, '--help');
};

DEBUG:
subtest 'test_commands' => sub {
    is_deeply(docopt('Usage: prog add', 'add'), {'add'=> True});
    is_deeply(docopt('Usage: prog [add]', ''), {'add'=> undef});
    is_deeply(docopt('Usage: prog [add]', 'add'), {'add'=> True});
    is_deeply(docopt('Usage: prog (add|rm)', 'add'), {'add' => True, 'rm'=> undef});
    is_deeply(docopt('Usage: prog (add|rm)', 'rm'), {'add'=> undef, 'rm'=> True});
    is_deeply(docopt('Usage: prog a b', 'a b'), {'a' => True, 'b' => True});
#   with raises(DocoptExit):
    isa_ok(exception { docopt('Usage: prog a b', 'b a') }, 'Docopt::Exceptions::DocoptExit');
};

subtest 'test_formal_usage' => sub {
    my $doc = "
    Usage: prog [-hv] ARG
           prog N M

    prog is a program.";
    my ($usage,) = Docopt::parse_section('usage:', $doc);
    is($usage, "Usage: prog [-hv] ARG\n           prog N M");
    is(Docopt::formal_usage($usage), "( [-hv] ARG ) | ( N M )");
};

subtest 'parse_argv' => sub {
    test_parse_argv(
        [],
        []
    );

    test_parse_argv(
        '-h',
        [Option('-h', None, 0, True)],
    );
    test_parse_argv(
        '-h --verbose',
        [Option('-h', None, 0, True), Option('-v', '--verbose', 0, True)],
    );
    test_parse_argv('-h --file f.txt',
            [Option('-h', None, 0, True), Option('-f', '--file', 1, 'f.txt')]);
    test_parse_argv('-h --file f.txt arg',
           [Option('-h', None, 0, True),
            Option('-f', '--file', 1, 'f.txt'),
            Argument(None, 'arg')]);
    test_parse_argv('-h --file f.txt arg arg2',
            [Option('-h', None, 0, True),
             Option('-f', '--file', 1, 'f.txt'),
             Argument(None, 'arg'),
             Argument(None, 'arg2')]);
    test_parse_argv('-h arg -- -v',
            [Option('-h', None, 0, True),
             Argument(None, 'arg'),
             Argument(None, '--'),
             Argument(None, '-v')]);
};

subtest 'test_parse_pattern' => sub {
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

subtest test_options_match => sub {
    test_options_match(
        [Option('-a')->match([Option('-a', True)])],
            [True, [], [Option('-a', True)]]
    );
    test_options_match([Option('-a')->match([Option('-x')])], [False, [Option('-x')], []]);
    test_options_match([Option('-a')->match([Argument('N')])], [False, [Argument('N')], []]);
    test_options_match([Option('-a')->match([Option('-x'), Option('-a'), Argument('N')])],
            [True, [Option('-x'), Argument('N')], [Option('-a')]]);
    test_options_match([Option('-a')->match([Option('-a', True), Option('-a')])],
            [True, [Option('-a')], [Option('-a', True)]]);
};

subtest 'test_argument_match' => sub {
    test_argument_match(
        [Argument('N')->match([Argument(None, 9)])],
        [True, [], [Argument('N', 9)]]
    );
    test_argument_match(
        [Argument('N')->match([Option('-x')])],
        [False, [Option('-x')], []]
    );
    test_argument_match(
        [Argument('N')->match([Option('-x'),
                                Option('-a'),
                                Argument(None, 5)])],
        [True, [Option('-x'), Option('-a')], [Argument('N', 5)]]
    );
    test_argument_match(
        [Argument('N')->match([Argument(None, 9), Argument(None, 0)])],
        [True, [Argument(None, 0)], [Argument('N', 9)]]
    );
};

subtest 'test_command_match' => sub {
    test_command_match(
        [Command('c')->match([Argument(None, 'c')])],
        [True, [], [Command('c', True)]]
    );
    test_command_match(
        [Command('c')->match([Option('-x')])],
        [False, [Option('-x')], []],
    );
    test_command_match(
        [Command('c')->match([Option('-x'),
                                Option('-a'),
                                Argument(None, 'c')])],
        [True, [Option('-x'), Option('-a')], [Command('c', True)]]
    );
    test_command_match(
        [Either(Command('add', False), Command('rm', False))->match(
            [Argument(None, 'rm')])],
        [True, [], [Command('rm', True)]]
    );
};

subtest 'test_optional_match' => sub {
    test_optional_match(
        [Optional(Option('-a'))->match([Option('-a')])],
        [True, [], [Option('-a')]]
    );
    test_optional_match(
        [Optional(Option('-a'))->match([])],
        [True, [], []],
    );
    test_optional_match(
        [Optional(Option('-a'))->match([Option('-x')])],
        [True, [Option('-x')], []]
    );
    test_optional_match(
        [Optional(Option('-a'), Option('-b'))->match([Option('-a')])],
        [True, [], [Option('-a')]],
    );
    test_optional_match(
        [Optional(Option('-a'), Option('-b'))->match([Option('-b')])],
        [True, [], [Option('-b')]],
    );
    test_optional_match(
        [Optional(Option('-a'), Option('-b'))->match([Option('-x')])],
        [True, [Option('-x')], []]
    );
    test_optional_match(
        [Optional(Argument('N'))->match([Argument(None, 9)])],
        [True, [], [Argument('N', 9)]],
    );
    test_optional_match(
        [Optional(Option('-a'), Option('-b'))->match(
                [Option('-b'), Option('-x'), Option('-a')])],
        [True, [Option('-x')], [Option('-a'), Option('-b')]],
    );
};

subtest 'test_required_match' => sub {
    test_required_match(
        [Required(Option('-a'))->match([Option('-a')])],
        [(True, [], [Option('-a')])],
    );
    test_required_match(
        [Required(Option('-a'))->match([])],
        [(False, [], [])],
    );
    test_required_match(
        [Required(Option('-a'))->match([Option('-x')])],
        [(False, [Option('-x')], [])],
    );
    test_required_match(
        [Required(Option('-a'), Option('-b'))->match([Option('-a')])],
        [(False, [Option('-a')], [])]
    );
};

subtest 'test_either_match()' => sub {
    test_either_match(
        [Either(Option('-a'), Option('-b'))->match(
                [Option('-a')])],
        [(True, [], [Option('-a')])]
    );
    test_either_match(
        [Either(Option('-a'), Option('-b'))->match(
            [Option('-a'), Option('-b')])],
        [(True, [Option('-b')], [Option('-a')])],
    );
    test_either_match(
        [Either(Option('-a'), Option('-b'))->match(
                [Option('-x')])],
        [(False, [Option('-x')], [])]
    );
    test_either_match(
        [Either(Option('-a'), Option('-b'), Option('-c'))->match(
            [Option('-x'), Option('-b')])],
        [(True, [Option('-x')], [Option('-b')])]
    );
    test_either_match(
       [Either(Argument('M'),
                  Required(Argument('N'), Argument('M')))->match(
                                   [Argument(None, 1), Argument(None, 2)])],
        [(True, [], [Argument('N', 1), Argument('M', 2)])]
    );
};

subtest 'test_one_or_more_match' => sub {
    test_one_or_more_match(
        [OneOrMore(Argument('N'))->match([Argument(None, 9)])],
        [(True, [], [Argument('N', 9)])],
    );
    test_one_or_more_match(
        [OneOrMore(Argument('N'))->match([])],
        [(False, [], [])]
    );
    test_one_or_more_match(
        [OneOrMore(Argument('N'))->match([Option('-x')])],
        [(False, [Option('-x')], [])],
    );
    test_one_or_more_match(
        [OneOrMore(Argument('N'))->match(
            [Argument(None, 9), Argument(None, 8)])],
        [True, [], [Argument('N', 9), Argument('N', 8)]],
    );
    test_one_or_more_match(
        [OneOrMore(Argument('N'))->match(
            [Argument(None, 9), Option('-x'), Argument(None, 8)])],
        [True, [Option('-x')], [Argument('N', 9), Argument('N', 8)]],
    );
    test_one_or_more_match(
        [OneOrMore(Option('-a'))->match(
            [Option('-a'), Argument(None, 8), Option('-a')])],
        [True, [Argument(None, 8)], [Option('-a'), Option('-a')]],
    );
    test_one_or_more_match(
        [OneOrMore(Option('-a'))->match([Argument(None, 8),
                                          Option('-x')])],
        [False, [Argument(None, 8), Option('-x')], []],
    );
    test_one_or_more_match(
        [OneOrMore(Required(Option('-a'), Argument('N')))->match(
                [Option('-a'), Argument(None, 1), Option('-x'),
                Option('-a'), Argument(None, 2)])],
        [True, [Option('-x')],
                [Option('-a'), Argument('N', 1), Option('-a'), Argument('N', 2)]],
    );
    test_one_or_more_match(
        [OneOrMore(Optional(Argument('N')))->match([Argument(None, 9)])],
        [(True, [], [Argument('N', 9)])],
    );
};

# TODO
#   subtest 'test_list_argument_match()' => sub {
#       test_list_argument_match(
#           [Required(Argument('N'), Argument('N'))->fix()->match(
#                   [Argument(None, '1'), Argument(None, '2')])],
#           [(True, [], [Argument('N', ['1', '2'])])],
#       );
#       test_list_argument_match(
#       [OneOrMore(Argument('N'))->fix()->match(
#           [Argument(None, '1'), Argument(None, '2'), Argument(None, '3')])],
#                       [(True, [], [Argument('N', ['1', '2', '3'])])],
#       );
#       test_list_argument_match(
#       [Required(Argument('N'), OneOrMore(Argument('N')))->fix()->match(
#           [Argument(None, '1'), Argument(None, '2'), Argument(None, '3')])],
#                       [(True, [], [Argument('N', ['1', '2', '3'])])],
#       );
#       test_list_argument_match(
#       [Required(Argument('N'), Required(Argument('N')))->fix()->match(
#               [Argument(None, '1'), Argument(None, '2')])],
#                       [(True, [], [Argument('N', ['1', '2'])])],
#       );
#   };

subtest 'test_basic_pattern_matching' => sub {
    # ( -a N [ -x Z ] )
    my $pattern = Required(Option('-a'), Argument('N'),
                       Optional(Option('-x'), Argument('Z')));
    # -a N
    test_basic_pattern_matching(
        [$pattern->match([Option('-a'), Argument(None, 9)])],
        [(True, [], [Option('-a'), Argument('N', 9)])],
    );
    # -a -x N Z
    test_basic_pattern_matching(
        [$pattern->match([Option('-a'), Option('-x'),
                          Argument(None, 9), Argument(None, 5)])],
        [(True, [], [Option('-a'), Argument('N', 9),
                        Option('-x'), Argument('Z', 5)])],
    );
    # -x N Z  # BZZ!
    test_basic_pattern_matching(
        [$pattern->match([Option('-x'),
                          Argument(None, 9),
                          Argument(None, 5)])],
        [(False, [Option('-x'), Argument(None, 9), Argument(None, 5)], [])],
    );
};

subtest 'test_pattern_either' => sub {
    test_pattern_either(
        [transform(Option('-a'))],
        [Either(Required(Option('-a')))]
    );
    test_pattern_either(
        [transform(Argument('A'))],
        [Either(Required(Argument('A')))]
    );
    test_pattern_either(
        [transform(Required(Either(Option('-a'), Option('-b')),
                    Option('-c')))],
        [Either(Required(Option('-a'), Option('-c')),
                Required(Option('-b'), Option('-c')))]
    );
    test_pattern_either(
        [transform(Optional(Option('-a'), Either(Option('-b'),
                                                   Option('-c'))))],
        [Either(Required(Option('-b'), Option('-a')),
                Required(Option('-c'), Option('-a')))],
    );
    test_pattern_either(
        [transform(Either(Option('-x'),
                                Either(Option('-y'), Option('-z'))))],
        [Either(Required(Option('-x')),
                Required(Option('-y')),
                Required(Option('-z')))]
    );
    test_pattern_either(
        [transform(OneOrMore(Argument('N'), Argument('M')))],
        [Either(Required(Argument('N'), Argument('M'),
                            Argument('N'), Argument('M')))]
    );
};

subtest 'test_pattern_fix_repeating_arguments' => sub {
    test_pattern_fix_repeating_arguments(
        [Option('-a')->fix_repeating_arguments()],
        [Option('-a')],
    );
    test_pattern_fix_repeating_arguments(
        [Argument('N', None)->fix_repeating_arguments()],
        [Argument('N', None)],
    );
    test_pattern_fix_repeating_arguments(
        [Required(Argument('N'),
                    Argument('N'))->fix_repeating_arguments()],
            [Required(Argument('N', []), Argument('N', []))]
    );
    test_pattern_fix_repeating_arguments(
        [Either(Argument('N'),
                        OneOrMore(Argument('N')))->fix()],
        [Either(Argument('N', []), OneOrMore(Argument('N', [])))],
    );
};


use Scalar::Util qw(refaddr);
subtest 'test_pattern_fix_identities_1' => sub {
    my $pattern = Required(Argument('N'), Argument('N'));
    is(serialize($pattern->children->[0]), serialize($pattern->children->[1]));
    isnt(refaddr($pattern->children->[0]), refaddr($pattern->children->[1]));
    $pattern->fix_identities();
    is(refaddr($pattern->children->[0]), refaddr($pattern->children->[1]));
};

subtest 'test_pattern_fix_identities_2' => sub {
    my $pattern = Required(Optional(Argument('X'), Argument('N')), Argument('N'));
    is_deeply($pattern->children->[0]->children->[1], $pattern->children->[1]);
    isnt(refaddr($pattern->children->[0]->children->[1]), refaddr($pattern->children->[1]));
    $pattern->fix_identities();
    is($pattern->children->[0]->children->[1], $pattern->children->[1]);
};


subtest 'test_long_options_error_handling' => sub {
#    with raises(DocoptLanguageError):
#        docopt('Usage: prog --non-existent', '--non-existent')
#    with raises(DocoptLanguageError):
#        docopt('Usage: prog --non-existent')

    isa_ok(exception {docopt('Usage: prog', '--non-existent')}, 'Docopt::Exceptions::DocoptExit');
    isa_ok(exception {docopt('Usage: prog [--version --verbose]\n',
               'Options: --version\n --verbose', '--ver')}, 'Docopt::Exceptions::DocoptExit');
    isa_ok(exception {docopt("Usage: prog --long\nOptions: --long ARG")}, 'Docopt::Exceptions::DocoptLanguageError');
    isa_ok(exception { docopt("Usage: prog --long ARG\nOptions: --long ARG", '--long') }, 'Docopt::Exceptions::DocoptExit');
    isa_ok(exception { docopt("Usage: prog --long=ARG\nOptions: --long") }, 'Docopt::Exceptions::DocoptLanguageError');
    isa_ok(exception { docopt("Usage: prog --long\nOptions: --long", '--long=ARG') }, 'Docopt::Exceptions::DocoptExit');
};

subtest 'test_short_options_error_handling' => sub {
    isa_ok(exception { docopt("Usage: prog -x\nOptions: -x  this\n -x  that") }, 'Docopt::Exceptions::DocoptLanguageError');

#    with raises(DocoptLanguageError):
#        docopt('Usage: prog -x')
    isa_ok(exception { docopt('Usage: prog', '-x') }, 'Docopt::Exceptions::DocoptExit');
    isa_ok(exception { docopt("Usage: prog -o\nOptions: -o ARG") }, 'Docopt::Exceptions::DocoptLanguageError');
    isa_ok(exception { docopt('Usage: prog -o ARG\nOptions: -o ARG', '-o') }, 'Docopt::Exceptions::DocoptExit');
};

subtest 'test_matching_paren' => sub {
    isa_ok(exception { docopt('Usage: prog [a [b]') }, 'Docopt::Exceptions::DocoptLanguageError');
    isa_ok(
        exception {
            docopt('Usage: prog [a [b] ] c )');
        },
        'Docopt::Exceptions::DocoptLanguageError'
    );
};

subtest 'test_allow_double_dash' => sub {
    is_deeply(
        docopt( "usage: prog [-o] [--] <arg>\nkptions: -o", '-- -o' ),
        { '-o' => undef, '<arg>' => '-o', '--' => true }
    );
    is_deeply(docopt("usage: prog [-o] [--] <arg>\nkptions: -o",
                  '-o 1'), {'-o'=> True, '<arg>'=>'1', '--' => undef});

    # "--" is not allowed; FIXME?
    isa_ok(exception { docopt("usage: prog [-o] <arg>\noptions:-o", '-- -o') }, 'Docopt::Exceptions::DocoptExit');
};

subtest 'test_docopt' => sub {
    my $doc = q{Usage: prog [-v] A

             Options: -v  Be verbose.};
    is_deeply(docopt($doc, 'arg'), {'-v' => undef, 'A' => 'arg'});
    is_deeply(docopt($doc, '-v arg'), {'-v' => true, 'A' => 'arg'});
    $doc = q{Usage: prog [-vqr] [FILE]
              prog INPUT OUTPUT
              prog --help

    Options:
      -v  print status messages
      -q  report only file names
      -r  show all occurrences of the same error
      --help

      };
    is_deeply(
        docopt($doc, '-v file.py'),
        {'-v'=> True, '-q'=> undef, '-r'=> undef, '--help'=> undef,
                 'FILE'=> 'file.py', 'INPUT'=> None, 'OUTPUT'=> None}
    );

    is_deeply(
        docopt($doc, '-v'),
        {'-v'=> True, '-q'=> undef, '-r'=> undef, '--help'=> undef,
                 'FILE'=> None, 'INPUT'=> None, 'OUTPUT'=> None}
    );

    # does not match
    isa_ok(exception { 
        docopt($doc, '-v input.py output.py')
    }, 'Docopt::Exceptions::DocoptExit');

    isa_ok(exception { 
        docopt($doc, '--fake')
    }, 'Docopt::Exceptions::DocoptExit');

    isa_ok(exception { 
        docopt($doc, '--hel')
    }, 'SystemExit');

    #with raises(SystemExit):
    #    docopt(doc, 'help')  XXX Maybe help command?
};

subtest 'test_language_errors' => sub {
    isa_ok(
        exception {
            docopt('no usage with colon here')
        },
        'Docopt::Exceptions::DocoptLanguageError',
    );
    isa_ok(
        exception {
            docopt("usage: here \n\n and again usage: here")
        },
        'Docopt::Exceptions::DocoptLanguageError',
    );
};

subtest 'test_issue_40' => sub {
    isa_ok(
        exception { docopt('usage: prog --help-commands | --help', '--help') },
        'SystemExit',
    );
    is_deeply(
        docopt('usage: prog --aabb | --aa', '--aa'),
        { '--aabb' => undef, '--aa' => true }
    );
};

# test_issue34_unicode_strings is python specific.

subtest 'test_count_multiple_flags' => sub {
    is_deeply(docopt('usage: prog [-v]', '-v'), {'-v' => True});
    is_deeply(docopt('usage: prog [-vv]', ''), {'-v'=> 0});
    is_deeply(docopt('usage: prog [-vv]', '-v'), {'-v' => 1});
    is_deeply(docopt('usage: prog [-vv]', '-vv'), {'-v' => 2});
    isa_ok(
        exception { docopt('usage: prog [-vv]', '-vvv') },
        'Docopt::Exceptions::DocoptExit'
    );
    is_deeply(
        docopt('usage: prog [-v | -vv | -vvv]', '-vvv'),{'-v' => 3}
    );
    is_deeply(
        docopt('usage: prog -v...', '-vvvvvv'),
        {'-v' => 6}
    );
    is_deeply(
        docopt('usage: prog [--ver --ver]', '--ver --ver'),
        {'--ver' => 2}
    );
};

subtest 'test_any_options_parameter' => sub {
    isa_ok(
        exception { docopt('usage: prog [options]', '-foo --bar --spam=eggs') },
        'Docopt::Exceptions::DocoptExit',
    );
#    assert docopt('usage: prog [options]', '-foo --bar --spam=eggs',
#                  any_options=True) == {'-f': True, '-o': 2,
#                                         '--bar': True, '--spam': 'eggs'}
    isa_ok(
        exception {
            docopt('usage: prog [options]', '--foo --bar --bar')
        },
        'Docopt::Exceptions::DocoptExit',
    );
#    assert docopt('usage: prog [options]', '--foo --bar --bar',
#                  any_options=True) == {'--foo': True, '--bar': 2}
    isa_ok(
        exception {
            docopt('usage: prog [options]', '--bar --bar --bar -ffff')
        },
        'Docopt::Exceptions::DocoptExit',
    );
#    assert docopt('usage: prog [options]', '--bar --bar --bar -ffff',
#                  any_options=True) == {'--bar': 3, '-f': 4}
    isa_ok(
        exception {
            docopt('usage: prog [options]', '--long=arg --long=another')
        },
        'Docopt::Exceptions::DocoptExit',
    );
#    assert docopt('usage: prog [options]', '--long=arg --long=another',
#                  any_options=True) == {'--long': ['arg', 'another']}
};

subtest 'test_default_value_for_positional_arguments' => sub {
    # disabled right now
    is_deeply(
        docopt("usage: prog [<p>]\n\n<p>  [default: x]", ""),
            {'<p>' => None}
    #       {'<p>': 'x'}
    );
    is_deeply(
        docopt("usage: prog [<p>]...\n\n<p>  [default: x y]", ""),
            {'<p>' => []}
    #       {'<p>': ['x', 'y']}
    );
    is_deeply_ex(
        docopt("usage: prog [<p>]...\n\n<p>  [default: x y]", "this"),
        {'<p>' => ['this']}
    #       {'<p>': ['this']}
    );
};

subtest 'test_issue_59' => sub {
    is_deeply(
        docopt('usage: prog --long=<a>', '--long='),
        {'--long' => ''}
    );
    is_deeply(
        docopt("usage: prog -l <a>\noptions: -l <a>", ['-l', '']),
        {'-l'=> ''}
    );
};

subtest 'test_options_first()' => sub {
    is_deeply_ex(docopt('usage: prog [--opt] [<args>...]',
                  '--opt this that'), {'--opt'=> True,
                                         '<args>'=> ['this', 'that']});
    is_deeply_ex(docopt('usage: prog [--opt] [<args>...]',
                  'this that --opt'), {'--opt'=> True,
                                         '<args>'=> ['this', 'that']});
    is_deeply_ex(docopt('usage: prog [--opt] [<args>...]',
                  'this that --opt',
                  True,undef,
                  True), {'--opt'=> undef,
                                          '<args>'=> ['this', 'that', '--opt']});
};



done_testing;

sub test_pattern_flat {
    my ($input, $expected, $types) = @_;
    local $Test::Builder::Level = $Test::Builder::Level + 1;
    my $got = $input->flat($types);
    is_deeply(
        $got,
        $expected,
    ) or diag Dumper($got);
}

sub test_option {
    my ($input, $expected) = @_;
    local $Test::Builder::Level = $Test::Builder::Level + 1;
    is_deeply(
        Docopt::Option->parse($input),
        Docopt::Option->new(@$expected),
        $input,
    );
}

sub test_parse_argv {
    my ($input, $expected) = @_;
    local $Test::Builder::Level = $Test::Builder::Level + 1;
    my $o = [Option('-h'), Option('-v', '--verbose'), Option('-f', '--file', 1)];
    my $got = Docopt::parse_argv(Docopt::Tokens->new($input), $o);
    is_deeply(
        $got,
        $expected
    ) or diag Dumper($got);
}

sub test_parse_pattern {
    my ($input, $expected) = @_;
    local $Test::Builder::Level = $Test::Builder::Level + 1;
    my $o = [Option('-h'), Option('-v', '--verbose'), Option('-f', '--file', 1)];
    is(
        Docopt::parse_pattern($input, $o)->__repl__,
        $expected->__repl__,
        $input
    );
}

sub test_options_match {
    my ($got, $expected) = @_;
    local $Test::Builder::Level = $Test::Builder::Level + 1;
    local $Data::Dumper::Purity=0;
    local $Data::Dumper::Terse=1;
    local $Data::Dumper::Deepcopy=1;
    local $Data::Dumper::Sortkeys=1;
    is_deeply(
        $got,
        $expected,
    ) or diag Dumper($got, $expected);
}

sub test_argument_match {
    my ($got, $expected) = @_;
    local $Test::Builder::Level = $Test::Builder::Level + 1;
    local $Data::Dumper::Purity=0;
    local $Data::Dumper::Terse=1;
    local $Data::Dumper::Deepcopy=1;
    local $Data::Dumper::Sortkeys=1;
    is_deeply(
        $got,
        $expected,
    ) or diag Dumper($got, $expected);
}
sub test_command_match { goto &test_argument_match }
sub test_optional_match { goto &test_argument_match }
sub test_required_match { goto &test_argument_match }
sub test_either_match { goto &test_argument_match }
sub test_one_or_more_match { goto &test_argument_match }
sub test_list_argument_match { goto &test_argument_match }
sub test_basic_pattern_matching { goto &test_argument_match }
sub test_pattern_either { goto &test_argument_match }
sub test_pattern_fix_repeating_arguments { goto &test_argument_match }

