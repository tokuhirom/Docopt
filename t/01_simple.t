use strict;
use warnings;
use utf8;
use Test::More;
use Docopt;
use Test::Base::Less;
use Carp;

$SIG{INT} = \&Carp::confess;

filters {
    expected => ['eval'],
    argv => ['eval'],
};

for my $block (blocks) {
    my $docopt = Docopt::Parser->new();
    $docopt->parse($block->usage);
    my $got = $docopt->evaluate($block->argv);
    is_deeply($got, $block->expected);
}
done_testing;

__END__

===
--- usage
Usage: my_program
--- argv: []
--- expected: +{}

===
--- usage
Usage: my_program <host> <port>
--- argv: ['127.0.0.1', 8080]
--- expected: { host => '127.0.0.1', port => 8080 }

