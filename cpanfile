requires 'perl', '5.008001';
requires 'boolean';
requires 'List::Util';

on 'test' => sub {
    requires 'Test::More', '0.98';
    requires 'Test::Fatal';
};

