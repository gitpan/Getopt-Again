#!/usr/bin/perl
use strict;
use warnings;

use Test::More;

my $CLASS;
BEGIN {
    $CLASS = 'Getopt::Again';
    require_ok $CLASS;
}

{
    package My::CLI;
    use Test::More;
    BEGIN { Getopt::Again->import(enable_help => 1) }

    can_ok(__PACKAGE__, qw/
        opt_meta opt_bools opt_lists opt_params opt_paths opt_files
        opt_add opt_usage opt_help opt_parse opt_use_help
    /);

    isa_ok(__PACKAGE__->opt_meta, $CLASS);

    opt_bools qw/foo bar/;
    opt_lists qw/stuff more_stuff/;
    opt_params qw/alpha beta/;
    opt_paths qw/dest origin/;
    opt_files qw/config state/;

    our $BAZ = 0;
    opt_add baz => sub { $BAZ++; $_ };

    opt_add bat => qr/bat/;

    our $COMP;
    opt_add comp => (
        type => 'string',
        list => 1,
        split_on => ',',
        alias => [ 'compute', 'stringy' ],
        process => sub { $COMP .= "Got: $_\n" for @{$_}; [ map { "X$_" } @{$_[0]} ] },
        description => "Computate this\n AND THESE\nOh... or those!",
    );

    opt_add laz => (
        type    => 'bool',
        default => 1,
    );

    opt_add ixe => (
        type    => 'bool',
        default => 1,
    );

    opt_add zapa => (
        type => 'regex',
        regex => qr/^zapa-(.+)$/,
    );

    opt_add zapb => (
        type => 'regex',
        list => 1,
        regex => qr/^zapb-(.+)$/,
    );

    opt_add zapc => (
        type => 'regex',
        regex => qr/^zapc-/,
    );

    my($opts, $args) = opt_parse(
        '-f', '--bar', '--laz',
        '--stuff=apple', '--stuff' => 'pear', '--stuff="cherry"', "--stuff='melon'",
        '-a' => 'yup',
        '--beta' => 'something',
        '--dest' => './',
        '--config' => __FILE__,
        '--baz' => 5, '--baz' => 22,
        '--bat' => 'batman',
        '--comp' => 'first', '--stringy' => 'second,third,fourth',
        '--zapa-foo', '--zapa-bar',
        '--zapb-foo', '--zapb-bar', '--zapb' => 'freeform',
        '--zapc-foo',

        '--',

        '--comp' => 'xxx',
        'some', 'args',
    );

    ok($BAZ, "Callback hit");
    is($COMP, "Got: first\nGot: second\nGot: third\nGot: fourth\n", "comp callback");

    is_deeply(
        $args,
        [qw/--comp xxx some args/],
        "Got args"
    );

    is_deeply(
        $opts,
        {
            'foo' => 1, 'bar' => 1, 'laz' => 0, 'ixe' => 1, # Bools
            stuff => [ qw/apple pear cherry melon/ ],
            alpha => 'yup', beta => 'something',
            dest => './',
            config => __FILE__,
            baz => 22,
            bat => 'batman',
            comp => [qw/Xfirst Xsecond Xthird Xfourth/],
            zapa => 'bar',
            zapb => [ 'foo', 'bar', 'freeform' ],
            zapc => 'foo',

            # Unspecified
            help       => undef,
            origin     => undef,
            state      => undef,
            more_stuff => [],
        },
        "Parsed correctly",
    );

    {
        my $exited = 'unset';
        no warnings 'redefine';
        local *Getopt::Again::_exit = sub { $exited = $_[1] };
        opt_parse('--help');
        is($exited, 0, "exited 0 after help");
    }

    my $ok = eval { opt_parse( '--bat' => 'xxx' ); 1};
    my $error = $@;
    ok(!$ok, "Threw exception");
    is($error, "Invalid value for 'bat', got: 'xxx'\n", "correct exception");

    is(opt_usage(), "$0 [-short_flags] [--long_flag] [--option '...'] [--option=...] arg1, arg2", "got usage");

    opt_usage("foo");
    is(opt_usage(), "$0 foo", "set usage");

    is(opt_meta()->opt_help_string('alpha'), <<'    EOT', "got help for alpha");
  alpha (string)
   --alpha "foo"
   -a "foo"

    EOT

    like(opt_help(), qr/alpha \(string\)/, "got alpha");
    like(opt_help(), qr/dest \(path\)/, "got dest");
    like(opt_help(), qr/zapa \(regex\)/, "got zapa");
}

done_testing;


