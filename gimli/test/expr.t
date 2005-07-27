#!/usr/bin/perl

use warnings;
use strict;

use Test::More tests => 4;

BEGIN { unshift @INC, 'test/lib'; }
use RunGimli;

sub evals_ok {
    no warnings 'once';
    my ($expr, $expected_result) = @_;
    my $test_fn = ref $expected_result ? *Test::More::like : *Test::More::is;
    my $result = run_gimli($expr);
    for ($result) { s/^\s+//s; s/\s+$//s; }  # trim whitespace
    $test_fn->($result, $expected_result, "$expr ==> $result");
}

sub evals_same_ok {
    my ($expr) = @_;
    evals_ok($expr, $expr);
}

evals_same_ok( "1" );
evals_same_ok( "1234" );
evals_same_ok( "-1" );
evals_same_ok( "-1234" );
