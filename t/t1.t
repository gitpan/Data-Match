#!/usr/bin/perl

# $Id: t1.t,v 1.1 2001/12/24 02:01:18 stephens Exp $

use strict;
use Test;

my @tests;
my $n_tests;
my $verbose = 0;

BEGIN { 
  print join(', ', @ARGV), "\n";

my $tests = q{
    match(undef, undef);
  ! match(1, undef);
    match(1, 1);
  
    match('x', 'x');
  ! match('x', 'y');
  ! match('x', 0);
    match('x', BIND('x'));

    match([], []);
    match([ 1 ], [ 1 ]);
  ! match([ 1 ], [ ]);
  ! match([ ],   [ 1 ]);

    match({}, {});
    match({ 'a' => 1 }, { 'a' => 1 });
  ! match({ },          { 'a' => 1 });
  ! match({ 'a' => 1 }, { });
  ! match({ 'a' => 1 }, { 'b' => '1' });
  ! match({ 'a' => 1 }, { 'a' => 2 });

  ! match([ 1, 2 ], [ 1 ]);
  ! match([ 1 ],    [ 1, 2 ]);

    match([ 1, 2, 3 ], BIND('x'));
    match([ 1, 2, 3 ], [ BIND('x'), BIND('y'), 3 ]);
  ! match([ 1, 2, 3 ], [ BIND('x'), BIND('y'), BIND('x') ]);
    match([ 1, 2, 1 ], [ BIND('x'), ANY(), BIND('x') ]);
  ! match([ 1, 2, 4 ], [ BIND('x'), ANY(), BIND('x') ]);
    match([ 1, [ 1, 2, 1 ], 2 ], [ ANY(), [ BIND('x'), ANY, BIND('x') ], REST ]);

    match([ 1, 2, 3 ], COLLECT('x'));
    match([ 1, 2, 3 ], [ COLLECT('x'), ANY, COLLECT('x') ]);
  ! match([ 1, 2, 3 ], [ COLLECT('x'), 5, COLLECT('x') ]);

    match([ 1, 2, 4 ], ISA('ARRAY'));
  ! match([ 1, 2, 4 ], ISA('HASH'));

    match([ 'foo', 'bar', 'baz' ], EACH(COLLECT('x', REGEX(q{z$}))));
  ! match([ 'foo', 'bar', 'baz' ], EACH(COLLECT('x', REGEX(q{^foobar$}))));

    match([ 1, 2, 4 ], BIND('x', ISA('ARRAY')));
  ! match([ 1, 2, 4 ], BIND('x', ISA('HASH')));

  ! match([],           REST() );
    match([],          [ REST() ] );
    match([ 1, 2 ],    [ REST() ]);
    match([ 1, 2, 3 ], [ 1, 2, REST() ]);
    match([ 1, 2, 3, 4 ], [ 1, 2, REST(BIND('r')) ]);

  ! match({}                 , REST );
    match({}                 , { REST() => REST(BIND('x')) } );
    match({ 'a'=>1, 'b'=>2 } , { REST() => REST(BIND('x')) } );
    match({ 'a'=>1, 'b'=>2, 'c'=>3 }, { 'a'=>1, 'b'=>2, REST() => REST(BIND('x')) });
  ! match({ 'a'=>1, 'b'=>2, 'c'=>3 }, { 'a'=>1, 'd'=>4, REST() => REST(BIND('x')) });

    match([ 1, 2, 3 ], EACH(COLLECT('x', 2)));
  ! match([ 1, 2, 3 ], EACH(COLLECT('x', 5)));
    match([ 1, 2, 3, 2, 5 ], EACH(COLLECT('x', 2)));

    match({ 'a'=>1, 'b'=>2, 'c'=>3 }, EACH(COLLECT('x', { 'c' => 3 })));
    match({ 'a'=>1, 'b'=>2, 'c'=>3 }, EACH(COLLECT('x', { ANY() => 3 })));
  ! match({ 'a'=>1, 'b'=>2, 'c'=>3 }, EACH(COLLECT('x', { ANY() => 5 })));
    match({ 'a'=>1, 'b'=>2, 'c'=>3, 'd'=>2, 'e'=>5 }, EACH(COLLECT('x', { ANY() => 2})));

    match([ 1, 2, 3 ], FIND(COLLECT('x', 2)));
  ! match([ 1, 2, 3 ], FIND(COLLECT('x', 5)));
    match([ 1, 2, 3, 2, 5 ], FIND(COLLECT('x', 2)));
    match([ 1, 2, [ 1, 2, [ 1, 2 ] ], 3 ],   FIND(COLLECT('x', 2)));
    match([ 1, 2, [ 1, 2, [ 1, 2 ] ], 3 ],   FIND(COLLECT('x', [ 1, 2 ])));
    match([ 1, 2, [ 'a', 2, [ 1, 2 ] ], 3 ], FIND(COLLECT('x', [ 1, REST ])));
  ! match([ 1, 2, [ 1, 2, [ 1, 2 ] ], 3 ], FIND(COLLECT('x', 5)));
  ! match([ 1, 2, [ 1, 2, [ 1, 2 ] ], 3 ], FIND(COLLECT('x', 5)));
  };
#)emacs

  @tests = split(';', $tests);
  grep(s/^\s+//, @tests);
  @tests = grep(length $_, @tests);
  $n_tests = @tests;
  warn "n_tests=$n_tests";

  plan tests => $n_tests;
};

use Data::Match qw(:all);


sub UNIT_TEST
{
  my $n_passed = 0;

  use Data::Dumper;

  #$debug = 1;
  #$DB::single = 1;

  for my $test ( @tests ) {
    my ($tv, $invert, $expr) = ( $test =~ /^\s*(V)?\s*(!)?\s*(.*)\s*$/);
    next unless length $expr;

    # A "V" before the test case forces
    # printing of the test case and result, 
    # regardless of $verbose.
    $tv ||= $verbose;

    # $DB::single = 1;
    eval "no warnings; (\$main::matched, \$main::results) = $expr";
    die "$@: $expr" if $@;

    my ($matched, $results) = ($main::matched, $main::results);
    my $passed = $matched;
    $passed = ! $passed if $invert;

    # Remove redundant results from display.
    if ( $results ) {
      # Validate the BIND and COLLECT values and paths.
      for my $ck ( 'BIND', 'COLLECT' ) {
	if ( my $c = $results->{$ck} ) {
	  for my $k ( keys %$c ) {
	    my $e = $c->{$k};
	    for my $i ( 0 .. $#{$e->{'v'}} ) {
	      my $v = $e->{'v'}[$i];
	      my $p = $e->{'p'}[$i];

	      my $ps = match_path_str($p, '$_[0]', $results);
	      my $pv = match_path_get($p, $results->{'root'}, $results);

	      my $path_ok = matches($pv, $v);
	      print "  PATH $ps ($pv) ne $v\n" if ! $path_ok || $tv;

	      $passed &&= $path_ok;
	    }
	  }
	}
      }

      delete $results->{'depth'};
      delete $results->{'path'};
      delete $results->{'root'};
    }


    # Print results?.
    if ( $tv || ! $passed ) {
      print '  ', $test, "\n";
      print " return ", Data::Dumper->new([ $matched, $results ], [ '$matched', '$results' ])->Indent(0)->Purity(1)->Terse(1)->Dump(), "\n\n";
    }

    # Did it pass?
    ok($passed);

    ++ $n_passed if $passed;
  }

  print STDERR "\n$n_passed passed / $n_tests tests \n";

  $n_passed == $n_tests ? 0 : 1;
}


UNIT_TEST();

1;
