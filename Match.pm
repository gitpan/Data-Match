
package Data::Match;

#########################################################################

=head1 NAME

Data::Match - Complex data structure pattern matching

=head1 SYNOPSIS

  use Data::Match qw(:all);

  my ($match, $results) = match($structure, $pattern);

=head1 DESCRIPTION

Data::Match provides extensible complex perl data structure searching and matching.

=head1 EXPORT

  None are exported by default.  C<:func> exports C<match> and C<matches>, C<:pat> exports all the pattern element generators below, C<:all> exports C<:func> and C<:pat>.

=head1 PATTERNS

A data pattern is a complex data structure that possibly matches another complex data structure.  For example:

  matches([ 1, 2 ], [ 1, 2 ]); # is true

  matches([ 1, 2, 3 ], [ 1, ANY, 2 ]); # is true

C<ANY> matches anything, including an undefined value.

  my $results = matches([ 1, 2, 1 ], [ BIND('x'), ANY, BIND('x') ]); # is true.

C<BIND($name)> matches anything and remembers each match and its position with every C<BIND($name)> in C<$result->{'BIND'}{$name}>.  If C<BIND($name)> is not the same as the first value bound to C<BIND($name)> it does not match.  For example:

  my $results = matches([ 1, 2, 3 ], [ BIND('x'), 2, BIND('x') ]); # is false: 3 != 1

C<COLLECT($name)> is similar to BIND but does not compare first bound values.

C<REST> matches all remaining elements of an array or hash.

  matches([ 1, 2, 3 ], [ 1, REST() ]); # is true.
  matches({ 'a'=>1, 'b'=>1 }, { 'b'=>1, REST() => REST() }); # is true

C<FIND> searches at all depths for matching subpatterns.

  matches([ 1, [ 1, 2 ], 3], FIND(COLLECT('x', [ 1, REST() ])); # is true.

See the test script C<t/t1.t> in the package distribution for more pattern examples.

=head1 MATCH COLLECTIONS

When a C<BIND> or C<COLLECT> matches a datum an entry is collect in C<$result->{BIND}> and C<$result->{COLLECT}>, respectively.

Each entry for the binding name is a hash containing C<'v'>, C<'p'> and C<'ps'> lists.  
C<'v'> is a list of the value at each match.
C<'p'> is a list describing where the corresponding match was found based on the root of the searchat each match.
C<'ps'> is a list of code strings that describes where the match was for each match.

=head1 CAVEATS

Blessed structures are not traversed.  Circular data structures are not handled properly.  Does not have regex-like operators like '?', '*', '+'.  Should probably have interfaces to Data::DRef and Data::Walker. 

=head1 STATUS

If you find this to be useful please contact the author.  This is beta software; all APIs, semantics and behavors are subject to change.

=head1 INTERNALS

This section describes the components of this module.

=cut

#########################################################################

use strict;
use warnings;

our $VERSION = '0.02';
our $REVISION = do { my @r = (q$Revision: 1.5 $ =~ /\d+/g); sprintf "%d." . "%02d" x $#r, @r };

our $PACKAGE = __PACKAGE__;

use Exporter;
our @ISA = qw(Exporter);
our @EXPORT = qw();

our @export_func = qw(match matches match_path_str match_path_get match_path_ref);
our @autoload_pat = qw(ANY AND OR NOT BIND COLLECT REGEX ISA REST EACH ALL FIND LENGTH EXPR);
our @export_pat = @autoload_pat;
our @EXPORT_OK = (@export_func, @export_pat);
our %EXPORT_TAGS = ( 
		     'all'  => \@EXPORT_OK,
		     'func' => \@export_func,
		     'pat'  => \@export_pat,
		     );

use Data::Dumper;
use Carp qw(confess);

our $debug = 0;


#########################################################################
# Automagically create creator functions for common patterns.
#


our %autoload_pat = map(($_, 1), @autoload_pat);

sub AUTOLOAD
{
  no strict "refs";
  use vars qw($AUTOLOAD);

  my ($pkg, $pat) = $AUTOLOAD =~ /^(.*)::(\w+)$/;

  my ($self) = @_;

  if ( $autoload_pat{$pat} eq 1 ) {
    my $pat_cls = "${pkg}::Pattern::${pat}";
    # $DB::single = 1;
    my $code = eval "sub { new $pat_cls(\@_); }";
    die "$@: PAT=$pat" if $@;
    *{$AUTOLOAD} = $autoload_pat{$pat} = $code;
    #warn "AUTOLOADED $pat_cls";
    #print "AUTOLOAD $AUTOLOAD: ", Data::Dumper->new([ \@_ ], [ qw(@_) ])->Indent(0)->Purity(1)->Terse(0)->Dump(), "\n\n";
    $code->(@_);
  } else {
    $self->SUPER::AUTOLOAD(@_);
    die "no such method: $AUTOLOAD";
  }
}


*OR = \&ANY; # See ANY::match => match_or.


#########################################################################


=head2 _match_ARRAY

Internal recursive match routine.  Assumes $results is initialized.

=cut
sub _match_ARRAY
{
  my ($x, $p, $results) = @_;

  my $match = 0;

  ++ $results->{'depth'};
  push(@{$results->{'path'}}, ref($x));
  
 ARRAY:
  {
    $match = 1;
    
    # Each element must match.
    for my $i ( 0 .. $#$p ) {
      # [ 'x', 'y', REST ] matches [ 'x', 'y', 'z', '1', '2', '3' ]
      if ( ! $results->{'disable_patterns'} && 
	 UNIVERSAL::isa($p->[$i], 'Data::Match::Pattern::REST') ) {
	push(@{$results->{'path'}}, [$i, scalar @$x]);

	# Create an new array slice to match the rest of the array.
	my $slice;
	if ( 1 ) {
	  $slice = new Data::Match::Slice::Array($x, $i, scalar @$x);
	} else {
	  $slice = [ @{$x}[$i .. $#$x] ];
	}

	$match &&= $p->[$i]->_match_rest_array($slice, $results);
	
	pop(@{$results->{'path'}});
	last ARRAY;
      }
      
      push(@{$results->{'path'}}, $i);
      
      $match = $i < @$x && _match($x->[$i], $p->[$i], $results);
      
      pop(@{$results->{'path'}});
      
      last ARRAY unless $match;
    }
    
    $match &&= @$p == @$x;
  }
      
  -- $results->{'depth'};
  pop(@{$results->{'path'}});

  $match;
}



=head2 _match_HASH

Internal recursive match routine.  Assumes $results is initialized.

=cut
sub _match_HASH
{
  my ($x, $p, $results) = @_;

  my $match = 0;

  ++ $results->{'depth'};
  push(@{$results->{'path'}}, ref($x));
	  
 HASH:
  {
    $match = 1;
    
    my $rest_pat;
    my $any_key = 0;
    
    my %matched_keys;

    for my $k ( keys %$p ) {
      # ANY in a pattern key matches any other elements.
      if ( ! $results->{'disable_patterns'} && 
	   $k =~ /^Data::Match::Pattern::ANY=/ ) {
	if ( ! $any_key ++ ) {
	  my $matches = 0;
	  
	  for my $xk ( keys %$x ) {
	    push(@{$results->{'path'}}, $xk);

	    ++ $matched_keys{$xk};
	    ++ $matches if _match($x->{$xk}, $p->{$k}, $results);
	    
	    pop(@{$results->{'path'}});
	    
	    last unless $match;
	  }
	  
	  # Must have at least one match.
	  # { ANY => 'x' } does not match { }.
	  $match &&= $matches;
	}
	next;
      }
      
      # Rest in a pattern causes the rest to match.
      if ( ! $results->{'disable_patterns'} && 
	   ! $rest_pat &&
	 UNIVERSAL::isa($p->{$k}, 'Data::Match::Pattern::REST')
	   ) {
	$rest_pat = $p->{$k};
	next;
      }
      
      # If the key does not exist in pattern, no match.
      push(@{$results->{'path'}}, $k);
      
      ++ $matched_keys{$k};
      $match &&= exists $x->{$k} && _match($x->{$k}, $p->{$k}, $results);
      
      pop(@{$results->{'path'}});
      
      last HASH unless $match;
    }
    
    # Handle REST pattern's subpatterns.
    if ( $rest_pat ) {
      my @rest_keys = grep(! exists $matched_keys{$_}, keys %$x);

      # Create a temporary hash slice containing
      # the values from $x for all the unmatched keys.
      my $slice;
      if ( 1 ) {
	$slice = new Data::Match::Slice::Hash($x, \@rest_keys);
      } else {
	$slice = { };
	@{$slice}{@rest_keys} = @{$x}{@rest_keys};
      }

      # See match_path_str().
      push(@{$results->{'path'}}, \@rest_keys);

      #$DB::single = 1;
      $match &&= $rest_pat->_match_rest_hash($slice, $results);

      pop(@{$results->{'path'}});
    }

    # Make sure they are the same length.
    $match &&= (scalar values %$p) == (scalar values %$x) unless $rest_pat or $any_key;
  }

  -- $results->{'depth'};
  pop(@{$results->{'path'}});

  $match;
}


=head2 _match_SCALAR

Internal recursive match routine.  Assumes $results is initialized.

=cut
sub _match_SCALAR
{
  my ($x, $p, $results) = @_;

  my $match = 0;

  ++ $results->{'depth'};
  push(@{$results->{'path'}}, ref($x));
	  
  push(@{$results->{'path'}}, undef); # 'emacs
  $match = _match($$x, $$p, $results);
  pop(@{$results->{'path'}});

  -- $results->{'depth'};
  pop(@{$results->{'path'}});

  $match;
};



=head2 _match

Internal recursive match routine.  Assumes $results is initialized.

=cut
sub _match
{
  my ($x, $p, $results) = @_;

  my $match = 0;

  # $DB::single = 1;

  RESULT: 
  {
    no warnings;

    # Is it simply the same?
    if ( $x eq $p ) {
      $match = 1;
    }

    # Is pattern a pattern?
    elsif ( ! $results->{'disable_patterns'} && UNIVERSAL::isa($p, 'Data::Match::Pattern') ) {
      # Delegate match to pattern object.
      $match = $p->match($x, $results);
    }

    # Handle deep structures.
    elsif ( ref($x) ) {
      if ( ref($x) eq 'ARRAY' && ref($p) eq 'ARRAY' ) {
	$match = _match_ARRAY($x, $p, $results);
      }
      elsif ( ref($x) eq 'HASH' && ref($p) eq 'HASH' ) {
	$match = _match_HASH($x, $p, $results);
      }
      elsif ( ref($x) eq 'SCALAR' && ref($p) eq 'SCALAR' ) {
	$match = _match_SCALAR($x, $p, $results);
      }
      else {
	# Extensible comparators?
	if ( my $comparator = $results->{'compare'}{ref($x) || '*'} ) {
	  # Try a comparator.
	  $match = $comparator->($x, $p, $results);
	} else {
	  # Default to eq.
	  $match = $x eq $p;
	}
      }
    } else {
      # Scalar eq.
      $match = $x eq $p;
    }
  };

  #$DB::single = 1;

  $match;
}


=head2 %match_opts

Default options for C<match>.

=cut
our %match_opts;

=head2 match

Matches a structure against a pattern.  In a list context, return both the match success and results; in a scalar context returns the results hash if match succeeded or undef.

  my ($matched, $results) = match($thing, $p);
  $results = match($thing, $p);

=cut
sub match
{
  my ($x, $p, $results, %opts) = @_;

  # Initialize a container for the match results.
  $results ||= { };

  # Install opts.
  @{$results}{keys %match_opts} = values %match_opts;
  @{$results}{keys %opts} = values %opts;

  # Initialize state.
  $results->{'depth'} ||= 0;
  $results->{'path'}  ||= [ ],
  $results->{'root'}  ||= $x,

  # Start matching.
  my $matches = _match($x, $p, $results);

  # Post conditions.
  {
    no warnings;

    confess "Expected results->{depth} == 0" unless $results->{'depth'} == 0;
    confess "Expected results->{path} eq [ ]" unless ! @{$results->{'path'}};
    confess "Expected results->{root} eq root" unless $results->{'root'} eq $x;
  }

  # Return results.
  if ( wantsarray ) {
    return ($matches, $results);
  } else {
    return $matches ? $results : undef;
  }
}


=head2 matches

Same as C<match> in scalar context.

=cut
sub matches
{
  my ($x, $p, $r, @opts) = @_;

  my ($match, $results) = match($x, $p, $r, @opts);

  $match ? $results : undef;
}



##################################################
# Path support
#


=head2 match_path_get

Returns the value pointing to the location for the match path in the root.

  my $results = matches($thing, FIND(BIND('x', [ 'x', REST ])));
  my $x = match_path_get($thing, $results->{'BIND'}{'x'}{'path'}[0]);

The above example returns.

=cut
sub match_path_get
{
  my ($path, $root, $results) = @_;

  my $ps = match_path_str($path, '$_[0]', $results);

  # warn "ps = $ps" if ( 1 || $ps =~ /,/ );

  my $pfunc = eval "sub { $ps; }";
  die "$@: $ps" if $@;

  $pfunc->($root);
}



=head2 match_path_set

Returns the value pointing to the location for the match path in the root.

  my $results = matches($thing, FIND(BIND('x', [ 'x', REST ])));
  match_path_set($thing, $results->{'BIND'}{'x'}{'path'}[0], 'y');

The above example replaces the first array found that starts with 'x' with 'y';

=cut
sub match_path_set
{
  my ($path, $root, $value, $results) = @_;

  my $ps = match_path_str($path, '$_[0]', $results);

  # warn "ps = $ps" if ( 1 || $ps =~ /,/ );

  my $pfunc = eval "sub { $ps = \$_[1]; }";
  die "$@: $ps" if $@;

  $pfunc->($root, $value);
}


=head2 match_path_ref

Returns a scalar ref pointing to the location for the match path in the root.

  my $results = matches($thing, FIND(BIND('x', [ 'x', REST ])));
  my $ref = match_path_ref($thing, $results->{'BIND'}{'x'}{'path'}[0]);
  $$ref = 'y';

The above example replaces the first array that starts with 'x' with 'y';

=cut
sub match_path_ref
{
  my ($path, $root, $results) = @_;

  my $ps = match_path_str($path, '$_[0]', $results);

  # warn "ps = $ps" if ( 1 || $ps =~ /,/ );

  my $pfunc = eval "sub { \\{$ps}; }";
  die "$@: $ps" if $@;

  $pfunc->($root);
}


=head2 match_path_str

Returns a perl expression that will generate code to point to the element of the path.

=cut
sub match_path_str
{
  my ($path, $str, $results) = @_;

  $str ||= '$_';

  my @path = @$path;

  while ( @path ) {
    my $ref = shift @path;
    my $ind = shift @path;

    if ( $ref eq 'ARRAY' ) {
      if ( ref($ind) eq 'ARRAY' ) {
	# Create a temporary array slice.
	$str = "(Data::Match::Slice::Array->new($str,$ind->[0],$ind->[1]))";
      } else {
	$str .= "->[$ind]";
      }
    }
    elsif ( $ref eq 'HASH' ) {
      if ( ref($ind) eq 'ARRAY' ) {
	# Create a temporary hash slice.
	my $elems = join(',', map('"' . quotemeta($_) . '"', sort @$ind));
	$str = "(Data::Match::Slice::Hash->new($str,[$elems]))";
      } else {
	$ind = '"'. quotemeta($ind) . '"';
	$str .= "->{$ind}";
      }
    }
    elsif ( $ref eq 'SCALAR' ) {
      # Maybe there is a better -> syntax?
      $str = "(\${$str})";
    }
    elsif ( $ref eq 'METHOD' ) {
      if ( ref($ind) eq 'ARRAY' ) {
	my @args = @$ind;
	my $method = shift @args;
	
	$str = $str . "->$method(" . join(',', map('"'. quotemeta($_) . '"', @args)) . ')';
      } else {
	confess "AIIIEE!!";
      }
    }
    else {
      $str = undef;
    }
  }

  $str;
}


##################################################


package Data::Match::Pattern;

use Carp qw(confess);


sub new
{
  my ($cls, @args) = @_;
  # $DB::single = 1;
  (bless \@args, $cls)->initialize->_is_valid;
}


sub initialize { shift; }


sub _is_valid
{
  my $self = shift;

  confess("INVALID " . ref($self) . ": expect at least " . $self->subpattern_offset . " elements")
    unless @$self >= $self->subpattern_offset;

  $self;
}


sub subpattern_offset { 0; }

sub match_and
{
  my ($self, $x, $results) = @_;

  for my $i ( $self->subpattern_offset .. $#$self ) {
    return 0 unless Data::Match::_match($x, $self->[$i], $results);
  }

  1;
}


sub match_or
{
  my ($self, $x, $results) = @_;

  for my $i ( $self->subpattern_offset .. $#$self ) {
    return 1 if Data::Match::_match($x, $self->[$i], $results);
  }

  0;
}


*match = \&match_and;


##################################################

package Data::Match::Pattern::AND;

our @ISA = qw(Data::Match::Pattern);


##################################################

package Data::Match::Pattern::NOT;

our @ISA = qw(Data::Match::Pattern);

sub match
{
  my ($self, $x, $results) = @_;

  # $DB::single = 1;
  ! ((scalar @$self) ? $self->match_and($x, $results) : $x);
}


##################################################

package Data::Match::Pattern::ANY;

our @ISA = qw(Data::Match::Pattern);

sub match 
{
  my ($self, $x, $results) = @_;

  #$DB::single = 1;
  # ANY always matches.

  if ( @{$self} ) {
    # Do subpatterns.
    $self->match_or($x, $results);
  } else {
    1;
  }
}


##################################################

package Data::Match::Pattern::COLLECT;

#use Data::Match qw(match_path_str);

our @ISA = qw(Data::Match::Pattern);

sub subpattern_offset { 1; };

sub binding { $_[0]->[0]; };

sub _collect
{
  my ($self, $x, $results, $binding) = @_;

  push(@{$binding->{'v'}}, $x );
  my $path = [ @{$results->{'path'}} ];
  push(@{$binding->{'p'}}, $path) ;# if ( $results->{'collect_path'} );
  push(@{$binding->{'ps'}}, Data::Match::match_path_str($path, undef, $results)) if ( $results->{'collect_pathstr'} );
}


sub match 
{ 
  my ($self, $x, $results) = @_;

  # warn "MATCH($self->[0])";

  # $DB::single = 1;
  
  # Do subpatterns.
  return 0 unless $self->match_and($x, $results);

  my $binding = $results->{'COLLECT'}{$self->[0]} ||= { };

  $self->_collect($x, $results, $binding);

  #$DB::single = 1;
  1;
}


##################################################


package Data::Match::Pattern::BIND;

use Data::Compare;

our @ISA = qw(Data::Match::Pattern::COLLECT);

sub subpattern_offset { 1; };

sub binding { $_[0]->[0]; };

sub match 
{ 
  my ($self, $x, $results) = @_;

  # warn "MATCH($self->[0])";

  # $DB::single = 1;

  # Do subpatterns.
  return 0 unless $self->match_and($x, $results);

  my $binding = $results->{'BIND'}{$self->[0]};

  if ( $binding ) {
    #$DB::single = 1;
    if ( Compare($binding->{'v'}[0], $x) ) {
      $self->_collect($x, $results, $binding);
    } else {
      return 0;
    }
  } else {
    $self->_collect($x, $results, $results->{'BIND'}{$self->[0]} = {});
  }

  1;
}


##################################################


package Data::Match::Pattern::REGEX;

our @ISA = qw(Data::Match::Pattern);

sub subpattern_offset { 1; };

sub match 
{
  my ($self, $x, $results) = @_;

  # $DB::single = 1;
  
  # Note: do not check that it is not a ref incase the object can be coerced into a string.
  ($x =~ /$self->[0]/sx) && $self->match_and($x, $results); 
}


##################################################


package Data::Match::Pattern::ISA;

our @ISA = qw(Data::Match::Pattern);

sub subpattern_offset { 1; };

sub match 
{
  my ($self, $x, $results) = @_;

  UNIVERSAL::isa($x, $self->[0]) and $self->match_and($x, $results);
}


##################################################


package Data::Match::Pattern::LENGTH;

our @ISA = qw(Data::Match::Pattern);

sub subpattern_offset { 0; };

sub match 
{
  my ($self, $x, $results) = @_;

  if ( ref($x) ) {
    if ( ref($x) eq 'ARRAY' ) {
      $x = @$x;
    }
    elsif ( ref($x) eq 'HASH' ) {
      $x = %$x;
    }
    else {
      $x = undef;
    }
  } else {
    $x = length $x;
  }

  @$self ? $self->match_and($x, $results) : $x;
}


##################################################


package Data::Match::Pattern::EXPR;

use Carp qw(confess);

our @ISA = qw(Data::Match::Pattern);

sub subpattern_offset { 2; };


sub initialize
{
  my $self = shift;

  # $DB::single = 1;

  # Make room for EXPR.
  splice(@$self, 1, 0, 'UGH');

  if ( ref($self->[0]) eq 'CODE' ) {
    $self->[1] = $self->[0];
  } else {
    my $expr = $self->[0];
    $self->[1] = eval "sub { local \$_ = \$_[0]; $expr; }";
    confess "$@: $expr" if $@;
  }

  $self;
}


sub match 
{
  my ($self, $x, $results) = @_;

  $DB::single = 1;

  $self->[1]->($x, $results, $self) && $self->match_and($x, $results);
}


##################################################


package Data::Match::Pattern::REST;

our @ISA = qw(Data::Match::Pattern);


sub match
{
  # Should only match in an array or hash context.
  0;
}


sub _match_rest_array
{
  my ($self, $x, $results) = @_;

  ref($x) && $self->match_and($x, $results);
}


sub _match_rest_hash
{
  my ($self, $x, $results) = @_;

  ref($x) && $self->match_and($x, $results);
}


##################################################


package Data::Match::Pattern::EACH;

our @ISA = qw(Data::Match::Pattern);


sub _match_each_array
{
  my ($self, $x, $results, $matches) = @_;

  ++ $results->{'depth'};
  push(@{$results->{'path'}}, ref($x));
  
  my $i = -1;
  for my $e ( @$x ) {
    push(@{$results->{'path'}}, ++ $i);
    ++ $$matches if $self->match_and($e, $results);
    pop(@{$results->{'path'}});
  }
  
  pop(@{$results->{'path'}});
  -- $results->{'depth'};
}


sub _match_each_hash
{
  my ($self, $x, $results, $matches) = @_;

  ++ $results->{'depth'};
  push(@{$results->{'path'}}, ref($x));
  
  for my $k ( keys %$x ) {
    my @k = ( $k );

    # We compensate the path for hash slice.
    push(@{$results->{'path'}}, \@k);
    
    # Create a temporary hash slice.
    # because we are matching EACH element of the hash.
    my $slice;
    if ( 1 ) {
      $slice = new Data::Match::Slice::Hash($x, \@k);
    } else {
      $slice = { $k => $x->{$k} };
    }

    ++ $$matches if $self->match_and($slice, $results);
    
    pop(@{$results->{'path'}});
  }
  
  pop(@{$results->{'path'}});
  -- $results->{'depth'};
}


sub _match_each_scalar
{
  my ($self, $x, $results, $matches) = @_;

  ++ $results->{'depth'};
  push(@{$results->{'path'}}, ref($x), undef);
  
  ++ $$matches if $self->match_and($$x, $results);
  
  pop(@{$results->{'path'}});
  pop(@{$results->{'path'}});
  -- $results->{'depth'};
}


sub _match_each
{
  my ($self, $x, $results, $matches) = @_;

  # Traverse.
  if ( ref($x) ) {
    if ( ref($x) eq 'ARRAY' ) {
      $self->_match_each_array($x, $results, $matches);
    }
    elsif ( ref($x) eq 'HASH' ) {
      $self->_match_each_hash($x, $results, $matches);
    }
    elsif ( ref($x) eq 'SCALAR' ) {
      $self->_match_each_scalar($x, $results, $matches);
    }
    else {
      # Try to match it explicitly.
      ++ $$matches if $self->match_and($x, $results);
    }
  }
}


sub match
{
  my ($self, $x, $results) = @_;

  my $matches = 0;

  $self->_match_each($x, $results, \$matches);

  $matches;
}


##################################################


package Data::Match::Pattern::ALL;

our @ISA = qw(Data::Match::Pattern::EACH);


sub match
{
  my ($self, $x, $results) = @_;

  my $matches = 0;

  my $expected = $self;

  if ( ref($x) eq 'ARRAY' ) {
    $expected = scalar @$x;
  }
  elsif ( ref($x) eq 'HASH' ) {
    $expected = scalar %$x;
  } else {
    $expected = -1;
  }

  $self->_match_each($x, $results, \$matches);

  $matches == $expected;
}



##################################################


package Data::Match::Pattern::FIND;

our @ISA = qw(Data::Match::Pattern);

sub _match_find
{
  my ($self, $x, $results, $matches) = @_;

  # Does this match directly? 
  if ( $self->match_and($x, $results) ) {
    ++ $$matches;
  }

  # Traverse.
  if ( ref($x) ) {
    # $DB::single = 1;

    if ( ref($x) eq 'ARRAY' ) {
      ++ $results->{'depth'};
      push(@{$results->{'path'}}, ref($x));

      my $i = -1;
      for my $e ( @$x ) {
	push(@{$results->{'path'}}, ++ $i);

	$self->_match_find($e, $results, $matches);

	pop(@{$results->{'path'}});
      }

      pop(@{$results->{'path'}});
      -- $results->{'depth'};
    }
    elsif ( ref($x) eq 'HASH' ) {
      ++ $results->{'depth'};
      push(@{$results->{'path'}}, ref($x));

      for my $k ( keys %$x ) {
	my $v = $x->{$k};
	push(@{$results->{'path'}}, $k);

	$self->_match_find($k, $results, $matches);

	pop(@{$results->{'path'}});
      }

      pop(@{$results->{'path'}});
      -- $results->{'depth'};
    }
    elsif ( ref($x) eq 'SCALAR' ) {
      ++ $results->{'depth'};
      push(@{$results->{'path'}}, ref($x));
      push(@{$results->{'path'}}, undef);

      $self->_match_find($$x, $results, $matches);

      pop(@{$results->{'path'}});
      pop(@{$results->{'path'}});
      -- $results->{'depth'};
    }
    else {
      # Extensible traveral.
      if ( my $traverser = $results->{'traverse'}{ref($x) || '*'} ) {
	++ $results->{'depth'};
	push(@{$results->{'path'}}, 'X', ref($x));
  
	$traverser->($x, sub { $self->match_and($_[0], $results); });

	pop(@{$results->{'path'}});
	pop(@{$results->{'path'}});
	-- $results->{'depth'};
      }
    }
  }
}


sub match
{
  my ($self, $x, $results) = @_;

  my $matches = 0;

  $self->_match_find($x, $results, \$matches);

  $matches;
}


#################################################


package Data::Match::Slice::Array;

our $debug = 0;

sub new
{
  my $cls = shift;
  my @x;
  tie @x, $cls, @_;
  \@x;
}


sub TIEARRAY
{
  my ($cls, $src, $from, $to) = @_;
  $DB::single = $debug;
  $from = 0 unless defined $from;
  $to = @$src unless defined $to;
  die "slice must be $from <= $to" unless $from <= $to;
  bless [ $src, $from, $to ], $cls;
}

sub FETCH 
{
  my $i = $_[1];
  $DB::single = $debug;
  $i = FETCHSIZE($_[0]) - $i if $i < 0;
  0 <= $i && $i < FETCHSIZE($_[0])
    ? $_[0][0]->[$_[0][1] + $i] 
    : undef;
}
sub STORE 
{
  $DB::single = $debug;
  STORESIZE($_[0], $_[1] + 1) if ( $_[1] >= $_[0][1] );
  $_[0][0]->[$_[0][1] + $_[1]] = $_[2];
}
sub FETCHSIZE 
{
  $DB::single = $debug;
  $_[0][2] - $_[0][1];
}
sub STORESIZE 
{
  $DB::single = $debug;
  if ( $_[1] > FETCHSIZE($_[0]) ) {
    PUSH($_[0], (undef) x (FETCHSIZE($_[0]) - $_[1]));
  } else {
    SPLICE($_[0], 0, $_[1]);
  }
  $_[0][2] = $_[0][1] + $_[1];
}
sub POP 
{
  $DB::single = $debug;
  $_[0][2] > $_[0][1] ? splice(@{$_[0][0]}, -- $_[0][2], 1) : undef;
}
sub PUSH 
{
  my $s = shift;
  my $o = $s->[2];
  $s->[2] += scalar(@_);
  splice(@{$s->[0]}, $s->[2], $o, @_); 
}
sub SHIFT 
{ 
  $DB::single = $debug;
  $_[0][1] < $_[0][2]
    ? splice(@{$_[0][0]}, $_[0][1] ++, 1)
    : undef;
}
sub UNSHIFT 
{ 
  $DB::single = $debug;
  my $s = shift;
  $_[0][2] += scalar @_;
  splice(@{$s->[0]}, $_[0][1], 0, @_);
}
sub SPLICE 
{
  $DB::single = $debug;
  my $s = shift;
  my $o = shift;
  my $l = shift;
  $_[0][2] += @_ - $l;
  splice(@{$_[0][0]}, $_[0][1] + $o, $l, @_);
}
sub DELETE 
{ 
  $DB::single = $debug;
  0 <= $_[1] && $_[1] < FETCHSIZE($_[0]) && delete $_[0][0][$_[0][1] + $_[1]];
}
sub EXTEND
{ 
  $DB::single = $debug;
  $_[0][0];
}
sub EXISTS 
{ 
  $DB::single = $debug;
  0 <= $_[1] && $_[1] < FETCHSIZE($_[0]) && defined $_[0][0][$_[0][1] + $_[1]];
}


#########################################################################


package Data::Match::Slice::Hash;

our $debug = 0;

sub new
{
  my $cls = shift;
  my %x;
  tie %x, $cls, @_;
  \%x;
}


sub TIEHASH
{
  my ($cls, $src, $keys) = @_;
  $DB::single = $debug;
  bless [ $src, { map(($_, $_), @$keys) } ], $cls;
}


sub FETCH 
{
  $DB::single = $debug;
  $_[0][1]->{$_[1]} ? $_[0][0]->{$_[1]} : undef;
}
sub STORE 
{ 
  $DB::single = $debug;
  $_[0][1]->{$_[1]} = 1;
  $_[0][0]->{$_[1]} = $_[2];
}
sub DELETE 
{ 
  $DB::single = $debug;
  if ( exists $_[0][1]->{$_[1]} ) {
    delete $_[0][1]->{$_[1]}; 
    delete $_[0][0]->{$_[1]};
  }
}
sub CLEAR 
{ 
  $DB::single = $debug;
  for my $k ( keys %{$_[0][1]} ) { 
    delete $_[0][0]->{$k} 
  }; 
  %{$_[0][1]} = ();
}
sub EXISTS 
{ 
  $DB::single = $debug;
  exists $_[0][1]->{$_[1]};
}
sub FIRSTKEY 
{ 
  $DB::single = $debug;
  each %{$_[0][1]}; 
}
sub NEXTKEY 
{ 
  $DB::single = $debug;
  each %{$_[0][1]};
}



#########################################################################

=head1 VERSION

Version 0.02, $Revision: 1.5 $.

=head1 AUTHOR

Kurt A. Stephens <kurtstephens@acm.org>

=head1 SEE ALSO

L<perl>, L<Data::Compare>, L<Data::Dumper>, L<Data::DRef>, L<Data::Walker>.

=cut

##################################################

1;
