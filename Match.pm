
package Data::Match;

#########################################################################

=head1 NAME

Data::Match - Complex data structure pattern matching

=head1 SYNOPSIS

  use Data::Match qw(:all);
  my ($match, $results) = match($structure, $pattern);

  use Data::Match;
  my $obj = new Data::Match;
  my ($match, $results) = $obj->execute($structure, $pattern);

=head1 DESCRIPTION

Data::Match provides extensible complex Perl data structure searching and matching.

=head1 EXPORT

None are exported by default.  C<:func> exports C<match> and C<matches>, C<:pat> exports all the pattern element generators below, C<:all> exports C<:func> and C<:pat>.

=head1 PATTERNS

A data pattern is a complex data structure that possibly matches another complex data structure.  For example:

  matches([ 1, 2 ], [ 1, 2 ]); # TRUE

  matches([ 1, 2, 3 ], [ 1, ANY, 3 ]); # TRUE

  matches([ 1, 2, 3 ], [ 1, ANY, 2 ]); # FALSE: 3 != 2

C<ANY> matches anything, including an undefined value.

  my $results = matches([ 1, 2, 1 ], [ BIND('x'), ANY, BIND('x') ]); # TRUE

C<BIND($name)> matches anything and remembers each match and its position with every C<BIND($name)> in C<$result->{'BIND'}{$name}>.  If C<BIND($name)> is not the same as the first value bound to C<BIND($name)> it does not match.  For example:

  my $results = matches([ 1, 2, 3 ], [ BIND('x'), 2, BIND('x') ]); # FALSE: 3 != 1

C<COLLECT($name)> is similar to BIND but does not compare first bound values.

C<REST> matches all remaining elements of an array or hash.

  matches([ 1, 2, 3 ], [ 1, REST() ]); # TRUE
  matches({ 'a'=>1, 'b'=>1 }, { 'b'=>1, REST() => REST() }); # TRUE

C<FIND> searches at all depths for matching subpatterns.

  matches([ 1, [ 1, 2 ], 3], FIND(COLLECT('x', [ 1, REST() ])); # is true.

See the test script C<t/t1.t> in the package distribution for more pattern examples.

=head1 MATCH COLLECTIONS

When a C<BIND> or C<COLLECT> matches a datum, an entry is collected in C<$result->{BIND}> and C<$result->{COLLECT}>, respectively.  (This might change in the future)

Each entry for the binding name is a hash containing C<'v'>, C<'p'> and C<'ps'> lists.  
C<'v'> is a list of the value at each match.
C<'p'> is a list describing where the corresponding match was found based on the root of the searchat each match.
C<'ps'> is a list of code strings that describes where the match was for each match.

=head1 SUBPATTERNS

All patterns can have sub-patterns.  Most patterns match the ANDed results of their sub-patterns and their own behavior, first trying the sub-patterns before attempting to match the intrinsic behavior.  However, C<OR> and C<ANY> match any sub-patterns;

For example:

  match([ ['a', 1 ], ['b', 2], ['a', 3] ], EACH(COLLECT('x', ['a', ANY())))) # TRUE

The above pattern means:

=over 2
  
For EACH element in the root structure (an array):

=over 2

COLLECT each element, in C<'x'>, that is,

=over 2

An ARRAY of length 2 that starts with C<'a'>.
 
=back

=back

=back

On the other hand.

  match( [ ['a', 1 ], ['b', 2], ['a', 3] ], ALL(COLLECT('x', [ 'a', ANY() ])) ) 
  # IS FALSE

Because the second root element (an array) does not start with C<'a'>.  But,

  match( [ ['a', 1 ], ['a', 2], ['a', 3] ], ALL(COLLECT('x', [ 'a', ANY() ])) ) 
  # IS TRUE

The pattern below essentially does a flatten of the nested array:

  match(
    [ 1, 'x', 
      [ 2, 'x', 
        [ 3, 'x'], 
        [ 4, 
           [ 5, 
             [ 'x' ] 
           ],
	  6
        ] 
      ] 
    ], 
    FIND(COLLECT('x', EXPR(q{! ref}))), 
    { 'no_collect_path' => 1 }
  );

C<no_collect_path> causes C<COLLECT> and C<BIND>  to not collect any paths.


=head1 MATCH SLICES

Match slices are objects that contain slices of matched portions of a data structure.  This is useful for inflicting change into substructures matched by patterns like C<REST>.

For example:

  do {
    my $a = [ 1, 2, 3, 4 ];
    my $p = [ 1, ANY, REST(BIND('s')) ];
    my $r = matches($a, $p); # 
    ok($r); # TRUE
    ok(Compare($r->{'BIND'}{'s'}{'v'}[0], [ 3, 4 ])); # TRUE
    $p->[0] = 'x';
    matches($a, [ 1, 2, 'x', 4 ]); # TRUE
  }

Hash match slices are generate for each key-value pair for a hash matched by C<EACH> and C<ALL>.  Each of these match slices can be matched as a hash with a single key->value pair.

Match slices are useful for search and replace missions.

=head1 VISITATION ADAPTERS

By default Data::Match is blind to Perl object interfaces.  To instruct Data::Match to not traverse object implementation containers and honor object interfaces you must provide a visitation adaptor.  A visitation adaptor tells Data::Match what to traverse through an object interface and how to keep track of how it got through.

For example:

  package Foo;
  sub new
  {
    my ($cls, %opts) = @_;
    bless \%opts, $cls;
  }
  sub x { shift->{x}; }
  sub parent { shift->{parent}; }
  sub children { shift->{children}; }
  sub add_child { 
    my $self = shift; 
    for my $c ( @_ ) { 
      $c->{parent} = $self;
    }
    push(@{$self->{children}}, @_);
  }


  my $foos = [ map(new Foo('x' => $_), 1 .. 10) ];
  for my $f ( @$foos ) { $f->add_child($foos->[rand($#$foo)); }

  my $pat = FIND(COLLECT('Foo', ISA('Foo', { 'parent' => $foos->[0], REST() => REST() })));
  $match->match($foos, $pat);

The problem with the above example is: C<FIND> will not honor the interface of class Foo by default and will eventually find a Foo where C<$_->parent eq $foos->[0]> through all the parent and child links in the objects' implementation container.  To force Data::Match to honor an interface (or a subset of an interface) during C<FIND> traversal we create a 'find' adapter sub that will do the right thing.

  my $opts = {
    'find' => {
       'Foo' => sub {
	 my ($self, $visitor) = @_;

         # Always do 'x'.
         $visitor->($self->x, 'METHOD', 'x');

	 # Optional children traversal.
	 if ( $match->{'Foo_traverse_children'} ) {
           $visitor->($self->children, 'METHOD', 'children');
	 }

	 # Optional parent traversal.
	 if ( $match->{'Foo_traverse_parent'} ) {
           $visitor->($self->parent, 'METHOD', 'parent');
	 }
       }
     }
  }
  my $match = new Data::Match($opts);
  $match = $match->match($foos, $pat);

See C<t/t4.t> for more examples of visitation adaptors.

=head1 DESIGN

Data::Match employs a mostly-functional external interface since this module was inspired by a Lisp tutorial ("The Little Lisper", maybe) I read too many years ago; besides, pattern matching is largely recursively functional.  The optional control hashes and traverse adapter interfaces are better represented by an object interface so implemented a functional veneer over the core object interface.

Internally, objects are used to represent the pattern primitives because most of the pattern primitives have common behavior.  There are a few design patterns that are particularly applicable in Data::Match: Visitor and Adapter.  Adapter is used to provide the extensiblity for the traversal of blessed structures such that Data::Match can honor the external interfaces of a class and not blindly violate encapsulation.  Visitor is the basis for some of the C<FIND> pattern implementation.  The C<Data::Match::Slice> classes that provide the match slices are probably a Veneer on the array and hash types through the tie meta-behaviors.

=head1 CAVEATS

=over 4

=item *

Does not have regex-like operators like '?', '*', '+'.  

=item *

Should probably have more interfaces with Data::DRef and Data::Walker.

=item *

The visitor adaptors do not use C<UNIVERSAL::isa> to search for the adapter is uses C<ref>.

=item *

Since hash keys do not retain blessedness (what was Larry thinking?) it is difficult to have patterns match keys without resorting to some bizarre regex instead of using C<isa>. 

=back

=head1 STATUS

If you find this to be useful please contact the author.  This is alpha software; all APIs, semantics and behavors are subject to change.

=head1 INTERFACE

This section describes the external interface of this module.

=cut
#'oh emacs, when will perl-mode recognize =pod?

#########################################################################


use strict;
use warnings;

our $VERSION = '0.03';
our $REVISION = do { my @r = (q$Revision: 1.8 $ =~ /\d+/g); sprintf "%d." . "%02d" x $#r, @r };

our $PACKAGE = __PACKAGE__;

use Exporter;
our @ISA = qw(Exporter);
our @EXPORT = qw();

our @export_func = qw(match matches match_path_str match_path_get match_path_ref);
our @autoload_pat = 
  qw(
     ANY 
     AND 
     OR 
     NOT 
     BIND 
     COLLECT 
     REGEX 
     ISA REF 
     DEPTH 
     REST 
     EACH 
     ALL 
     FIND 
     LENGTH 
     EXPR
     );
our @export_pat = @autoload_pat;
our @EXPORT_OK = (@export_func, @export_pat);
our %EXPORT_TAGS = ( 
		     'all'  => \@EXPORT_OK,
		     'func' => \@export_func,
		     'pat'  => \@export_pat,
		     );

use Data::Dumper;
use Data::Compare;
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
    warn "no autoload_pat{$pat}";
    $self->SUPER::AUTOLOAD(@_);
    die "no such method: $AUTOLOAD";
  }
}


sub DESTROY
{
  # NOTHING.
}


*OR = \&ANY; # See ANY::match => match_or.


#########################################################################
# Instance initialization.
#

sub new
{
  my ($self, @opts) = @_;
  my %opts = @opts & 1 ? ( %{$opts[0]}, @opts[1..$#opts]) : @opts;
  (bless \%opts, $self)->defaults->initialize;
}


sub defaults
{
  shift;
}


sub initialize
{
  shift;
}


#=head2 _self_or_instance
#
#Returns self if called as an instance method or a new instance if called as a class method.
# 
#=cut
sub _self_or_instance
{
  my $self = shift;
  # $DB::single = 1;
  ref($self) ? $self : __PACKAGE__->new(@_);
}



#########################################################################
# Low-level container match traversals.
#

#=head2 _match_ARRAY
#
#Internal recursive match routine.  Assumes $matchobj is initialized.
#
#=cut
sub _match_ARRAY
{
  my ($self, $x, $p) = @_;

  my $match;

  ++ $self->{'depth'};
  push(@{$self->{'path'}}, 'ARRAY');
  
 ARRAY:
  {
    $match = 1;
    
    # Each element must match.
    for my $i ( 0 .. $#$p ) {
      # [ 'x', 'y', REST ] matches [ 'x', 'y', 'z', '1', '2', '3' ]
      # Where SUBPAT in REST(SUBPAT) is bound to [ 'z', '1', '2', '3' ].
      if ( ! $self->{'disable_patterns'} && 
	 UNIVERSAL::isa($p->[$i], 'Data::Match::Pattern::REST') ) {
	push(@{$self->{'path'}}, [$i, scalar @$x]);

	# Create an new array slice to match the rest of the array.
	# The Slice::Array object will forward changes to
	# the real array.
	my $slice;
	if ( 1 ) {
	  $slice = Data::Match::Slice::Array->new($x, $i, scalar @$x);
	} else {
	  $slice = [ @{$x}[$i .. $#$x] ];
	}

	# Match REST's subpatterns against the REST slice.
	$match &&= $p->[$i]->_match_rest_ARRAY($slice, $self);
	
	pop(@{$self->{'path'}});

	last ARRAY;
      }
      
      push(@{$self->{'path'}}, $i);
      
      $match = $i < @$x && $self->_match($x->[$i], $p->[$i]);
      
      pop(@{$self->{'path'}});
      
      last ARRAY unless $match;
    }
    
    $match &&= @$p == @$x;
  }

  -- $self->{'depth'};
  pop(@{$self->{'path'}});

  $match;
}



#=head2 _match_HASH
#
#Internal recursive match routine.  Assumes $matchobj is initialized.
#
#=cut
sub _match_HASH
{
  my ($self, $x, $p) = @_;

  my $match = 0;

  ++ $self->{'depth'};
  push(@{$self->{'path'}}, 'HASH');
	  
 HASH:
  {
    $match = 1;
    
    my $rest_pat;
    my $any_key = 0;
    
    my %matched_keys;

    for my $k ( keys %$p ) {
      # ANY in a pattern key matches any other elements.
      if ( ! $self->{'disable_patterns'} && 
	   (
	    ($k =~ /^Data::Match::Pattern::ANY=/)              # unless grep(ref $_, keys %hash)
	    || UNIVERSAL::isa($k, 'Data::Match::Pattern::ANY') # if grep($ref $_, keys %hash)
	    )) {
	if ( ! $any_key ++ ) {
	  my $matches = 0;
	  
	  for my $xk ( keys %$x ) {
	    push(@{$self->{'path'}}, $xk);

	    ++ $matched_keys{$xk};
	    ++ $matches if $self->_match($x->{$xk}, $p->{$k});
	    
	    pop(@{$self->{'path'}});
	    
	    last unless $match;
	  }
	  
	  # Must have at least one match.
	  # { ANY => 'x' } does not match { }.
	  $match &&= $matches;
	}
	next;
      }
      
      # Rest in a pattern causes the rest to match.
      if ( ! $self->{'disable_patterns'} && 
	   ! $rest_pat &&
	 UNIVERSAL::isa($p->{$k}, 'Data::Match::Pattern::REST')
	   ) {
	$rest_pat = $p->{$k};
	next;
      }
      
      # If the key does not exist in pattern, no match.
      push(@{$self->{'path'}}, $k);
      
      ++ $matched_keys{$k};
      $match &&= exists $x->{$k} && $self->_match($x->{$k}, $p->{$k});
      
      pop(@{$self->{'path'}});
      
      last HASH unless $match;
    }
    
    # Handle REST pattern's subpatterns.
    if ( $rest_pat ) {
      my @rest_keys = grep(! exists $matched_keys{$_}, keys %$x);

      # Create a temporary hash slice containing
      # the values from $x for all the unmatched keys.
      my $slice;
      if ( 1 ) {
	$slice = Data::Match::Slice::Hash->new($x, \@rest_keys);
      } else {
	$slice = { };
	@{$slice}{@rest_keys} = @{$x}{@rest_keys};
      }

      # See match_path_str().
      push(@{$self->{'path'}}, \@rest_keys);

      #$DB::single = 1;
      $match &&= $rest_pat->_match_rest_HASH($slice, $self);

      pop(@{$self->{'path'}});
    } else {
      # Make sure they are the same length.
      $match &&= (scalar values %$p) == (scalar values %$x) unless $any_key;
    }
  }

  -- $self->{'depth'};
  pop(@{$self->{'path'}});

  $match;
}


#=head2 _match_SCALAR
#
#Internal recursive match routine.  Assumes $matchobj is initialized.
#
#=cut
sub _match_SCALAR
{
  my ($self, $x, $p) = @_;

  ++ $self->{'depth'};
  push(@{$self->{'path'}}, 'SCALAR');
	  
  push(@{$self->{'path'}}, undef); # 'emacs
  my $match = $self->_match($$x, $$p);
  pop(@{$self->{'path'}});

  -- $self->{'depth'};
  pop(@{$self->{'path'}});

  $match;
};



#=head2 _match_path_push
#
#Internal recursive match routine.  Assumes $self is initialized.
#
#=cut
sub _match_path_push
{
  my $self = shift;
  ++ $self->{'depth'};
  push(@{$self->{'path'}}, @_);
}


#=head2 _match_path_pop
#
#Internal recursive match routine.  Assumes $self is initialized.
#
#=cut
sub _match_path_pop
{
  my $self = shift;
  # $DB::single = 1;
  splice(@{$self->{'path'}}, -2);
  -- $self->{'depth'};
  confess "too many _match_path_pop" if $self->{'depth'} < 0;
}


#=head2 _match
#
#Internal recursive match routine.  Assumes $self is initialized.
#
#=cut
sub _match
{
  my ($self, $x, $p) = @_;

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
    elsif ( ! $self->{'disable_patterns'} && UNIVERSAL::isa($p, 'Data::Match::Pattern') ) {
      # Delegate match to pattern object.
      $match = $p->match($x, $self);
    }

    # Handle deep structures.
    elsif ( ref($x) ) {
      # Acquire visitation lock.
      if ( $self->{'visiting'}{$x} ++ ) {
	$match = Compare($x, $p);
      }
      # Class-specific visit adaptor?
      elsif ( my $visit = $self->{'match'}{ref($x)} ) {
	# $match = 1;
	my $visitor = sub {
	  $match &&= $self->_match($_[0], $p);  #should this be ||= or &&=? 
	};
	$match = $visit->($x, $visitor);
      }
      # Array pattern template?
      elsif ( UNIVERSAL::isa($x, 'ARRAY')  && UNIVERSAL::isa($p, 'ARRAY') ) {
	$match = $self->_match_ARRAY($x, $p);
      }
      # Hash pattern template?
      elsif ( UNIVERSAL::isa($x, 'HASH')   && UNIVERSAL::isa($p, 'HASH') ) {
	$match = $self->_match_HASH($x, $p);
      }
      # Scalar ref pattern template?
      elsif ( UNIVERSAL::isa($x, 'SCALAR') && UNIVERSAL::isa($p, 'SCALAR') ) {
	$match = $self->_match_SCALAR($x, $p);
      }
      else {
	# Extensible comparators?
	if ( my $comparator = $self->{'compare'}{ref($x) || '*'} ) {
	  # Try a comparator.
	  $match = $comparator->($x, $p, $self);
	} else {
	  # Default to eq.
	  $match = $x eq $p;
	}
      }
    } else {
      # Scalar eq.
      $match = $x eq $p;
    }

    # Release visitation lock.
    -- $self->{'visiting'}{$x};
  };

  #$DB::single = 1;

  $match;
}


=head2 %match_opts

Default options for C<match>.

=cut
our %match_opts
  = (
     #'collect_path_DRef' => 1,
     #'collect_path_str' => 0,
     #'no_collect_path' => 1.
     );


=head2 _match_pre

Initialize the match object before pattern traversal.

=cut
sub _match_pre
{
  my ($self, $x, $p, $opts) = @_;

  # Install opts.
  @{$self}{keys %match_opts} = values %match_opts;
  @{$self}{keys %$opts} = values %$opts if ( $opts );

  # Initialize state.
  $self->{'depth'}    ||= 0;
  $self->{'visiting'} ||= { };
  $self->{'path'}     ||= [ ];
  $self->{'root'}     ||= $x;
  $self->{'pattern'}  ||= $p;
  $self->{'_COLLECT'} ||= 'COLLECT';
  $self->{'_BIND'}    ||= 'BIND';
  
  $self;
}


=head2 _match_post

Initialize the match object before pattern traversal.

=cut
sub _match_post
{
  my ($self, $x, $p) = @_;

  delete $self->{'visiting'} unless $self->{'keep_visiting'};

  # Post conditions.
  {
    no warnings;

    confess "Expected results->{depth} == 0" unless $self->{'depth'} == 0;
    confess "Expected results->{path} eq [ ]" unless ! @{$self->{'path'}};
    confess "Expected results->{root} eq root" unless $self->{'root'} eq $x;
    confess "Expected results->{pattern} eq pattern" unless $self->{'pattern'} eq $p;
  }

  $self;
}


=head2 execute

Matches a structure against a pattern.  In a list context, returns both the match success and results; in a scalar context returns the results hash if match succeeded or undef.

  use Data::Match;
  my $obj = new Data::Match();
  my $matched = $obj->execute($thing, $pattern);

=cut
sub execute
{
  my ($self, $x, $p) = @_;

  $self->_match_pre($x, $p);
  my $matches = $self->_match($x, $p);
  $self->_match_post($x, $p);

  # Return results.
  if ( wantsarray ) {
    return ($matches, $self);
  } else {
    return $matches ? $self : undef;
  }
}


=head2 match

   match($thing, $pattern, @opts)

it same as:

   Data::Match->new(@opts)->execute($thing, $pattern);

=cut
sub match
{
  my ($x, $p, @opts) = @_;

  __PACKAGE__->new(@opts)->execute($x, $p);
}


=head2 matches

Same as C<match> in scalar context.

=cut
sub matches
{
  my ($x, $p, @opts) = @_;

  my ($match, $results) = match($x, $p, @opts);

  $match ? $results : undef;
}



##################################################
# Path support
#


=head2 match_path_str

Returns a perl expression that will generate code to point to the element of the path.

=cut
sub match_path_str
{
  my ($path, $str, $matchobj) = @_;

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
	$str = $str . "->$ind()";
      }
    }
    else {
      $str = undef;
    }
  }

  $str;
}



=head2 match_path_DRef_path

Returns a string suitable for Data::DRef.

=cut
sub match_path_DRef_path
{
  my ($path, $str, $matchobj, $sep) = @_;

  $str ||= '';
  $sep ||= $Data::DRef::sep || '.';

  my @path = @$path;

  while ( @path ) {
    my $ref = shift @path;
    my $ind = shift @path;

    if ( $ref eq 'ARRAY' ) {
      if ( ref($ind) eq 'ARRAY' ) {
	# Not supported by DRef.
	$str .= $sep . '[' . $ind->[0] . '..' . ($ind->[1] - 1) . ']';
      } else {
	$str .= $sep . $ind;
      }
    }
    elsif ( $ref eq 'HASH' ) {
      if ( ref($ind) eq 'ARRAY' ) {
	# Not supported by DRef.
	$str .= $sep . '{' . join(',', @$ind->[0]) . '}';
      } else {
	$str .= $sep . $ind;
      }
    }
    elsif ( $ref eq 'SCALAR' ) {
      # Not supported by DRef.
      $str .= $sep . '$'; #'emacs
    }
    elsif ( $ref eq 'METHOD' ) {
      # Not supported by DRef.
      confess "Ugh $ref";
    }
    else {
      # Not supported by DRef.
      confess "Ugh $ref";
    }
  }

  $str =~ s/^$sep//;

  $str;
}


=head2 match_path_get

Returns the value pointing to the location for the match path in the root.

  my $results = matches($thing, FIND(BIND('x', [ 'x', REST ])));
  my $x = match_path_get($thing, $results->{'BIND'}{'x'}{'p'}[0]);

The above example returns the first array that begins with C<'x'>.

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
  match_path_set($thing, $results->{'BIND'}{'x'}{'p'}[0], 'y');

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
  my $ref = match_path_ref($thing, $results->{'BIND'}{'x'}{'p'}[0]);
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

  confess("INVALID " . ref($self) . ": expected at least " . $self->subpattern_offset . " elements")
    unless @$self >= $self->subpattern_offset;

  $self;
}


sub subpattern_offset { 0; }

sub match_and
{
  my ($self, $x, $matchobj) = @_;

  for my $i ( $self->subpattern_offset .. $#$self ) {
    return 0 unless $matchobj->_match($x, $self->[$i]);
  }

  1;
}


sub match_or
{
  my ($self, $x, $matchobj) = @_;

  for my $i ( $self->subpattern_offset .. $#$self ) {
    return 1 if $matchobj->_match($x, $self->[$i]);
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
  my ($self, $x, $matchobj) = @_;

  # $DB::single = 1;
  ! ((scalar @$self) ? $self->match_and($x, $matchobj) : $x);
}


##################################################


package Data::Match::Pattern::ANY;

our @ISA = qw(Data::Match::Pattern);

sub match 
{
  my ($self, $x, $matchobj) = @_;

  #$DB::single = 1;
  # ANY always matches.

  if ( @{$self} ) {
    # Do subpatterns.
    $self->match_or($x, $matchobj);
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
  my ($self, $x, $matchobj, $binding) = @_;

  push(@{$binding->{'v'}}, $x );

  my $path = [ @{$matchobj->{'path'}} ];

  push(@{$binding->{'p'}}, $path) 
    unless $matchobj->{'no_collect_path'};

  push(@{$binding->{'ps'}}, Data::Match::match_path_str($path, undef, $matchobj)) 
    if ( $matchobj->{'collect_path_str'} );

  push(@{$binding->{'pdr'}}, Data::Match::match_path_DRef_path($path, undef, $matchobj)) 
    if ( $matchobj->{'collect_path_DRef'} );
}


sub match 
{ 
  my ($self, $x, $matchobj) = @_;

  # warn "MATCH($self->[0])";

  # $DB::single = 1;
  
  # Do subpatterns.
  return 0 unless $self->match_and($x, $matchobj);

  my $binding = $matchobj->{$matchobj->{'_COLLECT'}}{$self->[0]} ||= { };

  $self->_collect($x, $matchobj, $binding);

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
  my ($self, $x, $matchobj) = @_;

  # warn "MATCH($self->[0])";

  # $DB::single = 1;

  # Do subpatterns.
  return 0 unless $self->match_and($x, $matchobj);

  my $binding = $matchobj->{$matchobj->{'_BIND'}}{$self->[0]};

  if ( $binding ) {
    #$DB::single = 1;
    if ( Compare($binding->{'v'}[0], $x) ) {
      $self->_collect($x, $matchobj, $binding);
    } else {
      return 0;
    }
  } else {
    $self->_collect($x, $matchobj, $matchobj->{$matchobj->{'_BIND'}}{$self->[0]} = {});
  }

  1;
}


##################################################


package Data::Match::Pattern::REGEX;

our @ISA = qw(Data::Match::Pattern);

sub subpattern_offset { 1; };

sub match 
{
  my ($self, $x, $matchobj) = @_;

  # $DB::single = 1;
  
  # Note: do not check that it is not a ref incase the object can be coerced into a string.
  ($x =~ /$self->[0]/sx) && $self->match_and($x, $matchobj); 
}


##################################################


package Data::Match::Pattern::ISA;

our @ISA = qw(Data::Match::Pattern);

sub subpattern_offset { 1; };

sub match 
{
  my ($self, $x, $matchobj) = @_;

  UNIVERSAL::isa($x, $self->[0]) and $self->match_and($x, $matchobj);
}


##################################################


package Data::Match::Pattern::REF;

our @ISA = qw(Data::Match::Pattern);

sub subpattern_offset { 0; };

sub match 
{
  my ($self, $x, $matchobj) = @_;

  $x = ref($x);
  $x && $self->match_and($x, $matchobj);
}


##################################################


package Data::Match::Pattern::DEPTH;

our @ISA = qw(Data::Match::Pattern);

sub subpattern_offset { 0; };

sub match 
{
  my ($self, $x, $matchobj) = @_;

  $x = $matchobj->{'depth'};

  $self->match_and($x, $matchobj);
}


##################################################


package Data::Match::Pattern::LENGTH;

our @ISA = qw(Data::Match::Pattern);

sub subpattern_offset { 0; };

sub match 
{
  my ($self, $x, $matchobj) = @_;

  no warnings;

  if ( ref($x) ) {
    if (    UNIVERSAL::isa($x, 'ARRAY') ) {
      $x = @$x;
    }
    elsif ( UNIVERSAL::isa($x, 'HASH') ) {
      $x = %$x;
    }
    elsif ( UNIVERSAL::isa($x, 'SCALAR') ) {
      $x = $x ? 1 : 0;
    }
    else {
      $x = undef;
    }
  } else {
    $x = length $x;
  }

  @$self ? $self->match_and($x, $matchobj) : $x;
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

  # Make room for EXPR sub.
  splice(@$self, 1, 0, 'UGH');

  if ( UNIVERSAL::isa($self->[0], 'CODE') ) {
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
  my ($self, $x, $matchobj) = @_;

  # $DB::single = 1;

  $self->[1]->($x, $matchobj, $self) && $self->match_and($x, $matchobj);
}


##################################################


package Data::Match::Pattern::REST;

our @ISA = qw(Data::Match::Pattern);


sub match
{
  # Should only match in an array or hash context.
  0;
}


sub _match_rest_ARRAY
{
  my ($self, $x, $matchobj) = @_;

  ref($x) && $self->match_and($x, $matchobj);
}


sub _match_rest_HASH
{
  my ($self, $x, $matchobj) = @_;

  ref($x) && $self->match_and($x, $matchobj);
}


##################################################


package Data::Match::Pattern::EACH;

our @ISA = qw(Data::Match::Pattern);


sub _match_each_ARRAY
{
  my ($self, $x, $matchobj, $matches) = @_;

  ++ $matchobj->{'depth'};
  push(@{$matchobj->{'path'}}, 'ARRAY');
  
  my $i = -1;
  for my $e ( @$x ) {
    push(@{$matchobj->{'path'}}, ++ $i);
    ++ $$matches if $self->match_and($e, $matchobj);
    pop(@{$matchobj->{'path'}});
  }
  
  pop(@{$matchobj->{'path'}});
  -- $matchobj->{'depth'};
}


sub _match_each_HASH
{
  my ($self, $x, $matchobj, $matches) = @_;

  ++ $matchobj->{'depth'};
  push(@{$matchobj->{'path'}}, 'HASH');
  
  for my $k ( keys %$x ) {
    my @k = ( $k );

    # We compensate the path for hash slice.
    push(@{$matchobj->{'path'}}, \@k);
    
    # Create a temporary hash slice.
    # because we are matching EACH element of the hash.
    my $slice;
    if ( 1 ) {
      $slice = Data::Match::Slice::Hash->new($x, \@k);
    } else {
      $slice = { $k => $x->{$k} };
    }

    ++ $$matches if $self->match_and($slice, $matchobj);
    
    pop(@{$matchobj->{'path'}});
  }
  
  pop(@{$matchobj->{'path'}});
  -- $matchobj->{'depth'};
}


sub _match_each_SCALAR
{
  my ($self, $x, $matchobj, $matches) = @_;

  ++ $matchobj->{'depth'};
  push(@{$matchobj->{'path'}}, 'SCALAR', undef);
  
  ++ $$matches if $self->match_and($$x, $matchobj);
  
  pop(@{$matchobj->{'path'}});
  pop(@{$matchobj->{'path'}});
  -- $matchobj->{'depth'};
}


sub _match_each
{
  my ($self, $x, $matchobj, $matches) = @_;

  # Traverse.
  if ( ref($x) ) {
    if ( my $eacher = $matchobj->{'each'}{ref($x)} ) {
      my $visitor = sub { ++ $$matches if ( $self->_match_and($_[0], $matchobj) ); };
      $eacher->($x, $visitor);
    }
    elsif (    UNIVERSAL::isa($x, 'ARRAY') ) {
      $self->_match_each_ARRAY($x, $matchobj, $matches);
    }
    elsif ( UNIVERSAL::isa($x, 'HASH') ) {
      $self->_match_each_HASH($x, $matchobj, $matches);
    }
    elsif ( UNIVERSAL::isa($x, 'SCALAR') ) {
      $self->_match_each_SCALAR($x, $matchobj, $matches);
    }
    else {
      # Try to match it explicitly.
      ++ $$matches if $self->match_and($x, $matchobj);
    }
  }
}


sub match
{
  my ($self, $x, $matchobj) = @_;

  my $matches = 0;

  $self->_match_each($x, $matchobj, \$matches);

  $matches;
}


##################################################


package Data::Match::Pattern::ALL;

our @ISA = qw(Data::Match::Pattern::EACH);


sub match
{
  my ($self, $x, $matchobj) = @_;

  my $matches = 0;

  my $expected = $self;

  if ( UNIVERSAL::isa($x, 'ARRAY') ) {
    $expected = scalar @$x;
  }
  elsif ( UNIVERSAL::isa($x, 'HASH') ) {
    $expected = scalar %$x;
  } else {
    $expected = -1;
  }

  $self->_match_each($x, $matchobj, \$matches);

  $matches == $expected;
}



##################################################


package Data::Match::Pattern::FIND;

our @ISA = qw(Data::Match::Pattern);


sub _match_find_ARRAY
{
  my ($self, $x, $matchobj, $matches, $visited) = @_;

  my $i = -1;
  for my $e ( @$x ) {
    push(@{$matchobj->{'path'}}, ++ $i);
    
    $self->_match_find($e, $matchobj, $matches, $visited);
    
    pop(@{$matchobj->{'path'}});
  }
}


sub _match_find_HASH
{
  my ($self, $x, $matchobj, $matches, $visited) = @_;

  for my $k ( keys %$x ) {
    
    push(@{$matchobj->{'path'}}, [ $k ]);
    
    # This needs a new Slice class.
    $self->_match_find($k, $matchobj, $matches); # HUH?
    
    pop(@{$matchobj->{'path'}});
    
    push(@{$matchobj->{'path'}}, $k);
    
    $self->_match_find($x->{$k}, $matchobj, $matches, $visited);
    
    pop(@{$matchobj->{'path'}});
  }
}


sub _match_find
{
  my ($self, $x, $matchobj, $matches, $visited) = @_;

  # Does this match directly? 
  ++ $$matches if ( $self->match_and($x, $matchobj) );

  # Traverse.
  if ( ref($x) ) {

    return if ( $visited->{$x} ++ );

    # $DB::single = 1;

    if ( my $visit = ($matchobj->{'find'}{ref($x)} || $matchobj->{'visit'}{ref($x)}) ) {
      my $visitor = sub { 
	my $thing = shift;
	$matchobj->_match_path_push(@_) if @_;
	$self->_match_find($thing, $matchobj, $matches, $visited);
	$matchobj->_match_path_pop if @_;
      };
      $visit->($x, $visitor, $matchobj);
    }
    elsif ( UNIVERSAL::isa($x, 'ARRAY') ) {
      ++ $matchobj->{'depth'};
      push(@{$matchobj->{'path'}}, 'ARRAY');

      $self->_match_find_ARRAY($x, $matchobj, $matches, $visited);

      pop(@{$matchobj->{'path'}});
      -- $matchobj->{'depth'};
    }
    elsif ( UNIVERSAL::isa($x, 'HASH') ) {
      ++ $matchobj->{'depth'};
      push(@{$matchobj->{'path'}}, 'HASH');

      $self->_match_find_HASH($x, $matchobj, $matches, $visited);

      pop(@{$matchobj->{'path'}});
      -- $matchobj->{'depth'};
    }
    elsif ( UNIVERSAL::isa($x, 'SCALAR') ) {
      ++ $matchobj->{'depth'};
      push(@{$matchobj->{'path'}}, 'SCALAR', undef);

      $self->_match_find($$x, $matchobj, $matches, $visited);

      pop(@{$matchobj->{'path'}});
      pop(@{$matchobj->{'path'}});
      -- $matchobj->{'depth'};
    }
    else {
      warn "Huh?";
    }
  }
}


sub match
{
  my ($self, $x, $matchobj) = @_;

  my $matches = 0;

  $self->_match_find($x, $matchobj, \$matches, { });

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
  die "$src must be ARRAY" unless UNIVERSAL::isa($src, 'ARRAY');
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
  die "src $src must be a HASH" unless UNIVERSAL::isa($src, 'HASH');
  die "keys $keys must be an ARRAY" unless UNIVERSAL::isa($keys, 'ARRAY');
  bless [ $src, { map(($_, 1), @$keys) } ], $cls;
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

Version 0.03, $Revision: 1.8 $.

=head1 AUTHOR

Kurt A. Stephens <kurtstephens@acm.org>

=head1 COPYRIGHT

Copyright (c) 2001, Kurt A. Stephens and ION, INC.

=head1 SEE ALSO

L<perl>, L<Array::PatternMatcher>, L<Data::Compare>, L<Data::Dumper>, L<Data::DRef>, L<Data::Walker>.

=cut

##################################################

1;
