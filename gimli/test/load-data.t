#!/usr/bin/perl

use warnings;
use strict;

use Test::More tests => 13;

BEGIN { unshift @INC, 'test/lib'; }
use RunGimli;

#==============================================================================
# CSV tests
#==============================================================================

load_csv_evals_exact_ok( <<FILE, <<EXPECTED );
x
FILE
 x
EXPECTED

load_csv_evals_exact_ok( <<FILE, <<EXPECTED );
x
T
FILE
  x
1 T
EXPECTED

load_csv_evals_exact_ok( <<FILE, <<EXPECTED );
x
T
F
FILE
  x
1 T
2 F
EXPECTED

load_csv_evals_exact_ok( <<FILE, <<EXPECTED );
 ,x
1,T
FILE
  x
1 T
EXPECTED

load_csv_evals_exact_ok( <<FILE, <<EXPECTED );
x,y
T,9
FILE
  x y
1 T 9
EXPECTED

# blank columns ought to be ignored

load_csv_evals_exact_ok( <<FILE, <<EXPECTED );
x,y,,X,Y
T,9,,F,8
FILE
  x y X Y
1 T 9 F 8
EXPECTED

# non-rectangular tables are verboten

load_csv_evals_exact_ok( <<FILE, qr/error .* non-rectangular/x );
x,y
T
FILE


#==============================================================================
# WSV tests
#==============================================================================

load_wsv_evals_exact_ok( <<FILE, <<EXPECTED );
x
FILE
 x
EXPECTED

load_wsv_evals_exact_ok( <<FILE, <<EXPECTED );
x
T
FILE
  x
1 T
EXPECTED

load_wsv_evals_exact_ok( <<FILE, <<EXPECTED );
x
T
F
FILE
  x
1 T
2 F
EXPECTED

load_wsv_evals_exact_ok( <<FILE, <<EXPECTED );
  x
1 T
FILE
  x
1 T
EXPECTED

load_wsv_evals_exact_ok( <<FILE, <<EXPECTED );
x y
T 9
FILE
  x y
1 T 9
EXPECTED

# non-rectangular tables are verboten

load_csv_evals_exact_ok( <<FILE, qr/error .* non-rectangular/x );
x y
T
FILE



#==============================================================================
# helpers
#==============================================================================

use File::Temp;

sub load_csv_evals_exact_ok {
    load_file_evals_exact_ok( "csv", @_ );
}

sub load_wsv_evals_exact_ok {
    load_file_evals_exact_ok( "wsv", @_ );
}

sub load_file_evals_exact_ok {
    my ($kind, $file, $expected) = @_;
    with_file($file, sub {
        evals_exact_ok( qq[read.$kind("$_")], $expected )
    } );
}

sub with_file {
    my ($content, $testfn) = @_;
    my $tmp = File::Temp->new;
    print $tmp $content;
    local $_ = $tmp->filename;
    $testfn->();
}
