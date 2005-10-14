#!/usr/bin/perl

use warnings;
use strict;

use Test::More tests => 34;

BEGIN { unshift @INC, 'test/lib'; }
use RunGimli;


#==============================================================================
# list constuction
#==============================================================================

evals_exact_ok( "list(x=5)", <<'EOF' );
$x => 5
EOF

evals_exact_ok( "list(x=1,y=\"a\")", <<'EOF' );
$x => 1
$y => "a"
EOF

evals_exact_ok( "list(5)", <<'EOF' );
[1] => 5
EOF

evals_exact_ok( "list(1,y=2)", <<'EOF' );
[1] => 1
$y => 2
EOF

evals_exact_ok( "list(NULL)", <<'EOF' );
[1] => NULL
EOF


#==============================================================================
# list indexing
#==============================================================================

my $t = "l <- list(x=1,2,z=3)";

# project by number

evals_ok( "$t; l\$1", '1' );
evals_ok( "$t; l\$2", '2' );
evals_ok( "$t; l\$3", '3' );
evals_ok( "$t; l\$0", qr/out of range/ );
evals_ok( "$t; l\$4", qr/out of range/ );

# project by name

evals_ok( "$t; l\$x", "1" );
evals_ok( "$t; l\$z", "3" );
evals_ok( "$t; l\$foo", qr/name .* does not exist/ );
evals_ok( "$t; l\$x", '1' );
evals_ok( "$t; l\$y", qr/name .* does not exist/ );
evals_ok( "$t; l\$z", '3' );

# select by number vector

evals_ok( "$t; l[1]"      , '$x => 1' );
evals_ok( "$t; l[2]"      , '[1] => 2' );
evals_ok( "$t; l[3]"      , '$z => 3' );
evals_ok( "$t; l[0]"      , qr/out of range/ );
evals_ok( "$t; l[4]"      , qr/out of range/ );

# select by negative number vector

evals_exact_ok( "$t; l[-2]", <<'EOF' );
$x => 1
$z => 3
EOF
evals_exact_ok( "$t; l[-[1,2]]", <<'EOF' );
$z => 3
EOF

# select by logical vector

evals_ok( "$t; l[[T,F,F]]", '$x => 1' );
evals_ok( "$t; l[[F,T,F]]", '[1] => 2' );

# select by string vector

evals_ok( "$t; l[\"x\"]"  , '$x => 1' );
evals_ok( "$t; l[\"y\"]"  , qr/name .* does not exist/ );
evals_ok( "$t; l[\"z\"]"  , '$z => 3' );
evals_ok( "$t; l[[1,\"z\"]]", qr/name .* does not exist/ );
                              # "1" is not in list



#==============================================================================
# index-name uniqueness
#==============================================================================

evals_ok( "names(list(x=1,x=1))"      , '["x","x.1"]' );
evals_ok( "names(list(x=1,x=1,x=1))"  , '["x","x.1","x.2"]' );
evals_ok( "names(list(x=1,x=1,x.1=1))", '["x","x.2","x.1"]' );
evals_ok( "names(list(x.1=1,x.1=1))"  , '["x.1","x.1.1"]' );
