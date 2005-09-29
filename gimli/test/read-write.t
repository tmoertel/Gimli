#!/usr/bin/perl

use warnings;
use strict;

use Test::More tests => 38;

BEGIN { unshift @INC, 'test/lib'; }
use RunGimli;

#==============================================================================
# Error-handling tests
#==============================================================================


for (["read",1], ["write",2]) {
    my ($op, $nargs) = @$_;
    for my $type (qw(csv tsv wsv)) {
        evals_ok( "$op.$type()", qr/error:.*$nargs argument.*not 0/ );
    }
}


#==============================================================================
# CSV tests
#==============================================================================

#------------------------------------------------------------------------------
# read
#------------------------------------------------------------------------------

read_csv_evals_exact_ok( <<FILE, <<EXPECTED );
x
FILE
 x
EXPECTED

read_csv_evals_exact_ok( <<FILE, <<EXPECTED );
x
T
FILE
  x
1 T
EXPECTED

read_csv_evals_exact_ok( <<FILE, <<EXPECTED );
x
T
F
FILE
  x
1 T
2 F
EXPECTED

read_csv_evals_exact_ok( <<FILE, <<EXPECTED );
 ,x
1,T
FILE
  x
1 T
EXPECTED

read_csv_evals_exact_ok( <<FILE, <<EXPECTED );
x,y
T,9
FILE
  x y
1 T 9
EXPECTED

read_csv_evals_exact_ok( <<FILE, <<EXPECTED );
x,y
,9
FILE
   x y
1 "" 9
EXPECTED

# blank columns ought to be ignored

read_csv_evals_exact_ok( <<FILE, <<EXPECTED );
x,y,,X,Y
T,9,,F,8
FILE
  x y X Y
1 T 9 F 8
EXPECTED

# non-rectangular tables are verboten

read_csv_evals_exact_ok( <<FILE, qr/error .* non-rectangular/x );
x,y
T
FILE


#------------------------------------------------------------------------------
# write
#------------------------------------------------------------------------------

write_csv_evals_exact_ok( "table(x=1)", <<EOF );
x
1
EOF

write_csv_evals_exact_ok( "table(x=1:2,y=2:3)", <<EOF );
x,y
1,2
2,3
EOF

write_csv_evals_exact_ok( 'table(x="string")', <<EOF );
x
"string"
EOF


write_csv_evals_exact_ok( 'table(x="str\"ing")', <<'EOF' );
x
"str\"ing"
EOF



#==============================================================================
# WSV tests
#==============================================================================

#------------------------------------------------------------------------------
# read
#------------------------------------------------------------------------------

read_wsv_evals_exact_ok( <<FILE, <<EXPECTED );
x
FILE
 x
EXPECTED

read_wsv_evals_exact_ok( <<FILE, <<EXPECTED );
x
T
FILE
  x
1 T
EXPECTED

read_wsv_evals_exact_ok( <<FILE, <<EXPECTED );
x
T
F
FILE
  x
1 T
2 F
EXPECTED

read_wsv_evals_exact_ok( <<FILE, <<EXPECTED );
  x
1 T
FILE
  x
1 T
EXPECTED

read_wsv_evals_exact_ok( <<FILE, <<EXPECTED );
x y
T 9
FILE
  x y
1 T 9
EXPECTED

# non-rectangular tables are verboten

read_wsv_evals_exact_ok( <<FILE, qr/error .* non-rectangular/x );
x y
T
FILE


#------------------------------------------------------------------------------
# write
#------------------------------------------------------------------------------

write_wsv_evals_exact_ok( "table(x=1)", <<EOF );
  x
1 1
EOF

write_wsv_evals_exact_ok( 'table(x="string")', <<EOF );
         x
1 "string"
EOF


write_wsv_evals_exact_ok( 'table(x="str\"ing")', <<'EOF' );
           x
1 "str\"ing"
EOF



#==============================================================================
# TSV tests
#==============================================================================

#------------------------------------------------------------------------------
# read
#------------------------------------------------------------------------------

read_tsv_evals_exact_ok( <<FILE, <<EXPECTED );
x
FILE
 x
EXPECTED

read_tsv_evals_exact_ok( <<FILE, <<EXPECTED );
x
T
FILE
  x
1 T
EXPECTED

read_tsv_evals_exact_ok( <<FILE, <<EXPECTED );
x
T
F
FILE
  x
1 T
2 F
EXPECTED

read_tsv_evals_exact_ok( <<FILE, <<EXPECTED );
\tx
1\tT
FILE
  x
1 T
EXPECTED

read_tsv_evals_exact_ok( <<FILE, <<EXPECTED );
x\ty
T\t9
FILE
  x y
1 T 9
EXPECTED

read_tsv_evals_exact_ok( <<FILE, <<EXPECTED );
x\ty
\t9
FILE
   x y
1 "" 9
EXPECTED

# non-rectangular tables are verboten

read_tsv_evals_exact_ok( <<FILE, qr/error .* non-rectangular/x );
x\ty
T
FILE


#------------------------------------------------------------------------------
# write
#------------------------------------------------------------------------------

write_tsv_evals_exact_ok( "table(x=1)", <<EOF );
x
1
EOF

write_tsv_evals_exact_ok( "table(x=1:2,y=2:3)", <<EOF );
x\ty
1\t2
2\t3
EOF

write_tsv_evals_exact_ok( 'table(x="string")', <<EOF );
x
"string"
EOF


write_tsv_evals_exact_ok( 'table(x="str\"ing")', <<'EOF' );
x
"str\"ing"
EOF




#==============================================================================
# helpers
#==============================================================================

use File::Temp;

sub read_csv_evals_exact_ok {
    read_file_evals_exact_ok( "csv", @_ );
}

sub read_wsv_evals_exact_ok {
    read_file_evals_exact_ok( "wsv", @_ );
}

sub read_tsv_evals_exact_ok {
    read_file_evals_exact_ok( "tsv", @_ );
}

sub read_file_evals_exact_ok {
    my ($kind, $file, $expected) = @_;
    with_file($file, sub {
        evals_exact_ok( qq[read.$kind("$_")], $expected )
    } );
}

sub write_csv_evals_exact_ok {
    write_file_evals_exact_ok( "csv", @_ );
}

sub write_wsv_evals_exact_ok {
    write_file_evals_exact_ok( "wsv", @_ );
}

sub write_tsv_evals_exact_ok {
    write_file_evals_exact_ok( "tsv", @_ );
}

sub write_file_evals_exact_ok {
    my ($kind, $gimli_cmd, $expected) = @_;
    with_file("--write-file-exact-ok-initial-content--", sub {
        run_gimli( qq[t <- $gimli_cmd; write.$kind(t,"$_")] );
        is( read_file($_), $expected ) ;
    } );
}

sub with_file {
    my ($content, $testfn) = @_;
    my $tmp = File::Temp->new;
    $tmp->autoflush(1);
    print $tmp $content;
    local $_ = $tmp->filename;
    my $level = $Test::Builder::Level;
    {  local $Test::Builder::Level = $level + 4;
       $testfn->($content);
   }
}

sub read_file {
    my ($file) = @_;
    open my $fh, $file or die "cannot open $file for reading: $!";
    my $content = do { local $/; <$fh> };
    close $fh or die "cannot close $file: $!";
    return $content;
}
