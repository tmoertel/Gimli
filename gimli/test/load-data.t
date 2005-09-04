#!/usr/bin/perl

use warnings;
use strict;

use Test::More tests => 16;

BEGIN { unshift @INC, 'test/lib'; }
use RunGimli;

#==============================================================================
# READ CSV tests
#==============================================================================

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


#==============================================================================
# READ WSV tests
#==============================================================================

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

read_csv_evals_exact_ok( <<FILE, qr/error .* non-rectangular/x );
x,y
T
FILE


#==============================================================================
# WRITE WSV tests
#==============================================================================

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
# helpers
#==============================================================================

use File::Temp;

sub read_csv_evals_exact_ok {
    read_file_evals_exact_ok( "csv", @_ );
}

sub read_wsv_evals_exact_ok {
    read_file_evals_exact_ok( "wsv", @_ );
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

sub write_file_evals_exact_ok {
    my ($kind, $gimli_cmd, $expected) = @_;
    with_file("", sub {
        run_gimli( qq[t <- $gimli_cmd; write.$kind(t,"$_")] );
        is( read_file($_), $expected ) ;
    } );
}

sub with_file {
    my ($content, $testfn) = @_;
    my $tmp = File::Temp->new;
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
