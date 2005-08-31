#!/usr/bin/perl

use warnings;
use strict;

use Test::More tests => 3;

BEGIN { unshift @INC, 'test/lib'; }
use RunGimli;

#=============================================================================
# defaults
#=============================================================================

evals_ok( "SYS.COLS", "1.0e9" );
evals_ok( "SYS.ROWS", "1.0e9" );

#=============================================================================
# user config via $HOME/.gimli
#=============================================================================

use File::Temp 'tempdir';
use File::Basename 'dirname';

{
    my $dir = tempdir( "shell-XXXXXX", CLEANUP => 1 );
    write_file( "$dir/.gimli", "SYS.COLS <- 13\n" );
    local $ENV{"HOME"} = $dir;
    evals_ok( "SYS.COLS", 13);
}



#=============================================================================
# helpers
#=============================================================================

sub write_file {
    my ($path, $content) = @_;
    open my $fh, ">$path" or die "cannot open $path for writing: $!";
    print $fh $content;
    close $fh or die "close of $path failed: $!";
}
