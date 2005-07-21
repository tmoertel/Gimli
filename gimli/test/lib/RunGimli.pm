package RunGimli;

use File::Temp 'tempfile';

sub import {
    my $caller = caller;
    { no strict 'refs';  *{$caller.'::run_gimli'} = \&run; }
}

sub run {

    my ($input) = @_;

    my $fh = tempfile();
    die "cannot open tempfile: $!" unless $fh;

    my $pid = open my $write_to_child, "|-";
    die "cannot fork: $!" unless defined $pid;

    if ($pid) {  # parent
        print $write_to_child $input;
        close $write_to_child || warn "kid exited $?";
    } else {     # child
        open STDOUT, '>&', $fh  or die "could not redirect stdout: $!";
        open STDERR, ">&STDOUT" or die "could not dup stdout to stderr: $!";
        close $fh               or die "error on close of tempfile: $!";
        exec "app/gimli"        or die "could not exec gimli: $!";
    }

    wait;

    seek $fh, 0, 0 or die "could not seek to start of tempfile: $!";
    my $results = do { local $/; <$fh> };
    close $fh;

    return $results;
}

1;
