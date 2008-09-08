package RunGimli;

use Exporter 'import';
use File::Temp 'tempfile';

our @EXPORT = (qw( run_gimli evals_ok evals_exact_ok evals_same_ok
                   evals_true_ok evals_false_ok      ));

sub run_gimli {

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
        exec "app/gimli" or
        exec "dist/build/gimli/gimli"
                                or die "could not exec gimli: $!";
    }

    wait;

    seek $fh, 0, 0 or die "could not seek to start of tempfile: $!";
    my $results = do { local $/; <$fh> };
    close $fh;

    return $results;
}

sub evals_base {
    no warnings 'once';
    my ($processfn, $expr, $expected_result, $name) = @_;
    my $test_fn = ref $expected_result ? *Test::More::like : *Test::More::is;
    my $result = run_gimli($expr);
    for ($result) { $processfn->() }
    my $level = $Test::Builder::Level;
    {  local $Test::Builder::Level = $level + 1;
       $test_fn->($result, $expected_result,
                  $name || "$expr ==> $expected_result");
    }
}

sub evals_ok {
    my $level = $Test::Builder::Level;
    {   local $Test::Builder::Level = $level + 1;
        evals_base( sub { s/^\s+//s; s/\s+$//s }, @_ );
    }
}

sub evals_exact_ok {
    my $level = $Test::Builder::Level;
    {   local $Test::Builder::Level = $level + 1;
        evals_base( sub { }, @_ );
    }
}

sub evals_same_ok {
    my ($expr, $name) = @_;
    my $level = $Test::Builder::Level;
    {   local $Test::Builder::Level = $level + 1;
        evals_ok($expr, $expr, $name);
    }
}

sub evals_true_ok {
    my ($expr, $name) = @_;
    my $level = $Test::Builder::Level;
    {   local $Test::Builder::Level = $level + 1;
        evals_ok($expr, "T", $name);
    }
}

sub evals_false_ok {
    my ($expr, $name) = @_;
    my $level = $Test::Builder::Level;
    {   local $Test::Builder::Level = $level + 1;
        evals_ok($expr, "F", $name);
    }
}


1;
