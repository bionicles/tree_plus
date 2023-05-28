# PerlTest.pl
package PerlTest;

sub new {
    my $class = shift;
    my $self = {};
    bless $self, $class;
    return $self;
}

sub hello {
    print "Hello, World!\n";
}

sub say_hello {
    my $person = PerlTest->new();
    $person->hello();
}

1;
