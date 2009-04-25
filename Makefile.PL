use strict;
use warnings;
use ExtUtils::MakeMaker qw(WriteMakefile prompt);
use IO::Socket ();
use Getopt::Long  qw(GetOptions);

# clean up the online testing flag file.
unlink("t/online.enabled");

my $usage        = "$0 [--[no]online-tests]\n";
my $online_tests = 0;
GetOptions(
    'online-tests!' => \$online_tests,
) or die $usage;

#
# Check if we have internet connection
# (I lifted this code from Net::DNS, which lifted it from LWP... )
#
unless (defined $online_tests) {
    my $s = IO::Socket::INET->new(
	PeerAddr => "jira.atlassian.com:80",
	Timeout  => 10,
    );
    if ($s) {
	close($s);
	print <<EOT;

You appear to be directly connected to the Internet.  I have some tests
that try to access the Atlassian's JIRA demo server.

EOT
	$online_tests = prompt("Do you want to enable these tests?", "y") =~ /^y/i ? 1 : 0;
    }
}

if ($online_tests) {
    open(ENABLED, ">t/online.enabled") or die "Can't touch ./t/online.enabled: $!";
    close(ENABLED)                     or die "Can't touch ./t/online.enabled: $!";
}

WriteMakefile(
    NAME                => 'JIRA::Client',
    AUTHOR              => 'Gustavo Chaves <gnustavo@cpan.org>',
    VERSION_FROM        => 'lib/JIRA/Client.pm',
    ABSTRACT_FROM       => 'lib/JIRA/Client.pm',
    PL_FILES            => {},
    PREREQ_PM => {
        'Test::More' => 0,
	'IO::Socket' => 0,
	'SOAP::Lite' => 0,
    },
    dist                => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
    clean               => { FILES => 'JIRA-Client-*' },
);
