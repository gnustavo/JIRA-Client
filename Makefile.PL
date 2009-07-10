use strict;
use warnings;
use ExtUtils::MakeMaker qw(WriteMakefile prompt);
use IO::Socket ();
use Getopt::Long  qw(GetOptions);

# clean up the online testing flag file.
unlink("t/online.enabled");

my $usage        = "$0 [--online-tests=JIRA_URL,USER,PASS,PRJKEY]\n";
my $online_tests;
GetOptions(
    'online-tests=s' => \$online_tests,
) or die $usage;

if ($online_tests) {
    my ($url, $user, $pass, $prjkey) = split /,/, $online_tests;
    defined $prjkey or die $usage;
    open(ENABLED, ">t/online.enabled") or die "Can't touch ./t/online.enabled: $!";
    print ENABLED <<"EOS";
{
    url    => '$url',
    user   => '$user',
    pass   => '$pass',
    prjkey => '$prjkey',
};
EOS
    close(ENABLED) or die "Can't touch ./t/online.enabled: $!";
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
