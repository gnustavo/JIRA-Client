use strict;
use warnings;
use ExtUtils::MakeMaker qw(WriteMakefile prompt);
use IO::Socket ();
use Getopt::Long  qw(GetOptions);

# clean up the online testing flag file.
unlink("t/online.enabled");

my $usage = "$0 [--author-tests] [--online-tests=JIRAURL,USER,PASS,PRJKEY,ISSUETYPE,PRIORITY,COMPONENT,VERSION,CUSTOMFIELDNAME,CUSTOMFIELDVALUE,FILTER,ACTION]\n";
my $author_tests = 0;
my $online_tests;
GetOptions(
    'author-tests'   => \$author_tests,
    'online-tests=s' => \$online_tests,
) or die $usage;

if ($author_tests) {
    open(ENABLED, '>t/author.enabled') or die "Can't touch ./t/author.enabled: $!";
    close(ENABLED)                     or die "Can't touch ./t/author.enabled: $!";
}

if ($online_tests) {
    # These are the author's personal test configuration. :-)
    $online_tests = 'http://localhost:8080/,gustavo,senhaforte,TST,Bug,Major,comp1,1.1,cor,azul,filtro,Close Issue'
	if $online_tests eq 'default';
    my ($url, $user, $pass, $prjkey, $type, $prio, $component, $version, $cfname, $cfvalue, $filter, $action) = split /,/, $online_tests;
    defined $prjkey or die $usage;
    open(ENABLED, ">t/online.enabled") or die "Can't touch ./t/online.enabled: $!";
    print ENABLED <<"EOS";
{
    url       => '$url',
    user      => '$user',
    pass      => '$pass',
    prjkey    => '$prjkey',
    type      => '$type',
    priority  => '$prio',
    component => '$component',
    version   => '$version',
    cfname    => '$cfname',
    cfvalue   => '$cfvalue',
    filter    => '$filter',
    action    => '$action',
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
    clean               => { FILES => 'JIRA-Client-* t/author.enabled t/online.enabled' },
);
