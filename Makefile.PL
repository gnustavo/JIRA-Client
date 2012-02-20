use strict;
use warnings;
use ExtUtils::MakeMaker qw(WriteMakefile prompt);
use IO::Socket ();
use Getopt::Long  qw(GetOptions);

# clean up the online testing flag file.
unlink("t/author.enabled");

my $usage = "$0 [--author-tests=PASSWORD]\n";
my $author_tests;
GetOptions(
    'author-tests=s' => \$author_tests,
) or die $usage;

if ($author_tests) {
    # This relies on the author's personal test configuration. :-)
    open my $enabled, '>', 't/author.enabled' or die "Can't touch ./t/author.enabled: $!";
    print $enabled <<"EOS";
{
    url  => 'https://jira.atlassian.com/',
    user => 'gnustavo',
    pass => '$author_tests',
};
EOS
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
    clean               => { FILES => 'JIRA-Client-* t/*.enabled' },
);
