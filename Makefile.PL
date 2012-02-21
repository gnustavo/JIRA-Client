use strict;
use warnings;
use ExtUtils::MakeMaker qw(WriteMakefile);

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
