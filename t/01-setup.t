#!perl -T

use strict;
use warnings;
use Test::More;
use JIRA::Client;

my $conf;
if (-f 't/online.enabled') {
    $conf = do 't/online.enabled';
    plan tests => 6;
}
else {
    plan skip_all => 'Online tests are disabled.';
}

my $jira = eval {JIRA::Client->new($conf->{url}, $conf->{user}, $conf->{pass})};

ok(defined $jira, 'new returns')
    and ok(ref $jira, 'new returns an object')
    and is(ref $jira, 'JIRA::Client', 'new returns a correct object')
    or BAIL_OUT("Cannot proceed without a JIRA::Client object: $@\n");

my $issue = eval {$jira->getIssue("$conf->{prjkey}-1")};

ok(defined $issue, 'getissue returns')
    and ok(ref $issue, 'getIssue returns an object')
    and is(ref $issue, 'RemoteIssue', 'getIssue returns a RemoteIssue object')
    or BAIL_OUT("Cannot proceed because I cannot get anything from the server: $@\n");

#$jira->set_filter_iterator('test-filter');
#$issue = eval {$jira->next_issue()};
#ok(defined $issue, 'next_issue returns an issue');
