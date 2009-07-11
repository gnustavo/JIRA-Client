#!perl -T

use strict;
use warnings;
use Test::More;
use JIRA::Client;

my $conf;
if (-f 't/online.enabled') {
    $conf = do 't/online.enabled';
    plan tests => 10;
}
else {
    plan skip_all => 'Online tests are disabled.';
}

my $jira = eval {JIRA::Client->new($conf->{url}, $conf->{user}, $conf->{pass})};

ok(defined $jira, 'new returns')
    and ok(ref $jira, 'new returns an object')
    and is(ref $jira, 'JIRA::Client', 'new returns a correct object')
    or BAIL_OUT("Cannot proceed without a JIRA::Client object: $@\n");

my $issue = eval {$jira->create_issue({
    project  => $conf->{prjkey},
    type     => 1,
    assignee => $conf->{user},
    summary  => 'JIRA::Client test',
})};

ok(defined $issue, 'create_issue returns')
    and ok(ref $issue, 'create_issue returns an object')
    and is(ref $issue, 'RemoteIssue', 'create_issue returns a RemoteIssue object')
    or BAIL_OUT("Cannot proceed because I cannot create an issue: $@\n");

$issue = eval {$jira->getIssue($issue->{key})};

ok(defined $issue, 'getissue returns')
    and ok(ref $issue, 'getIssue returns an object')
    and is(ref $issue, 'RemoteIssue', 'getIssue returns a RemoteIssue object')
    and is($issue->{summary}, 'JIRA::Client test', 'getIssue returns the correct issue')
    or BAIL_OUT("Cannot proceed because I cannot get anything from the server: $@\n");
