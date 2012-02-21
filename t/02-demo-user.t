# Hey Emacs, this is -*- perl -*-
use strict;
use warnings;
use Test::More;
use JIRA::Client;

my $conf;
if (-f 't/author.enabled') {
    $conf = do 't/author.enabled';
    plan tests => 13;
}
else {
    plan skip_all => 'Demo tests are disabled.';
}

my $jira = eval {JIRA::Client->new($conf->{url}, $conf->{user}, $conf->{pass})};

ok(defined $jira, 'new returns')
    and ok(ref $jira, 'new returns an object')
    and is(ref $jira, 'JIRA::Client', 'new returns a correct object')
    or BAIL_OUT("Cannot proceed without a JIRA::Client object: $@\n");

my $issue = eval {$jira->create_issue({
    project         => 'DEMO',
    type            => 'Bug',
    assignee        => 'gnustavo',
    priority        => 'Trivial',
    components      => [qw/Fuselage Wings/],
    affectsVersions => [qw/Design Test/],
    fixVersions     => [qw/Build Production/],
    summary         => 'JIRA::Client test',
    description     => '[Author\'s test|http://search.cpan.org/perldoc?JIRA::Client].',
    duedate         => '2020-01-01',
})};

ok(defined $issue, 'create_issue returns')
    and ok(ref $issue, 'create_issue returns an object')
    and is(ref $issue, 'RemoteIssue', "create_issue returns a RemoteIssue object (https://jira.atlassian.com/browse/$issue->{key})")
    or BAIL_OUT("Cannot proceed because I cannot create an issue: $@\n");

$issue = eval {$jira->getIssue($issue->{key})};

ok(defined $issue, 'getIssue returns')
    and ok(ref $issue, 'getIssue returns an object')
    and is(ref $issue, 'RemoteIssue', 'getIssue returns a RemoteIssue object')
    and is($issue->{summary}, 'JIRA::Client test', 'getIssue returns the correct issue')
    or BAIL_OUT("Cannot proceed because I cannot get anything from the server: $@\n");

my $subissue = eval {$jira->create_issue({
    project => 'TST',
    type    => 'Sub-task',
    parent  => $issue->{key},
    summary => 'JIRA::Client test Sub-task',
})};

ok(defined $issue, 'create_issue sub-task returns')
    and ok(ref $issue, 'create_issue sub-task returns an object')
    and is(ref $issue, 'RemoteIssue', "create_issue sub-task returns a RemoteIssue object (http://sandbox.onjira.com/browse/$issue->{key})")
    or BAIL_OUT("Cannot proceed because I cannot create an issue: $@\n");

