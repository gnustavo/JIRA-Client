use strict;
use warnings;
use Test::More;
use JIRA::Client;

my $conf;
if (-f 't/demo.enabled') {
    $conf = do 't/demo.enabled';
    plan tests => 14;
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
    project         => 'TST',
    type            => 1,	# Bug
    assignee        => 'gnustavo',
    priority        => 5,	# Trivial
    components      => [10003],	# External Develpment
    affectsVersions => [10000],	# Test 1.0
    summary         => 'JIRA::Client test',
    description     => '[Author\'s test|http://search.cpan.org/perldoc?JIRA::Client].',
})};

ok(defined $issue, 'create_issue returns')
    and ok(ref $issue, 'create_issue returns an object')
    and is(ref $issue, 'RemoteIssue', "create_issue returns a RemoteIssue object (http://sandbox.onjira.com/browse/$issue->{key})")
    or BAIL_OUT("Cannot proceed because I cannot create an issue: $@\n");

$issue = eval {$jira->getIssue($issue->{key})};

ok(defined $issue, 'getissue returns')
    and ok(ref $issue, 'getIssue returns an object')
    and is(ref $issue, 'RemoteIssue', 'getIssue returns a RemoteIssue object')
    and is($issue->{summary}, 'JIRA::Client test', 'getIssue returns the correct issue')
    or BAIL_OUT("Cannot proceed because I cannot get anything from the server: $@\n");

my $pissue = eval {$jira->progress_workflow_action_safely($issue, 5)}; # Resolve issue

ok(defined $pissue, 'progress_workflow_action_safely returns')
    and ok(ref $pissue, 'next_ssue returns an object')
    and is(ref $pissue, 'RemoteIssue', 'progress_workflow_action_safely returns a RemoteIssue object')
    and isnt($issue->{status}, $pissue->{status}, 'progress_workflow_action_safely progressed the issue');
