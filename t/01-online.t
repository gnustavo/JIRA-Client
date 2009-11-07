use strict;
use warnings;
use Test::More;
use JIRA::Client;

my $conf;
if (-f 't/online.enabled') {
    $conf = do 't/online.enabled';
    plan tests => 39;
}
else {
    plan skip_all => 'Online tests are disabled.';
}

my $jira = eval {JIRA::Client->new($conf->{url}, $conf->{user}, $conf->{pass})};

ok(defined $jira, 'new returns')
    and ok(ref $jira, 'new returns an object')
    and is(ref $jira, 'JIRA::Client', 'new returns a correct object')
    or BAIL_OUT("Cannot proceed without a JIRA::Client object: $@\n");

my $types = eval {$jira->get_issue_types()};

ok(defined $types, 'get_issue_types returns')
    and ok(ref $types && ref $types eq 'HASH', 'get_issue_types returns a hash reference')
    and ok(exists $types->{$conf->{type}}, "issue type $conf->{type} exists");

my $prios = eval {$jira->get_priorities()};

ok(defined $prios, 'get_priorities returns')
    and ok(ref $prios && ref $prios eq 'HASH', 'get_priorities returns a hash reference')
    and ok(exists $prios->{$conf->{priority}}, "Priority $conf->{priority} exists");

my $cfs = eval {$jira->get_custom_fields()};

ok(defined $cfs, 'get_custom_fields')
    and ok(ref $cfs && ref $cfs eq 'HASH', 'get_custom_fields returns a hash reference')
    and ok(exists $cfs->{$conf->{cfname}}, "Custom field $conf->{cfname} exists");

my $components = eval {$jira->get_components($conf->{prjkey})};

ok(defined $components, 'get_components')
    and ok(ref $components && ref $components eq 'HASH', 'get_components returns a hash reference')
    and ok(exists $components->{$conf->{component}}, "Component $conf->{component} exists");

my $versions = eval {$jira->get_versions($conf->{prjkey})};

ok(defined $versions, 'get_versions')
    and ok(ref $versions && ref $versions eq 'HASH', 'get_versions returns a hash reference')
    and ok(exists $versions->{$conf->{version}}, "Version $conf->{version} exists");

my $filters = eval {$jira->get_favourite_filters()};

ok(defined $filters, 'get_favourite_filters')
    and ok(ref $filters && ref $filters eq 'HASH', 'get_favourite_filters returns a hash reference')
    and ok(exists $filters->{$conf->{filter}}, "Filter $conf->{filter} exists");

my $issue = eval {$jira->create_issue({
    project         => $conf->{prjkey},
    type            => $conf->{type},
    assignee        => $conf->{user},
    priority        => $conf->{priority},
    components      => [$conf->{component}],
    affectsVersions => [$conf->{version}],
    custom_fields   => {$conf->{cfname} => $conf->{cfvalue}},
    summary         => 'JIRA::Client test',
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

my ($value) = eval {$jira->get_issue_custom_field_values($issue, $conf->{cfname})};

ok(defined $value, 'get_issue_custom_field_values returns')
    and ok(ref $value && ref $value eq 'ARRAY', 'get_issue_custom_field_values returns an ARRAY ref')
    and is($value->[0], $conf->{cfvalue}, 'get_issue_custom_field_values returns the correct value');

eval {$jira->set_filter_iterator($conf->{filter})};

is($@, '', 'set_filter_iterator works');

my $nissue = eval {$jira->next_issue()};

ok(defined $nissue, 'next_issue returns')
    and ok(ref $nissue, 'next_ssue returns an object')
    and is(ref $nissue, 'RemoteIssue', 'next_issue returns a RemoteIssue object');

my $actions = eval {$jira->getAvailableActions($issue->{key})};

my $pissue = eval {$jira->progress_workflow_action_safely($issue, $conf->{action})};

ok(defined $pissue, 'progress_workflow_action_safely returns')
    and ok(ref $pissue, 'next_ssue returns an object')
    and is(ref $pissue, 'RemoteIssue', 'progress_workflow_action_safely returns a RemoteIssue object')
    and isnt($issue->{status}, $pissue->{status}, 'progress_workflow_action_safely progressed the issue');
