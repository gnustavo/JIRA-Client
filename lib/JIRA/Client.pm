package JIRA::Client;

use strict;
use warnings;
use Carp;
use SOAP::Lite;

=head1 NAME

JIRA::Client - An extended interface to JIRA's SOAP API.

=head1 VERSION

Version 0.29

=cut

our $VERSION = '0.29';

=head1 SYNOPSIS

  use JIRA::Client;

  my $jira = JIRA::Client->new('http://jira.example.com/jira', 'user', 'passwd');

  my $issue = $jira->create_issue(
    {
      project => 'TST',
      type => 'Bug',
      summary => 'Summary of the bug',
      assignee => 'gustavo',
      components => ['compa', 'compb'],
      fixVersions => ['1.0.1'],
      custom_fields => {Language => 'Perl', Architecture => 'Linux'},
    }
  );

  $issue = $jira->getIssue('TST-123');

  $jira->set_filter_iterator('my-filter');
  while (my $issue = $jira->next_issue()) {
      # ...
  }

=head1 DESCRIPTION

JIRA is a proprietary bug tracking system from Atlassian
(L<http://www.atlassian.com/software/jira/>).

This module implements an Object Oriented wrapper around JIRA's SOAP
API, which is specified in
L<http://docs.atlassian.com/software/jira/docs/api/rpc-jira-plugin/latest/com/atlassian/jira/rpc/soap/JiraSoapService.html>.
(This version is known work against JIRA 4.4.)

Moreover, it implements some other methods to make it easier to do
some common operations.

=head1 API METHODS

With the exception of the API C<login> and C<logout> methods, which
aren't needed, all other methods are available through the
JIRA::Client object interface. You must call them with the same name
as documented in the specification but you should not pass the
C<token> argument, because it is supplied transparently by the
JIRA::Client object.

Some of the API methods require hard-to-build data structures as
arguments. This module tries to make them easier to call by accepting
simpler structures and implicitly constructing the more elaborated
ones before making the actual SOAP call. Note that this is an option,
i.e, you can either pass the elaborate structures by yourself or the
simpler ones in the call.

The items below are all the implemented implicit conversions. Wherever
a parameter of the type specified first is required (as an rvalue, not
as an lvalue) by an API method you can safely pass a value of the type
specified second.

=over 4

=item A B<issue key> as a string can be specified by a B<RemoteIssue> object.

=item A B<RemoteComment> object can be specified by a string.

=item A B<filterId> as a string can be specified by a B<RemoteFilter> object.

=item A B<RemoteFieldValue> object array can be specified by a hash mapping field names to values.

=back

=head1 EXTRA METHODS

This module implements some extra methods to add useful functionality
to the API. They are described below. Note that their names don't
follow the CamelCase convention used by the native API methods but the
more Perlish underscore_separated_words convention so that you can
distinguish them and we can avoid future name clashes.

=over 4

=item B<new> JIRAURL, USER, PASSWD [, <SOAP::Lite arguments>]

The JIRA::Client constructor requires three arguments. JIRAURL is
JIRA's base URL from which will be constructed it's WSDL descriptor as
C<$JIRAURL/rpc/soap/jirasoapservice-v2?wsdl>. USER and PASSWD are the
credentials that will be used to authenticate into JIRA. Any other
arguments will be passed to the L<SOAP::Lite> object that will be
created to talk to JIRA.

=cut

sub new {
    my ($class, $base_url, $user, $pass, @args) = @_;

    my $soap = SOAP::Lite->proxy("$base_url/rpc/soap/jirasoapservice-v2?wsdl", @args);

    # Make all scalars be encoded as strings by default.
    %{$soap->typelookup()} = (default => [0, sub {1}, 'as_string']);

    my $auth = $soap->login($user, $pass);
    croak $auth->faultcode(), ', ', $auth->faultstring()
        if defined $auth->fault();

    my $self = {
        soap  => $soap,
        auth  => $auth->result(),
        iter  => undef,
        cache => {
            components => {}, # project_key => {name => RemoteComponent}
            versions   => {}, # project_key => {name => RemoteVersion}
        },
    };

    return bless $self, $class;
}

sub DESTROY {
    # FIXME - This call to logout during global destruction in the
    # context of the SVN::Hooks module doesn't work right. When it's
    # called the 'soap' member is undefined for a reason that escapes
    # me so far. Getting rid of the DESTROY method doesn't work either
    # because it would trigger a call to AUTOLOAD which is unable to
    # do it correctly. I think a call to logout is proper here to shut
    # down the SOAP connection cleanly, but it doesn't seem to hurt
    # not to call it.

    # shift->logout();
}

# These are some helper functions to convert names into ids.

sub _convert_type {
    my ($self, $hash) = @_;
    my $type = $hash->{type};
    if ($type =~ /\D/) {
        my $types = $self->get_issue_types();
        croak "There is no issue type called '$type'.\n"
            unless exists $types->{$type};
        $hash->{type} = $types->{$type}{id};
    }
    return;
}

sub _convert_priority {
    my ($self, $hash) = @_;
    my $prio = $hash->{priority};
    if ($prio =~ /\D/) {
        my $prios = $self->get_priorities();
        croak "There is no priority called '$prio'.\n"
            unless exists $prios->{$prio};
        $hash->{priority} = $prios->{$prio}{id};
    }
    return;
}

sub _convert_resolution {
    my ($self, $hash) = @_;
    my $resolution = $hash->{resolution};
    if ($resolution =~ /\D/) {
        my $resolutions = $self->get_resolutions();
        croak "There is no resolution called '$resolution'.\n"
            unless exists $resolutions->{$resolution};
        $hash->{resolution} = $resolutions->{$resolution}{id};
    }
    return;
}

sub _convert_security_level {
    my ($self, $seclevel) = @_;
    if ($seclevel =~ /\D/) {
        my $seclevels = $self->get_security_levels();
        croak "There is no security level called '$seclevel'.\n"
            unless exists $seclevels->{$seclevel};
        $seclevel = $seclevels->{$seclevel}{id};
    }
    return $seclevel;
}

sub _convert_components {
    my ($self, $hash, $key, $project) = @_;
    my $comps = $hash->{components};
    croak "The 'components' value must be an ARRAY ref.\n"
       unless ref $comps && ref $comps eq 'ARRAY';
    my $pcomps;                 # project components
    foreach my $c (@{$comps}) {
       next if ref $c;
       if ($c =~ /\D/) {
           # It's a component name. Let us convert it into its id.
	   croak "Cannot convert component names because I don't know for which project.\n"
	       unless $project;
           $pcomps = $self->get_components($project) unless defined $pcomps;
           croak "There is no component called '$c'.\n" unless exists $pcomps->{$c};
           $c = $pcomps->{$c}{id};
       }
       # Now we can convert it into an object.
       $c = RemoteComponent->new($c);
    }
    return;
}

sub _convert_versions {
    my ($self, $hash, $key, $project) = @_;
    my $versions = $hash->{$key};
    croak "The '$versions' value must be a ARRAY ref.\n"
       unless ref $versions && ref $versions eq 'ARRAY';
    my $pversions;
    foreach my $v (@{$versions}) {
       next if ref $v;
       if ($v =~ /\D/) {
           # It is a version name. Let us convert it into its id.
	   croak "Cannot convert version names because I don't know for which project.\n"
	       unless $project;
           $pversions = $self->get_versions($project) unless defined $pversions;
           croak "There is no version called '$v'.\n" unless exists $pversions->{$v};
           $v = $pversions->{$v}{id};
       }
       # Now we can convert it into an object.
       $v = RemoteVersion->new($v);
    }
    return;
}

sub _convert_duedate {
    my ($self, $hash) = @_;
    my $duedate = $hash->{duedate};
    if (ref $duedate) {
	croak "duedate fields must be set with DateTime references.\n"
	    unless ref $duedate eq 'DateTime';
	$hash->{duedate} = $duedate->strftime('%d/%B/%y');
    }
    elsif (my ($year, $month, $day) = ($duedate =~ /^(\d{4})-(\d{2})-(\d{2})/)) {
	$month >= 1 and $month <= 12
	    or croak "Invalid duedate ($hash->{duedate})";
	$hash->{duedate} = join(
	    '/',
	    $day,
	    qw/zero Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec/[$month],
	    substr($year, 2, 2),
	);
    }
    return;
}

sub _convert_custom_fields {
    my ($self, $hash) = @_;
    my $custom_fields = $hash->{custom_fields};
    croak "The 'custom_fields' value must be a HASH ref.\n"
        unless ref $custom_fields && ref $custom_fields eq 'HASH';
    my %id2values;
    while (my ($id, $values) = each %$custom_fields) {
	my $realid = $id;
        unless ($realid =~ /^customfield_\d+$/) {
            my $cfs = $self->get_custom_fields();
            croak "Can't find custom field named '$id'.\n"
                unless exists $cfs->{$id};
            $realid = $cfs->{$id}{id};
        }

	# Custom field values must be specified as ARRAYs but we allow for some short-cuts.
	if (! ref $values) {
	    $id2values{$realid} = [$values];
	} elsif (ref $values eq 'ARRAY') {
	    $id2values{$realid} = $values;
	} elsif (ref $values eq 'HASH') {
	    # This is a short-cut for a Cascading select field, which
	    # must be specified like this: http://tinyurl.com/2bmthoa
	    # The short-cut requires a HASH where each cascading level
	    # is indexed by its level number, starting at zero.
	    foreach my $level (sort {$a <=> $b} keys %$values) {
		my $level_values = $values->{$level};
		$level_values = [$level_values] unless ref $level_values;
		if ($level eq '0') {
		    # The first level doesn't have a colon
		    $id2values{$realid} = $level_values
		} elsif ($level =~ /^\d+$/) {
		    $id2values{"$realid:$level"} = $level_values;
		} else {
		    croak "Invalid cascading field values level spec ($level). It must be a natural number.\n";
		}
	    }
	} else {
	    croak "Custom field '$id' got a '", ref($values), "' reference as a value.\nValues can only be specified as scalars, ARRAYs, or HASHes though.\n";
	}
    }
    $hash->{custom_fields} = \%id2values;
    return;
}

my %_converters = (
    affectsVersions => \&_convert_versions,
    components      => \&_convert_components,
    custom_fields   => \&_convert_custom_fields,
    duedate         => \&_convert_duedate,
    fixVersions     => \&_convert_versions,
    priority        => \&_convert_priority,
    resolution      => \&_convert_resolution,
    type            => \&_convert_type,
);

=item B<create_issue> HASH_REF [, SECURITYLEVEL]

Creates a new issue given a hash containing the initial values for its
fields and, optionally, a security-level. The hash must specify at
least the fields C<project>, C<summary>, and C<type>.

This is an easier to use version of the createIssue API method. For
once it accepts symbolic values for some of the issue fields that the
API method does not. Specifically:

=over 4

=item C<type> can be specified by I<name> instead of by I<id>.

=item C<priority> can be specified by I<name> instead of by I<id>.

=item C<component> can be specified by a list of component I<names> or
I<ids> instead of a list of C<RemoteComponent> objects.

=item C<affectsVersions> and C<fixVersions> can be specified by a list
of version I<names> or I<ids> instead of a list of C<RemoteVersion>
objects.

=item C<duedate> can be specified in the ISO standard format
(YYYY-MM-DD...) instead of the required format (d/MMM/yy).

=back

It accepts a 'magic' field called B<parent>, which specifies the issue
key from which the created issue must be a sub-task.

It accepts another 'magic' field called B<custom_fields> to make it
easy to set custom fields. It accepts a hash mapping each custom field
to its value. The custom field can be specified by its id (in the
format B<customfield_NNNNN>) or by its name, in which case the method
will try to convert it to its id. Note that to do that conversion the
user needs administrator rights.

A simple custom field value can be specified by a scalar, which will
be properly placed inside an ARRAY in order to satisfy the
B<RemoteFieldValue>'s structure.

Cascading select fields are properly specified like this:
http://tinyurl.com/2bmthoa. The magic short-cut requires a HASH where
each cascading level is indexed by its level number, starting at
zero. So, instead of specifying it like this:

    {
        id => 'customfield_10011',
        values => [ SOAP::Data->type(string => '10031' ) ]
    },
    {
        id => 'customfield_10011:1',
        values => [ SOAP::Data->type(string => '10188') ],
    },

You can do it like this:

    {customfield_10011 => {'0' => 10031, '1' => 10188}},


=cut

sub create_issue
{
    my ($self, $hash, $seclevel) = @_;
    croak "create_issue requires an argument.\n"
        unless defined $hash;
    croak "create_issue's argument must be a HASH ref.\n"
        unless ref $hash && ref $hash eq 'HASH';
    for my $field (qw/project summary type/) {
        croak "create_issue's HASH ref must define a '$field'.\n"
            unless exists $hash->{$field};
    }

    # Convert some fields' values
    foreach my $field (grep {exists $_converters{$_}} keys %$hash) {
	$_converters{$field}->($self, $hash, $field, $hash->{project});
    }

    # Substitute customFieldValues for custom_fields
    if (my $cfs = delete $hash->{custom_fields}) {
        $hash->{customFieldValues} = [map {RemoteCustomFieldValue->new($_, $cfs->{$_})} keys %$cfs];
    }

    if (my $parent = delete $hash->{parent}) {
	if (defined $seclevel) {
	    return $self->createIssueWithParentWithSecurityLevel($hash, _convert_security_level($self, $parent, $seclevel));
	} else {
	    return $self->createIssueWithParent($hash, $parent);
	}
    } else {
	if (defined $seclevel) {
	    return $self->createIssueWithSecurityLevel($hash, _convert_security_level($self, $seclevel));
	} else {
	    return $self->createIssue($hash);
	}
    }
}

=item B<update_issue> ISSUE_OR_KEY, HASH_REF

Update a issue given a hash containing the values for its fields. The
first argument may be an issue key or a RemoteIssue object.

This is an easier to use version of the updateIssue API method because
it accepts the same shortcuts that create_issue does.

=cut

sub update_issue
{
    my ($self, $issue, $params) = @_;
    my $key;
    if (ref $issue) {
	croak "update_issue's first argument must be a RemoteIssue reference.\n"
	    unless ref $issue eq 'RemoteIssue';
	$key = $issue->{key};
    }
    else {
	$key = $issue;
	$issue = $self->getIssue($key);
    }

    croak "update_issue requires two arguments.\n"
        unless defined $params;
    croak "update_issue's second argument must be a HASH ref.\n"
        unless ref $params && ref $params eq 'HASH';

    my ($project) = ($key =~ /^([^-]+)/);

    # Convert some fields' values
    foreach my $field (grep {exists $_converters{$_}} keys %$params) {
	$_converters{$field}->($self, $params, $field, $project);
    }

    # Convert RemoteComponent objects into component ids
    if (my $comps = $params->{components}) {
        $_ = $_->{id} foreach @$comps;
    }

    # Convert RemoteVersion objects into version ids
    for my $field (qw/fixVersions affectsVersions/) {
        if (my $versions = $params->{$field}) {
            $_ = $_->{id} foreach @$versions;
        }
    }
    # Due to a bug in JIRA
    # (http://jira.atlassian.com/browse/JRA-12300) we have to
    # substitute 'versions' for the 'affectsVersions' key
    if (my $versions = delete $params->{affectsVersions}) {
        $params->{versions} = $versions;
    }

    # Expand the custom_fields hash into the custom fields themselves.
    if (my $custom_fields = delete $params->{custom_fields}) {
        while (my ($id, $values) = each %$custom_fields) {
            $params->{$id} = $values;
        }
    }

    return $self->updateIssue($key, $params);
}

=item B<get_issue_types>

Returns a hash mapping the server's issue type names to the
RemoteIssueType objects describing them.

=cut

sub get_issue_types {
    my ($self) = @_;
    $self->{cache}{issue_types} ||= {map {$_->{name} => $_} @{$self->getIssueTypes()}};
    return $self->{cache}{issue_types};
}

=item B<get_statuses>

Returns a hash mapping the server's status names to the
RemoteStatus objects describing them.

=cut

sub get_statuses {
    my ($self) = @_;
    $self->{cache}{statuses} ||= {map {$_->{name} => $_} @{$self->getStatuses()}};
    return $self->{cache}{statuses};
}

=item B<get_priorities>

Returns a hash mapping a server's priorities names to the
RemotePriority objects describing them.

=cut

sub get_priorities {
    my ($self) = @_;
    $self->{cache}{priorities} ||= {map {$_->{name} => $_} @{$self->getPriorities()}};
    return $self->{cache}{priorities};
}

=item B<get_resolutions>

Returns a hash mapping a server's resolution names to the
RemoteResolution objects describing them.

=cut

sub get_resolutions {
    my ($self) = @_;
    $self->{cache}{resolutions} ||= {map {$_->{name} => $_} @{$self->getResolutions()}};
    return $self->{cache}{resolutions};
}

=item B<get_security_levels>

Returns a hash mapping a server's security level names to the
RemoteSecurityLevel objects describing them.

=cut

sub get_security_levels {
    my ($self) = @_;
    $self->{cache}{seclevels} ||= {map {$_->{name} => $_} @{$self->getSecurityLevels()}};
    return $self->{cache}{seclevels};
}

=item B<get_custom_fields>

Returns a hash mapping JIRA's custom field names to the RemoteField
representing them. It's useful since when you get a RemoteIssue object
from this API it doesn't contain the custom field's names, but just
their identifiers. From the RemoteField object you can obtain the
field's B<id>, which is useful when calling the B<updateIssue> method.

The method calls the getCustomFields API method the first time and
keeps the custom fields information in a cache.

=cut

sub get_custom_fields {
    my ($self) = @_;
    $self->{cache}{custom_fields} ||= {map {$_->{name} => $_} @{$self->getCustomFields()}};
    return $self->{cache}{custom_fields};
}

=item B<set_custom_fields> HASHREF

Passes a hash mapping JIRA's custom field names to the RemoteField
representing them to populate the custom field's cache. This can be
useful if you don't have administrative privileges to the JIRA
instance, since only administrators can call the B<getCustomFields>
API method.

=cut

sub set_custom_fields {
    my ($self, $cfs) = @_;
    $self->{cache}{custom_fields} = $cfs;
    return;
}

=item B<get_components> PROJECT_KEY

Returns a hash mapping a project's components names to the
RemoteComponent objects describing them.

=cut

sub get_components {
    my ($self, $project_key) = @_;
    $self->{cache}{components}{$project_key} ||= {map {$_->{name} => $_} @{$self->getComponents($project_key)}};
    return $self->{cache}{components}{$project_key};
}

=item B<get_versions> PROJECT_KEY

Returns a hash mapping a project's versions names to the RemoteVersion
objects describing them.

=cut

sub get_versions {
    my ($self, $project_key) = @_;
    $self->{cache}{versions}{$project_key} ||= {map {$_->{name} => $_} @{$self->getVersions($project_key)}};
    return $self->{cache}{versions}{$project_key};
}

=item B<get_favourite_filters>

Returns a hash mapping the user's favourite filter names to its filter
ids.

=cut

sub get_favourite_filters {
    my ($self) = @_;
    $self->{cache}{filters} ||= {map {$_->{name} => $_} @{$self->getFavouriteFilters()}};
    return $self->{cache}{filters};
}

=item B<set_filter_iterator> FILTER [, CACHE_SIZE]

Sets up an iterator for the filter identified by FILTER. It must
be called before calls to B<next_issue>.

FILTER can be either a filter I<id> or a filter I<name>, in which case
it's converted to a filter id with a call to C<getSavedFilters>.

CACHE_SIZE defines the number of issues that will be pre-fetched by
B<nect_issue> using C<getIssuesFromFilterWithLimit>. If not specified,
a suitable default will be used.

=cut

sub set_filter_iterator {
    my ($self, $filter, $cache_size) = @_;

    if ($filter =~ /\D/) {
        my $filters = $self->getSavedFilters();
        foreach my $f (@$filters) {
            if ($f->{name} eq $filter) {
                $filter = $f->{id};
                last;
            }
        }
        croak "Can't find filter '$filter'\n" if $filter =~ /\D/;
    }

    if ($cache_size) {
        croak "set_filter_iterator's second arg must be a number ($cache_size).\n"
            if $cache_size =~ /\D/;
    }

    $self->{iter} = {
        id     => $filter,
        offset => 0,  # offset to be used in the next call to getIssuesFromFilterWithLimit
        issues => [], # issues returned by the last call to getIssuesFromFilterWithLimit
        size   => $cache_size || 128,
    };

    return;
}

=item B<next_issue>

This must be called after a call to B<set_filter_iterator>. Each call
returns a reference to the next issue from the filter. When there are
no more issues it returns undef.

=cut

sub next_issue {
    my ($self) = @_;
    defined $self->{iter}
        or croak "You must call setFilterIterator before calling nextIssue\n";
    my $iter = $self->{iter};
    if (@{$iter->{issues}} == 0) {
        if ($iter->{id}) {
            my $issues = eval {$self->getIssuesFromFilterWithLimit($iter->{id}, $iter->{offset}, $iter->{size})};
            if ($@) {
                # The getIssuesFromFilterWithLimit appeared in JIRA
                # 3.13.4. Before that we had to use the unsafe
                # getIssuesFromFilter. Here we detect that we're talking
                # with an old JIRA and resort to the deprecated method
                # instead.
                croak $@ unless $@ =~ /No such operation/;
                $iter->{issues} = $self->getIssuesFromFilter($iter->{id});
                $iter->{id}     = undef;
            }
            elsif (@$issues) {
                $iter->{offset} += @$issues;
                $iter->{issues}  =  $issues;
            }
            else {
                $self->{iter} = undef;
                return;
            }
        }
        else {
            return;
        }
    }
    return shift @{$iter->{issues}};
}

=item B<progress_workflow_action_safely> ISSUE, ACTION, PARAMS

This is a safe and easier to use version of the
B<progressWorkflowAction> API method which is used to progress an
issue through a workflow's action while making edits to the fields
that are shown in the action screen. The API method is dangerous
because if you forget to provide new values to all the fields shown in
the screen, then the fields not provided will become undefined in the
issue. The problem has a pending issue on Atlassian's JIRA
L<http://jira.atlassian.com/browse/JRA-8717>.

This method plays it safe by making sure that all fields shown in the
screen that already have a value are given new (or the same) values so
that they don't get undefined. It calls the B<getFieldsForAction> API
method to grok all fields that are shown in the screen. If there is
any field not set in the ACTION_PARAMS then it calls B<getIssue> to
grok the missing fields current values. As a result it constructs the
necessary RemoteFieldAction array that must be passed to
progressWorkflowAction.

The method is also easier to use because its arguments are more
flexible:

=over 4

=item C<ISSUE> can be either an issue key or a RemoteIssue object
returned by a previous call to, e.g., C<getIssue>.

=item C<ACTION> can be either an action I<id> or an action I<name>.

=item C<PARAMS> must be a hash mapping field names to field
values. This hash accepts the same shortcuts as the argument to
B<create_issue>.

=back

For example, instead of using this:

  my $action_id = somehow_grok_the_id_of('close');
  $jira->progressWorkflowAction('PRJ-5', $action_id, [
    RemoteFieldValue->new(2, 'new value'),
    ..., # all fields must be specified here
  ]);

And risking to forget to pass some field you can do this:

  $jira->progress_workflow_action_safely('PRJ-5', 'close', {2 => 'new value'});

=cut

sub progress_workflow_action_safely {
    my ($self, $key, $action, $params) = @_;
    my $issue;
    if (ref $key) {
        $issue = $key;
        $key   = $issue->{key};
    }
    my ($project) = (split /-/, $key)[0];
    $params = {} unless defined $params;
    ref $params and ref $params eq 'HASH'
        or croak "progress_workflow_action_safely's third arg must be a HASH-ref\n";

    # Grok the action id if it's not a number
    if ($action =~ /\D/) {
        my @available_actions = @{$self->getAvailableActions($key)};
        my @named_actions     = grep {$action eq $_->{name}} @available_actions;
        if (@named_actions) {
            $action = $named_actions[0]->{id};
        }
        else {
            croak "Unavailable action ($action).\n";
        }
    }

    # Make sure $params contains all the fields that are present in
    # the action screen.
    my @fields = @{$self->getFieldsForAction($key, $action)};
    foreach my $id (map {$_->{id}} @fields) {
        # This is due to a bug in JIRA
        # http://jira.atlassian.com/browse/JRA-12300
        $id = 'affectsVersions' if $id eq 'versions';

        next if exists $params->{$id};

        $issue = $self->getIssue($key) unless defined $issue;
        if (exists $issue->{$id}) {
            $params->{$id} = $issue->{$id} if defined $issue->{$id};
        }
        else {
            foreach my $cf (@{$issue->{customFieldValues}}) {
                if ($cf->{customfieldId} eq $id) {
                    $params->{$id} = $cf->{values};
                    last;
                }
            }
            # NOTE: It's not a problem if we can't find a missing
            # parameter in the issue. It will simply stay undefined.
        }
    }

    # Convert some fields' values
    foreach my $field (grep {exists $_converters{$_}} keys %$params) {
	$_converters{$field}->($self, $params, $field, $project);
    }

    # Convert RemoteComponent objects into component ids
    if (my $comps = $params->{components}) {
        $_ = $_->{id} foreach @$comps;
    }

    # Convert RemoteVersion objects into version ids
    for my $field (qw/fixVersions affectsVersions/) {
        if (my $versions = $params->{$field}) {
            $_ = $_->{id} foreach @$versions;
        }
    }
    # Due to a bug in JIRA
    # (http://jira.atlassian.com/browse/JRA-12300) we have to
    # substitute 'versions' for the 'affectsVersions' key
    if (my $versions = delete $params->{affectsVersions}) {
        $params->{versions} = $versions;
    }

    # Expand the custom_fields hash into the custom fields themselves.
    if (my $custom_fields = delete $params->{custom_fields}) {
        while (my ($id, $values) = each %$custom_fields) {
            $params->{$id} = $values;
        }
    }

    return $self->progressWorkflowAction($key, $action, $params);
}

=item B<get_issue_custom_field_values> ISSUE, NAME_OR_IDs

This method receives a RemoteField object and a list of names or ids
of custom fields. It returns a list of references to the ARRAYs
containing the values of the ISSUE's custom fields denoted by their
NAME_OR_IDs. Returns undef for custom fields not set on the issue.

In scalar context it returns a reference to the list.

=cut

sub get_issue_custom_field_values {
    my ($self, $issue, @cfs) = @_;
    my @values;
    my $cfs;
  CUSTOM_FIELD:
    foreach my $cf (@cfs) {
        unless ($cf =~ /^customfield_\d+$/) {
            $cfs = $self->get_custom_fields() unless defined $cfs;
            croak "Can't find custom field named '$cf'.\n"
                unless exists $cfs->{$cf};
            $cf = $cfs->{$cf}{id};
        }
        foreach my $rcfv (@{$issue->{customFieldValues}}) {
            if ($rcfv->{customfieldId} eq $cf) {
                push @values, $rcfv->{values};
                next CUSTOM_FIELD;
            }
        }
        push @values, undef;    # unset custom field
    }
    return wantarray ? @values : \@values;
}

=item B<attach_files_to_issue> ISSUE, FILES...

This method attaches one or more files to an issue. The ISSUE argument
may be an issue key or a B<RemoteIssue> object. The attachments may be
specified in two ways:

=over 4

=item STRING

A string denotes a filename to be open and read. In this case, the
attachment name is the file's basename.

=item HASHREF

When you want to specify a different name to the attachment or when
you already have an IO object (a GLOB, a IO::File, or a FileHandle)
you must pass them as values of a hash. The keys of the hash are taken
as the attachment name. You can specify more than one attachment in
each hash.

=back

The method retuns the value returned by the
B<addBase64EncodedAttachmentsToIssue> API method.

In the example below, we attach three files to the issue TST-1. The
first is called C<file1.txt> and its contents are read from
C</path/to/file1.txt>. The second is called C<text.txt> and its
contents are read from C</path/to/file2.txt>. the third is called
C<me.jpg> and its contents are read from the object refered to by
C<$fh>.

    $jira->attach_files_to_issue('TST-1',
                                 '/path/to/file1.txt',
                                 {
                                     'text.txt' => '/path/to/file2.txt',
                                     'me.jpg'   => $fh,
                                 },
    );

=cut

sub attach_files_to_issue {
    my ($self, $issue, @files) = @_;

    # First we process the @files specification. Filenames are pushed
    # in @filenames and @attachments will end up with IO objects from
    # which the file contents are going to be read later.

    my (@filenames, @attachments);

    for my $file (@files) {
	if (not ref $file) {
	    require File::Basename;
	    push @filenames, File::Basename::basename($file);
	    open my $fh, '<:raw', $file
		or croak "Can't open $file: $!\n";
	    push @attachments, $fh;
	}
	elsif (ref $file eq 'HASH') {
	    while (my ($name, $contents) = each %$file) {
		push @filenames, $name;
		if (not ref $contents) {
		    open my $fh, '<:raw', $contents
			or croak "Can't open $contents: $!\n";
		    push @attachments, $fh;
		} elsif (ref($contents) =~ /^(?:GLOB|IO::File|FileHandle)$/) {
		    push @attachments, $contents;
		} else {
		    croak "Invalid content specification for file $name.\n";
		}
	    }
	}
	else {
	    croak "Files must be specified by STRINGs or HASHes, not by " . ref($file) . "s\n";
	}
    }

    # Now we have to read all file contents and encode them to Base64.

    require MIME::Base64;
    for my $i (0 .. $#attachments) {
	my $fh = $attachments[$i];
	my $attachment = '';
	my $chars_read;
	while ($chars_read = read $fh, my $buf, 57*72) {
	    $attachment .= MIME::Base64::encode_base64($buf);
	}
	defined $chars_read
	    or croak "Error reading '$filenames[$i]': $!\n";
	length $attachment
	    or croak "Can't attach empty file '$filenames[$i]'\n";
	$attachments[$i] = $attachment;
    }

    return $self->addBase64EncodedAttachmentsToIssue($issue, \@filenames, \@attachments);
}

=item B<attach_strings_to_issue> ISSUE, HASHREF

This method attaches one or more strings to an issue. The ISSUE
argument may be an issue key or a B<RemoteIssue> object. The
attachments are specified by a HASHREF in which the keys denote the
file names and the values their contents.

The method retuns the value returned by the
B<addBase64EncodedAttachmentsToIssue> API method.

=cut

sub attach_strings_to_issue {
    my ($self, $issue, $hash) = @_;

    require MIME::Base64;

    my (@filenames, @attachments);

    while (my ($filename, $contents) = each %$hash) {
	push @filenames,   $filename;
	push @attachments, MIME::Base64::encode_base64($contents);
    }

    return $self->addBase64EncodedAttachmentsToIssue($issue, \@filenames, \@attachments);
}

=back

=head1 OTHER CONSTRUCTORS

The JIRA SOAP API uses several types of objects (i.e., classes) for
which the Perl SOAP interface does not provide the necessary
constructors. This module implements some of them.

=over 4

=item B<RemoteFieldValue-E<gt>new> ID, VALUES

The RemoteFieldValue object represents the value of a field of an
issue. It needs two arguments:

=over

=item ID

The field name, which must be a valid key for the ISSUE hash.

=item VALUES

A scalar or an array of scalars.

=back

=cut

package RemoteFieldValue;

sub new {
    my ($class, $id, $values) = @_;

    # This is due to a bug in JIRA
    # http://jira.atlassian.com/browse/JRA-12300
    $id = 'versions' if $id eq 'affectsVersions';

    $values = [$values] unless ref $values;
    return bless({id => $id, values => $values}, $class);
}

=item B<RemoteCustomFieldValue-E<gt>new> ID, VALUES

The RemoteCustomFieldValue object represents the value of a
custom_field of an issue. It needs two arguments:

=over

=item ID

The field name, which must be a valid custom_field key.

=item VALUES

A scalar or an array of scalars.

=back

=cut

package RemoteCustomFieldValue;

sub new {
    my ($class, $id, $values) = @_;

    $values = [$values] unless ref $values;
    return bless({customfieldId => $id, key => undef, values => $values} => $class);
}

=item B<RemoteComponent-E<gt>new> ID, NAME

=cut

package RemoteComponent;

sub new {
    my ($class, $id, $name) = @_;
    my $o = bless({id => $id}, $class);
    $o->{name} = $name if $name;
    return $o;
}

=item B<RemoteVersion-E<gt>new> ID, NAME

=cut

package RemoteVersion;

sub new {
    my ($class, $id, $name) = @_;
    my $o = bless({id => $id}, $class);
    $o->{name} = $name if $name;
    return $o;
}

=back

=cut

package JIRA::Client;

# Almost all of the JIRA API parameters are strings. The %typeof hash
# specifies the exceptions. It maps a method name to a hash mapping a
# parameter position to its type. (The parameter position is
# zero-based, after the authentication token.

my %typeof = (
    addActorsToProjectRole                   => {1 => \&_cast_remote_project_role},
    addAttachmentsToIssue              	     => \&_cast_attachments,
    addBase64EncodedAttachmentsToIssue 	     => \&_cast_base64encodedattachments,
    addComment                         	     => {0 => \&_cast_issue_key, 1 => \&_cast_remote_comment},
    addDefaultActorsToProjectRole            => {1 => \&_cast_remote_project_role},
    # addPermissionTo
    # addUserToGroup
    # addVersion
    addWorklogAndAutoAdjustRemainingEstimate => {0 => \&_cast_issue_key},
    addWorklogAndRetainRemainingEstimate     => {0 => \&_cast_issue_key},
    addWorklogWithNewRemainingEstimate       => {0 => \&_cast_issue_key},
    archiveVersion                     	     => {2 => 'boolean'},
    # createGroup
    # createIssue
    createIssueWithParent                    => {1 => \&_cast_issue_key},
    createIssueWithParentWithSecurityLevel   => {1 => \&_cast_issue_key, 2 => 'long'},
    createIssueWithSecurityLevel       	     => {1 => 'long'},
    # createPermissionScheme
    # createProject
    # createProjectFromObject
    createProjectRole                        => {0 => \&_cast_remote_project_role},
    # createUser
    # deleteGroup
    deleteIssue                 	     => {0 => \&_cast_issue_key},
    # deletePermissionFrom
    # deletePermissionScheme
    # deleteProject
    deleteProjectAvatar                	     => {0 => 'long'},
    deleteProjectRole                  	     => {0 => \&_cast_remote_project_role, 1 => 'boolean'},
    # deleteUser
    # deleteWorklogAndAutoAdjustRemainingEstimate
    # deleteWorklogAndRetainRemainingEstimate
    # deleteWorklogWithNewRemainingEstimate
    # editComment
    # getAllPermissions
    getAssociatedNotificationSchemes         => {0 => \&_cast_remote_project_role},
    getAssociatedPermissionSchemes           => {0 => \&_cast_remote_project_role},
    getAttachmentsFromIssue           	     => {0 => \&_cast_issue_key},
    getAvailableActions           	     => {0 => \&_cast_issue_key},
    getComment                         	     => {0 => 'long'},
    getComments                        	     => {0 => \&_cast_issue_key},
    # getComponents
    # getConfiguration
    # getCustomFields
    getDefaultRoleActors                     => {0 => \&_cast_remote_project_role},
    # getFavouriteFilters
    getFieldsForAction                 	     => {0 => \&_cast_issue_key},
    getFieldsForCreate                       => {1 => 'long'},
    getFieldsForEdit                 	     => {0 => \&_cast_issue_key},
    # getGroup
    getIssue	                 	     => {0 => \&_cast_issue_key},
    # getIssueById
    getIssueCountForFilter             	     => {0 => \&_cast_filter_name_to_id},
    getIssuesFromFilter                	     => {0 => \&_cast_filter_name_to_id},
    getIssuesFromFilterWithLimit       	     => {0 => \&_cast_filter_name_to_id, 1 => 'int', 2 => 'int'},
    getIssuesFromJqlSearch             	     => {1 => 'int'},
    # getIssuesFromTextSearch
    getIssuesFromTextSearchWithLimit   	     => {1 => 'int', 2 => 'int'},
    getIssuesFromTextSearchWithProject 	     => {2 => 'int'},
    # getIssueTypes
    # getIssueTypesForProject
    # getNotificationSchemes
    # getPermissionSchemes
    # getPriorities
    # getProjectAvatar
    getProjectAvatars                  	     => {1 => 'boolean'},
    getProjectById                     	     => {0 => 'long'},
    # getProjectByKey
    getProjectRole                     	     => {0 => 'long'},
    getProjectRoleActors               	     => {0 => \&_cast_remote_project_role},
    # getProjectRoles
    # getProjectsNoSchemes
    getProjectWithSchemesById          	     => {0 => 'long'},
    getResolutionDateById              	     => {0 => 'long'},
    getResolutionDateByKey             	     => {0 => \&_cast_issue_key},
    # getResolutions
    # getSavedFilters
    getSecurityLevel             	     => {0 => \&_cast_issue_key},
    # getSecurityLevels
    # getSecuritySchemes
    # getServerInfo
    # getStatuses
    # getSubTaskIssueTypes
    # getSubTaskIssueTypesForProject
    # getUser
    # getVersions
    getWorklogs		             	     => {0 => \&_cast_issue_key},
    hasPermissionToCreateWorklog       	     => {0 => \&_cast_issue_key},
    # hasPermissionToDeleteWorklog
    # hasPermissionToEditComment
    # hasPermissionToUpdateWorklog
    # isProjectRoleNameUnique
    # login ##NOT USED##
    # logout ##NOT USED##
    progressWorkflowAction             	     => {0 => \&_cast_issue_key, 2 => \&_cast_remote_field_values},
    # refreshCustomFields
    # releaseVersion
    removeActorsFromProjectRole              => {1 => \&_cast_remote_project_role},
    # removeAllRoleActorsByNameAndType
    # removeAllRoleActorsByProject
    removeDefaultActorsFromProjectRole       => {1 => \&_cast_remote_project_role},
    # removeUserFromGroup
    # setNewProjectAvatar
    setProjectAvatar                   	     => {1 => 'long'},
    # setUserPassword
    # updateGroup
    updateIssue                        	     => {0 => \&_cast_issue_key, 1 => \&_cast_remote_field_values},
    # updateProject
    updateProjectRole                        => {0 => \&_cast_remote_project_role},
    # updateUser
    # updateWorklogAndAutoAdjustRemainingEstimate
    # updateWorklogAndRetainRemainingEstimate
    # updateWorklogWithNewRemainingEstimate
);

sub _cast_issue_key {
    my ($self, $issue) = @_;
    return ref $issue ? $issue->{key} : $issue;
}

sub _cast_remote_comment {
    my ($self, $arg) = @_;
    return ref $arg ? $arg : bless({body => $arg} => 'RemoteComment');
}

sub _cast_filter_name_to_id {
    my ($self, $arg) = @_;
    ref $arg and croak "Filter arg must be a scalar, not a ", ref($arg), "\n";
    return $arg unless $arg =~ /\D/;
    my $filters = $self->get_favourite_filters();
    exists $filters->{$arg} or croak "Unknown filter: $arg\n";
    return $filters->{$arg}{id};
}

sub _cast_remote_field_values {
    my ($self, $arg) = @_;
    if (ref $arg && ref $arg eq 'HASH') {
	return [map {RemoteFieldValue->new($_, $arg->{$_})} keys %$arg];
    }
    return $arg;
}

sub _cast_remote_project_role {
    my ($self, $arg) = @_;
    if (ref $arg && ref $arg eq 'RemoteProjectRole' && exists $arg->{id} && ! ref $arg->{id}) {
	$arg->{id} = SOAP::Data->type(long => $arg->{id});
    }
    return $arg;
}

sub _cast_attachments {
    my ($self, $method, $args) = @_;
    # The addAttachmentsToIssue method is deprecated and requires too
    # much overhead to pass the file contents over the wire. Here we
    # convert the arguments to call the newer
    # addBase64EncodedAttachmentsToIssue method instead.
    require MIME::Base64;
    for my $content (@{$args->[2]}) {
	$content = MIME::Base64::encode_base64($content);
    }
    $$method = 'addBase64EncodedAttachmentsToIssue';
    _cast_base64encodedattachments($self, $method, $args);
    return;
}

sub _cast_base64encodedattachments {
    my ($self, $method, $args) = @_;
    $args->[0] = _cast_issue_key($self, $args->[0]);
    # We have to set the names of the arrays and of its elements
    # because the default naming isn't properly understood by JIRA.
    for my $i (1 .. 2) {
	$args->[$i] = SOAP::Data->name(
	    "array$i",
	    [map {SOAP::Data->name("elem$i", $_)} @{$args->[$i]}],
	);
    }
    return;
}

# All methods follow the same call convention, which makes it easy to
# implement them all with an AUTOLOAD.

our $AUTOLOAD;
sub AUTOLOAD {
    my ($self, @args) = @_;
    (my $method = $AUTOLOAD) =~ s/.*:://;

    # Perform any non-default type coersion
    if (my $typeof = $typeof{$method}) {
	if (ref $typeof eq 'HASH') {
	    while (my ($i, $type) = each %$typeof) {
		if (ref $type) {
		    ref $type eq 'CODE'
			or croak "Invalid coersion spec to (", ref($type), ").\n";
		    $args[$i] = $type->($self, $args[$i]);
		} elsif (! ref $args[$i]) {
		    $args[$i] = SOAP::Data->type($type => $args[$i]);
		} elsif (ref $args[$i] eq 'ARRAY') {
		    foreach (@{$args[$i]}) {
			$_ = SOAP::Data->type($type => $_);
		    }
		} elsif (ref $args[$i] eq 'HASH') {
		    foreach (values %{$args[$i]}) {
			$_ = SOAP::Data->type($type => $_);
		    }
		} else {
		    croak "Can't coerse argument $i of method $AUTOLOAD.\n";
		}
	    }
	}
	elsif (ref $typeof eq 'CODE') {
	    $typeof->($self, \$method, \@args);
	}
    }

    my $call = $self->{soap}->call($method, $self->{auth}, @args);
    croak $call->faultcode(), ', ', $call->faultstring()
        if defined $call->fault();
    return $call->result();
}

=head1 EXAMPLES

Please, see the examples under the C<examples> directory in the module
distribution.

=head1 AUTHOR

Gustavo Chaves, C<< <gnustavo@cpan.org> >>

=head1 BUGS

Please report any bugs or feature requests to C<bug-jira-client at
rt.cpan.org>, or through the web interface at
L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=JIRA-Client>.  I will
be notified, and then you'll automatically be notified of progress on
your bug as I make changes.

=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc JIRA::Client

You can also look for information at:

=over 4

=item * RT: CPAN's request tracker

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=JIRA-Client>

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/JIRA-Client>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/JIRA-Client>

=item * Search CPAN

L<http://search.cpan.org/dist/JIRA-Client>

=back

=head1 COPYRIGHT & LICENSE

Copyright 2009-2011 CPqD, all rights reserved.

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

1; # End of JIRA::Client
