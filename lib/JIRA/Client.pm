package JIRA::Client;

use strict;
use warnings;
use Carp;
use SOAP::Lite;

=head1 NAME

JIRA::Client - An extended interface to JIRA's SOAP API.

=head1 VERSION

Version 0.21

=cut

our $VERSION = '0.21';

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
(This version is known to work with JIRA 4 but it was tested by the author only against JIRA 3.13.4 so far.)

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

=item B<new> JIRAURL, USER, PASSWD

The JIRA::Client constructor needs three arguments. JIRAURL is JIRA's
base URL from which will be constructed it's WSDL descriptor as
C<$JIRAURL/rpc/soap/jirasoapservice-v2?wsdl>. USER and PASSWD are the
credentials that will be used to authenticate into JIRA.

=cut

sub _fault_details {
    my $r = shift;
    return join(', ', $r->faultcode(), $r->faultstring());
}

sub new {
    my ($class, $base_url, $user, $pass) = @_;

    my $soap = SOAP::Lite->proxy("$base_url/rpc/soap/jirasoapservice-v2?wsdl");

    # Make all scalars be encoded as strings by default.
    %{$soap->typelookup()} = (default => [0, sub {1}, 'as_string']);

    my $auth = $soap->login($user, $pass);
    croak _fault_details($auth), "\n"
        if defined $auth->fault();

    my $self = {
        soap  => $soap,
        auth  => scalar($auth->result()),
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
    # down the SOAP connection cleanly, but it doesn't seems to hurt
    # to not call it.

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
        $hash->{resolution} = $resolutions->{$resolution}{name};
    }
    return;
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
	    qw/zero Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dez/[$month],
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
        unless ($id =~ /^customfield_\d+$/) {
            my $cfs = $self->get_custom_fields();
            croak "Can't find custom field named '$id'.\n"
                unless exists $cfs->{$id};
            $id = $cfs->{$id}{id};
        }
        $id2values{$id} = ref $values ? $values : [$values];
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

=item B<create_issue> HASH_REF

Creates a new issue given a hash containing the initial values for its
fields. The hash must specify at least the fields C<project>,
C<summary>, and C<type>.

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

Moreover, it accepts a 'magic' field called B<custom_fields> to make
it easy to set custom fields. It accepts a hash mapping each custom
field to its value. The custom field can be specified by its id (in
the format B<customfield_NNNNN>) or by its name, in which case the
method will try to convert it to its id. Note that to do that
conversion the user needs administrator rights.

=cut

sub create_issue
{
    my ($self, $hash) = @_;
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

    return $self->createIssue($hash);
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
    addAttachmentsToIssue              	     => {0 => \&_cast_issue_key, 3 => 'base64Binary'},
    addBase64EncodedAttachmentsToIssue 	     => {0 => \&_cast_issue_key},
    addComment                         	     => {0 => \&_cast_issue_key, 1 => \&_cast_remote_comment},
    addWorklogAndAutoAdjustRemainingEstimate => {0 => \&_cast_issue_key},
    addWorklogAndRetainRemainingEstimate     => {0 => \&_cast_issue_key},
    addWorklogWithNewRemainingEstimate       => {0 => \&_cast_issue_key},
    archiveVersion                     	     => {2 => 'boolean'},
    createIssueWithSecurityLevel       	     => {1 => 'long'},
    deleteIssue                 	     => {0 => \&_cast_issue_key},
    deleteProjectAvatar                	     => {0 => 'long'},
    deleteProjectRole                  	     => {1 => 'boolean'},
    getAttachmentsFromIssue           	     => {0 => \&_cast_issue_key},
    getAvailableActions           	     => {0 => \&_cast_issue_key},
    getComment                         	     => {0 => 'long'},
    getComments                        	     => {0 => \&_cast_issue_key},
    getFieldsForAction                 	     => {0 => \&_cast_issue_key},
    getFieldsForEdit                 	     => {0 => \&_cast_issue_key},
    getIssue	                 	     => {0 => \&_cast_issue_key},
    getIssueCountForFilter             	     => {0 => \&_cast_filter_name_to_id},
    getIssuesFromFilter                	     => {0 => \&_cast_filter_name_to_id},
    getIssuesFromFilterWithLimit       	     => {0 => \&_cast_filter_name_to_id, 1 => 'int', 2 => 'int'},
    getIssuesFromJqlSearch             	     => {1 => 'int'},
    getIssuesFromTextSearchWithLimit   	     => {1 => 'int', 2 => 'int'},
    getIssuesFromTextSearchWithProject 	     => {2 => 'int'},
    getProjectAvatars                  	     => {1 => 'boolean'},
    getProjectById                     	     => {0 => 'long'},
    getProjectRole                     	     => {0 => 'long'},
    getProjectWithSchemesById          	     => {0 => 'long'},
    getResolutionDateById              	     => {0 => 'long'},
    getResolutionDateByKey             	     => {0 => \&_cast_issue_key},
    getSecurityLevel             	     => {0 => \&_cast_issue_key},
    getWorklogs		             	     => {0 => \&_cast_issue_key},
    hasPermissionToCreateWorklog       	     => {0 => \&_cast_issue_key},
    progressWorkflowAction             	     => {0 => \&_cast_issue_key, 2 => \&_cast_remote_field_values},
    setProjectAvatar                   	     => {1 => 'long'},
    updateIssue                        	     => {0 => \&_cast_issue_key, 1 => \&_cast_remote_field_values},
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
	# Convert some fields' values
	foreach my $field (grep {exists $_converters{$_}} keys %$arg) {
	    $_converters{$field}->($self, $arg, $field);
	}
	return [map {RemoteFieldValue->new($_, $arg->{$_})} keys %$arg];
    }
    return $arg;
}

# All methods follow the same call convention, which makes it easy to
# implement them all with an AUTOLOAD.

our $AUTOLOAD;
sub AUTOLOAD {
    my ($self, @args) = @_;
    (my $method = $AUTOLOAD) =~ s/.*:://;

    # Perform any non-default type coersion
    if (my $typeof = $typeof{$method}) {
        while (my ($i, $type) = each %$typeof) {
            if (ref $type) {
		ref $type eq 'CODE'
		    or croak "Invalid coersion spec to (", ref($type), ").\n";
		$args[$i] = $type->($self, $args[$i]);
            }
            elsif (! ref $args[$i]) {
                $args[$i] = SOAP::Data->type($type => $args[$i]);
            }
            elsif (ref $args[$i] eq 'ARRAY') {
                foreach (@{$args[$i]}) {
                    $_ = SOAP::Data->type($type => $_);
                }
            }
            elsif (ref $args[$i] eq 'HASH') {
                foreach (values %{$args[$i]}) {
                    $_ = SOAP::Data->type($type => $_);
                }
            }
            else {
                croak "Can't coerse argument $i of method $AUTOLOAD.\n";
            }
        }
    }

    my $call = $self->{soap}->call($method, $self->{auth}, @args);
    croak _fault_details($call), "\n"
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

Copyright 2009 CPqD, all rights reserved.

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

1; # End of JIRA::Client
