package JIRA::Client;

use strict;
use warnings;
use Carp;
use SOAP::Lite;

=head1 NAME

JIRA::Client - An extended interface to JIRA's SOAP API.

=head1 VERSION

Version 0.05

=cut

our $VERSION = '0.05';

=head1 SYNOPSIS

  use JIRA::Client;

  my $jira = JIRA::Client->new('http://jira.example.com/jira', 'user', 'passwd');

  my $issue = $jira->getIssue('PRJ-123');

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

Moreover, it implements some other methods to make it easier to do
some common operations.

=head1 METHODS

With the exception of the API C<login> and C<logout> methods, which aren't needed, all other methods are available through the JIRA::Client object interface. You must call them with the same name as documented in the specification but you should not pass the C<token> argument, because it is supplied transparently by the JIRA::Client object.

The extra methods implemented by this module are described below.

=over 4

=item B<new> JIRAURL, USER, PASSWD

The JIRA::Client constructor needs three arguments. JIRAURL is JIRA's
base URL from which will be constructed it's WSDL descriptor as
C<$JIRAURL/rpc/soap/jirasoapservice-v2?wsdl>. USER and PASSWD are the
credentials that will be used to authenticate into JIRA.

=cut

sub _fault_details {
    my $r = shift;
    join(', ', $r->faultcode(), $r->faultstring());
}

sub new {
    my ($class, $base_url, $user, $pass) = @_;

    my $soap = SOAP::Lite->proxy("$base_url/rpc/soap/jirasoapservice-v2?wsdl");

    # Make all scalars be encoded as strings by default.
    %{$soap->typelookup()} = (default => [0, sub {1}, 'as_string']);

    my $auth = $soap->login($user, $pass);
    die _fault_details($auth), "\n"
	if defined $auth->fault();

    my $self = {
	soap  => $soap,
	auth  => scalar($auth->result()),
	iter  => undef,
	cache => {
	    custom_fields => undef, # {name => RemoteField}
	    priorities    => undef, # {name => RemotePriority}
	    components    => {}, # project_key => {name => RemoteComponent}
	    versions      => {}, # project_key => {name => RemoteVersion}
	},
    };

    bless $self, $class;
}

sub DESTROY {
    shift->logout();
}

=item B<set_filter_iterator> FILTER_NAME

This method sets up an interator for the filter identified by
FILTER_NAME. It must be called before calls to B<next_issue>.

=cut

sub set_filter_iterator {
    my ($self, $filter_name) = @_;

    my $filters = $self->getSavedFilters();
    foreach my $filter (@$filters) {
        if ($filter->{name} eq $filter_name) {
            $self->{iter} = {
		id     => $filter->{id},
		offset => 0,  # offset to be used in the next call to getIssuesFromFilterWithLimit
		issues => [], # issues returned by the last call to getIssuesFromFilterWithLimit
	    };
	    return;
        }
    }
    croak "Can't find filter '$filter_name'\n";
}

=item B<next_issue>

This method must be called after a call to
B<set_filter_iterator>. Each call returns a reference to the next
issue from the filter. When there are no more issues it returns undef.

=cut

sub next_issue {
    my ($self) = @_;
    defined $self->{iter}
	or croak "You must call setFilterIterator before calling nextIssue\n";
    my $iter = $self->{iter};
    if (@{$iter->{issues}} == 0) {
	my $issues = $self->getIssuesFromFilterWithLimit($iter->{id}, $iter->{offset}, 100);
	my $offset = $iter->{offset} + @{$iter->{issues}};
	if (@$issues) {
	    $iter->{offset} += @$issues;
	    $iter->{issues}  =  $issues;
	}
	else {
	    $self->{iter} = undef;
	    return undef;
	}
    }
    return shift @{$iter->{issues}};
}

=item B<get_custom_fields>

This method returns a hash mapping JIRA's custom field names to the
RemoteField representing them. It's useful since when you get a
RemoteIssue object from this API it doesn't contain the custom field's
names, but just their identifiers. From the RemoteField object you can
obtain the field's B<id>, which is useful when calling the
B<updateIssue> method.

This module method calls the getCustomFields API method the first time
and keeps the custom fields information in a cache.

=cut

sub get_custom_fields {
    my ($self) = @_;
    unless (defined $self->{cache}{custom_fields}) {
	my %custom_fields;
	my $cfs = $self->getCustomFields();
	foreach my $cf (@$cfs) {
	    $custom_fields{$cf->{name}} = $cf;
	}
	$self->{cache}{custom_fields} = \%custom_fields;
    }
    $self->{cache}{custom_fields};
}

=item B<set_custom_fields> HASHREF

This method passes a hash mapping JIRA's custom field names to the
RemoteField representing them to populate the custom field's
cache. This can be useful if you don't have administrative priviledges
to the JIRA instance, since only administrators can call the
B<getCustomFields> API method.

=cut

sub set_custom_fields {
    my ($self, $cfs) = @_;
    $self->{cache}{custom_fields} = $cfs;
}

=item B<get_priorities>

This method returns a hash mapping a project's priorities names to the
RemotePriority objects describing them.

=cut

sub get_priorities {
    my ($self) = @_;
    unless (defined $self->{cache}{priorities}) {
	my %priorities;
	my $prios = $self->getPriorities();
	foreach my $prio (@$prios) {
	    $priorities{$prio->{name}} = $prio;
	}
	$self->{cache}{priorities} = \%priorities;
    }
    $self->{cache}{priorities};
}

=item B<get_versions> PROJECT_KEY

This method returns a hash mapping a project's versions names to the
RemoteVersion objects describing them.

=cut

sub get_versions {
    my ($self, $project_key) = @_;
    my $cache = $self->{cache}{versions};
    unless (exists $cache->{$project_key}) {
	my %versions;
	my $versions = $self->getVersions($project_key);
	foreach my $version (@$versions) {
	    $versions{$version->{name}} = $version;
	}
	$cache->{$project_key} = \%versions;
    }
    $cache->{$project_key};
}

=item B<get_components> PROJECT_KEY

This method returns a hash mapping a project's components names to the
RemoteComponent objects describing them.

=cut

sub get_components {
    my ($self, $project_key) = @_;
    my $cache = $self->{cache}{components};
    unless (exists $cache->{$project_key}) {
	my %components;
	my $components = $self->getComponents($project_key);
	foreach my $component (@$components) {
	    $components{$component->{name}} = $component;
	}
	$cache->{$project_key} = \%components;
    }
    $cache->{$project_key};
}

=back

=cut

# Almost all of the JIRA API parameters are strings. The %typeof hash
# specifies the exceptions. It maps a method name to a hash mapping a
# parameter position to its type. (The parameter position is
# zero-based, after the authentication token.

my %typeof = (
    addAttachmentsToIssue              => {3 => 'base64Binary'},
    archiveVersion                     => {2 => 'boolean'},
    createIssueWithSecurityLevel       => {1 => 'long'},
    deleteProjectRole                  => {1 => 'boolean'},
    getComment                         => {0 => 'long'},
    getIssuesFromFilterWithLimit       => {1 => 'int', 2 => 'int'},
    getIssuesFromTextSearchWithLimit   => {1 => 'int', 2 => 'int'},
    getIssuesFromTextSearchWithProject => {2 => 'int'},
    getProjectById                     => {0 => 'long'},
    getProjectRole                     => {0 => 'long'},
    getProjectWithSchemesById          => {0 => 'long'},
);

# All methods follow the same call convention, which makes it easy to
# implement them all with an AUTOLOAD.

our $AUTOLOAD;
sub AUTOLOAD {
    my ($self, @args) = @_;
    (my $method = $AUTOLOAD) =~ s/.*:://;

    # Perform any non-default type coersion
    if (my $typeof = $typeof{$method}) {
	while (my ($i, $type) = each %$typeof) {
	    if (! ref $args[$i]) {
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
    die _fault_details($call), "\n"
	if defined $call->fault();
    $call->result();
}

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
