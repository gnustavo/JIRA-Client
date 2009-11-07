use strict;
use warnings;
use File::Spec;
use Test::More;
use English qw(-no_match_vars);

unless (-e 't/author.enabled') {
    plan skip_all => "Author-only tests";
    exit 0;
}

eval { require Test::Perl::Critic; };

if ( $EVAL_ERROR ) {
    my $msg = 'Test::Perl::Critic required to criticise code';
    plan( skip_all => $msg );
}

Test::Perl::Critic->import( -verbose => 5 );
all_critic_ok();
