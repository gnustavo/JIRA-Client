use Test::More;

unless (-e 't/author.enabled') {
    plan skip_all => "Author-only tests";
    exit 0;
}

eval { require Test::Kwalitee; };

if ( $EVAL_ERROR ) {
    plan( skip_all => 'Test::Kwalitee required to evaluate code' );
   exit 0;
}

Test::Kwalitee->import();
