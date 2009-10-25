use Test::More;

unless (-e 't/author.enabled') {
    plan skip_all => "Author-only tests";
    exit 0;
}

require Test::Kwalitee;

if ($@) {
   plan skip_all => 'Test::Kwalitee not installed; skipping';
   exit 0;
}

Test::Kwalitee->import();
