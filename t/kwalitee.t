use Test::More;

plan skip_all => "Author-only tests" unless -e 't/author.enabled';

eval { require Test::Kwalitee; Test::Kwalitee->import() };

plan skip_all => 'Test::Kwalitee not installed; skipping' if $@;


