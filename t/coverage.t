use Test::More;

eval "use Test::Pod::Coverage";
plan skip_all => "Test::Pod::Coverage required for testing pod coverage" if $@;

plan tests => 1;

#pod_coverage_ok('Heavener::Question', 'Heavener::Question is covered');
TODO: {
    local $TODO = 'no pod';
    pod_coverage_ok('Heavener::Pg', 'Heavener::Pg is covered');
}

