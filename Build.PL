use strict;
use warnings;
use Module::Build;

my $builder = Module::Build->new(
    module_name   => 'XHTML::MediaWiki',
    dist_abstract => 'Convert MediaWiki text to xhtml',
    license       => 'perl',
    requires => {
    	'perl'		=> '5.6.0',
 	'HTML::Parser'  => 0,
	'Params::Validate' => '0.85',
	'URI::Escape' => 0,
    },
    build_requires => {
	'Test::XML' => 0,
	'Test::XML::Order' => 0,
    },
    recommends => {
        'Test::Pod' => 0,
        'Test::Pod::Coverage' => 0,
    },
    create_makefile_pl => 'traditional',
    create_readme => 1,
    sign          => 1,
);

$builder->create_build_script;

