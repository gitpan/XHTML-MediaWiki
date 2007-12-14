use Test::More tests => 2;

use_ok('XHTML::MediaWiki');

my $mediawiki = XHTML::MediaWiki->new();

my $xhtml;

$xhtml = $mediawiki->format(<<EOT);
{{template}}
{{
template
}}
<div>
{{template
}}
</div>
{{
template}}
{{{ template }}}
<div>
{{
This
is 
a
template
}}
EOT

print $xhtml;
ok($xhtml, "headers");

