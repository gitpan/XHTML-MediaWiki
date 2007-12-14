use Test::More tests => 2;

use Test::XML;

use_ok('XHTML::MediaWiki');

my $mediawiki = XHTML::MediaWiki->new();

my ($xhtml, $cmp);

$xhtml = $mediawiki->format(<<EOT);
<pre>
This is pre nowiki text [[asdf]]
</pre>
 This is pre text [[asdf]]
EOT

$xhtml =~ s|<p><br/></p>||g;
$xhtml = '<div>' . $xhtml . "</div>";

$cmp = <<EOP;
<div>
<pre>
This is pre nowiki text [[asdf]]
</pre>
<pre>
This is pre text <a href="asdf">asdf</a>
</pre>
</div>
EOP

is_xml($xhtml, $cmp, 'pre');

