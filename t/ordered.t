use Test::More tests => 1;
use Test::XML;

use XHTML::MediaWiki;

my $mediawiki = XHTML::MediaWiki->new();

my ($xhtml, $cmp);

$xhtml = $mediawiki->format(<<EOT);
<div>
# one
## one.one
# two
# three
</div>
EOT

$cmp = <<EOP;
<div>
<ol>
<li>one<ol>
<li>one.one</li>
</ol>
</li>
<li>two</li>
<li>three</li>
</ol>
</div>
EOP

is_xml($xhtml, $cmp, 'simple ordered list');

