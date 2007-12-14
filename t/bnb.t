use Test::More;

use Test::XML tests => 1;

use XHTML::MediaWiki;

my $mediawiki = XHTML::MediaWiki->new();

my ($xhtml, $cmp);

$xhtml = $mediawiki->format(<<EOT);
<div>
= Books & Baskets =
== About us ==

We are a locally own business and involved in the local community. 

== Location ==

 169 Olive Street,
 Saranac Lake, New York 12983


Across from the Post office and behind the Masonic Hall.

== Hours ==

10:00 - 5:00 Tuesday through Saturday

Closed Sunday and Monday

== What we have ==

Books and baskets has used books and basket kits.  We also have:

* New books about the Tri-Lakes area and the Adirondack Region.
* Fine teas.
* used VHS videos
* used DVD videos
* used Books on tape.
</div>
EOT

$cmp = <<EOT;
<div>
<a name="Books_&amp;_Baskets"></a><h1>Books &amp; Baskets</h1>
<a name="About_us"></a><h2>About us</h2>
<p>We are a locally own business and involved in the local community.</p>
<a name="Location"></a><h2>Location</h2>
<pre>169 Olive Street,
Saranac Lake, New York 12983</pre>
<p>Across from the Post office and behind the Masonic Hall.
</p>
<a name="Hours"></a><h2>Hours</h2>
<p>10:00 - 5:00 Tuesday through Saturday</p>
<p>Closed Sunday and Monday</p>
<a name="What_we_have"></a><h2>What we have</h2><p>Books and baskets has used books and basket kits.  We also have:</p>
<ul>
<li>New books about the Tri-Lakes area and the Adirondack Region.</li>
<li>Fine teas.</li>
<li>used VHS videos</li>
<li>used DVD videos</li>
<li>used Books on tape.</li>
</ul>
</div>
EOT

is_xml($xhtml, $cmp, "books and baskets");

