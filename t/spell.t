use Test::More;

eval 'use Test::Spelling 0.10'; plan( skip_all => 'Test::Spelling 0.10 required for this test' ) if $@;

add_stopwords(<DATA>);
all_pod_files_spelling_ok();
__DATA__
AnnoCPAN
API
Bowden
cgi
cgi-bin
chromatic's
colorization
CPAN
csv
DBI
discrepencies
DocBook
emphasised
existance
formattings
gloria
Heavener
href
html
HTML
Hukins
independantly
Mediawiki
mundi
Paulett
PHP
recieves
RT
SGML
StudlyCapsStrings
subref
subrefs
TODO
txt
URI
Vandiver
Vilain
wiki
Wiki
writted
xhtml
xls
xml

monospaced
