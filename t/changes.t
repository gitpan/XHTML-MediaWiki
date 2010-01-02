use Test::More;

eval {
    require Test::CheckChanges;
    Test::CheckChanges->import();
};
if ($@) {
    plan skip => "Need Test::CheckChanges";
}


ok_changes();

