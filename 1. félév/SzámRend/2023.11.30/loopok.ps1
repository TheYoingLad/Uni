for ($i=1; $i -lt 6; $i++){
    $i
}

foreach ($item in @(1,2,3)){
    $item
}

(0..6) | ForEach-Object {$_}

$foo = 5

while ($foo -ge 0){
    $foo
    $foo--
}