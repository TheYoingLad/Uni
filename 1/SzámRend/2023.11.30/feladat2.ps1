$sum = 0
for ($i = 0; $i -lt $args.Length; $i++){
    $sum += $args[$i]
}
$sum
$sum = 0

foreach ($item in $args){
    $sum += $item
}
$sum

$args | ForEach-Object -Begin {$sum = 0} -Process{$sum += $_} -End{$sum}