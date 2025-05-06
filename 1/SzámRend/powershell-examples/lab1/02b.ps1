[int]$sum = 0
foreach ($n in $ARGS) {
  $sum += [int]$n
}

$sum
