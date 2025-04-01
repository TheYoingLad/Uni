[int]$sum = 0;
for ($i = 0; $i -lt $ARGS.Length; $i++) {
  [int]$n = $ARGS[$i]
  $sum += $n
}

$sum
