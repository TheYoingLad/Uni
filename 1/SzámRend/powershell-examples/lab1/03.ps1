# We could use the same for-loop we did in 2., but lets try ForEach-Object here
$input | ForEach-Object -Begin { [int]$sum = 0 } -Process { $sum += [int]$_ } -End { $sum }

