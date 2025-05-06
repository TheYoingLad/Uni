param([Parameter(Mandatory=$true)] [int]$n)

$f = 1
for($i = 2; $i -le $n; $i++){
    $f *= $i
}

$f