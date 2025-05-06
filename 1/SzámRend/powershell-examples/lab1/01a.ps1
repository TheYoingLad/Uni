if ($ARGS.Length -eq 0) {
  Write-Error "An argument must be provided" -ErrorAction Stop
}

# Throws error if argument cannot be converted to int
[int]$n = $ARGS[0]

$f = 1

for ($i = 1; $i -le $n; $i++) {
  $f *= $i
}

$f
