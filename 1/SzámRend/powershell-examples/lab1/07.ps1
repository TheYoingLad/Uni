# Let's check arguments manually now

if ($ARGS.Length -ne 2) {
  Write-Error "You must provide 2 arguments" -ErrorAction Stop
}

$a = $ARGS[0]
$b = $ARGS[1]

# Lets check type manually as well
if (-not ($a -as [int]) -or -not ($b -as [int])) {
  Write-Error "Arguments must be integers" -ErrorAction Stop
}

while ($b -ne 0) {
  $temp = $b
  $b = $a % $b
  $a = $temp
}

$a
