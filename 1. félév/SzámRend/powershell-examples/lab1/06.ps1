param([Parameter(Mandatory=$true)] [int]$n)

if ($n -le 1) {
   return $false
}

$i = 2
while ($i -le [math]::sqrt($n)) {
  if ($n % $i -eq 0) {
    return $false
  }

  $i++
}

return $true
