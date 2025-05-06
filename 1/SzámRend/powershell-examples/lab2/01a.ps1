param([Parameter(Mandatory=$true)] [int]$a, [Parameter(Mandatory=$true)] [int]$b)

try {
  $a / $b
} catch {
   Write-Error "0-val való osztás" -ErrorAction Stop
}
