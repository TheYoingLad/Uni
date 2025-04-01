param([Parameter(Mandatory=$true)] [string]$path)

if ( -not (Test-Path $path) ) {
  Write-Error "File doesn't exist" -ErrorAction Stop
}

$i = 1
foreach ($line in Get-Content $path) {
  if ($i % 2 -eq 0) {
    $line | Out-File "paros.txt" -Append
  } else {
    $line | Out-File "paratlan.txt" -Append
  }
  $i++
}
