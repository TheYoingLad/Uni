param([Parameter(Mandatory=$true)] [string]$path)

if (-not (Test-Path $path)) {
  Write-Error "A path must be supplied" -ErrorAction Stop
}

foreach ($line in Get-Content $path) {
  $decoded = $line -replace '\s+kukac\s+', '@' -replace '\s+pont\s', '.'
  $decoded | Out-File "decoded.txt" -Append
  $decoded
}
