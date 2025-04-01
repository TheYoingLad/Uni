param([Parameter(Mandatory=$true)] [string]$path)

if (-not (Test-Path $path)) {
  Write-Error "The provided path does not exist" -ErrorAction Stop
}

foreach ($line in Get-Content $path) {
  $line -replace '^(.*)\s+(.*)$', '$2 $1'
}
