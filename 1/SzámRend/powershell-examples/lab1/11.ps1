param([Parameter(Mandatory=$true)] [string]$path)

if (-not (Test-Path $path)) {
  Write-Error "The provided path does not exist" -ErrorAction Stop
}

foreach ($line in Get-Content $path) {
  $chars = $line.ToCharArray()
  [array]::Reverse($chars)
  -join $chars
}

