param([Parameter(Mandatory=$true)] [string]$path)

if (-not (Test-Path $path)) {
  Write-Error "The provided path does not exist" -ErrorAction Stop
}

$lines = Get-Content $path
$count = $lines.Length

$lines[-1..-$count]
