param([Parameter(Mandatory=$true)] [String]$file, [Parameter(Mandatory=$true)] [String]$pattern)

foreach ($line in Get-Content $file) {
  if ($line -match $pattern) {
    $line | Out-File out.txt -Append 
  }
}
