param([Parameter(Mandatory=$true)] [String]$file, [Parameter(Mandatory=$true)] [String]$pattern)

foreach ($line in Get-Content $file) {
  try {
    if ($line -match $pattern) {
      throw "valami" 
    }
  } catch {
    $line | Out-File out.txt -Append
  }
}
