param([Parameter(Mandatory=$true)] [String[]]$files)

foreach ($file in $files) {
  Remove-Item $file -ErrorAction SilentlyContinue
}
