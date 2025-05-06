param([Parameter(Mandatory=$true)] [String]$file)

Get-Content $file -ErrorAction Stop | Where-Object {$_ -match "alma" -and $_.ToString().Length -gt 12} | Out-File 4out.txt -Append