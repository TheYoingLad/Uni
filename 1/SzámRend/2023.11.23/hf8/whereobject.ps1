param([Parameter(Mandatory=$true)] [double]$x)
Get-Process | Where-Object cpu -gt $x