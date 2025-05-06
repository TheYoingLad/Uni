param([Parameter(Mandatory=$true)] [uint32]$days, [Parameter(Mandatory=$true)] [uint64]$MinSize)

Get-ChildItem -File | Where-Object {$_.LastAccessTime -gt (Get-Date).AddDays(-$days) -and $_.Size -gt $MinSize}
