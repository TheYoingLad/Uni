param([Parameter(Mandatory=$true)] [uint32]$days)

Get-ChildItem -File | Where-Object {$_.LastAccessTime -gt (Get-Date).AddDays(-$days)}
