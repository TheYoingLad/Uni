param([Parameter(Mandatory=$true)] $type)
$regex = "*."+$type
Get-ChildItem -Recurse -Filter $regex