param([Parameter(Mandatory=$true)] $path)
if(!(Test-Path $path)){
    throw "File does not exist"
}
$lines = Get-Content $path -Delimiter ","
$lines.Length