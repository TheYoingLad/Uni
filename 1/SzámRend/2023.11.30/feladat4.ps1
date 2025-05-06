param([Parameter(Mandatory=$true)] $path)

if(!(Test-Path $path)){
    throw "File does not exist"
}

$i = 0
foreach ($sor in Get-Content $path){
    if($i % 2 -eq 0){
        $sor | Out-File "4ps.txt" -Append
    } else{
        $sor >> "4ptln.txt"
    }
    $i++
}