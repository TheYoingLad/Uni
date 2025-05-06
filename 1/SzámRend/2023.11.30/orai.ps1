if ($args.Length -ne 1){
    Write-Error "Kell egy argumentum"
    exit
}

if (! ($args[0] -is [int])){
    throw "nem int!"
}
$args[0] + 2