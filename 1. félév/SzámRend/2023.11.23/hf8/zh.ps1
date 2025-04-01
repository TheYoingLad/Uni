if ($args.Count -ne 4){
    Write-Error "4 paramétert kell megadni!"
    exit
}
foreach($n in $args){
    if (!($n -match "^[+-]?((0$)|([1-9]\d*$))")){
        Write-Error "az egyik paraméter nem egész szám"
        exit
    }
}
$args[0]+$args[1]-$args[2]*$args[3]