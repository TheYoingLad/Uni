foreach($line in Get-Content $args[0]){
    $chars = $line.ToCharArray()
    [array]::Reverse($chars)
    -join $chars
}