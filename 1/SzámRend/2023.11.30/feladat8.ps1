foreach($line in Get-Content $args[0]){
    $line -replace '\(.*)\s+(.*)$', '$2 $1'
    $line
}