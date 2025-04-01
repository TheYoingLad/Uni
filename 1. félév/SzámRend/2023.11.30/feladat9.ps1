foreach ($line in Get-Content $args[0]){
    $line = $line -replace '\s+kukac\s+', '@' -replace '\s+pont\s+', '.'
    $line
    $line >> "9ki.txt"
}