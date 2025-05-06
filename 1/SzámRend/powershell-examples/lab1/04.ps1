[double]$a = Read-Host -Prompt 'a'
[double]$b = Read-Host -Prompt 'b'
[double]$c = Read-Host -Prompt 'c'

$d = [math]::sqrt($b*$b - 4*$a*$c)
$x1 = (-$b + $d) / 2 * $a
$x2 = (-$b - $d) / 2 * $a

$x1,$x2
