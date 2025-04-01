param([Parameter(Mandatory=$true)] [double]$min,
      [Parameter(Mandatory=$true)] [double]$max,
      [Parameter(Mandatory=$true)] [double]$t)

if($min -gt $max) {throw "min nagyon mint max"}
if($t -lt 0 -or $t -gt 1) {throw "t nem eleme [0;1]"}

$min + ($max - $min) * $t