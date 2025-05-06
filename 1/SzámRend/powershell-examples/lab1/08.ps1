param([Parameter(Mandatory=$true)] [int]$start, [Parameter(Mandatory=$true)] [int]$end)

if ($end -le $start) {
  Write-Error "Start must be less then End" -ErrorAction Stop
}

function IsPrime([int]$n) {
   if ($n -le 1) {
        return $false
    }
    $i = 2
    while ($i -le [math]::sqrt($n)) {
        if ($n % $i -eq 0) {
            return $false
        }
        $i++
    }
    return $true
}

($start..$end) | Where-Object { IsPrime($_) }


