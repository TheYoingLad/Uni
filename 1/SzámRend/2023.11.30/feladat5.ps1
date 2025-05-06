param([Parameter(Mandatory=$true)] [int]$start, [Parameter(Mandatory=$true)] [int]$end)

function isPrime([int]$n){
    if ($n -le 1){
        return $false
    }
    for ($i = 2; $i -le [math]::Sqrt($n); $i++){
        if ($n % $i -eq 0){
            return $false
        }
    }
    return $true
}

($start..$end) | ? {isPrime($_)}