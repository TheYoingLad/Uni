filter isPrime{
    if ($_ -le 1){
        return $false
    }
    for ($i = 2; $i -le [math]::Sqrt($_); $i++){
        if ($_ % $i -eq 0){
            return $false
        }
    }
    return $true
}

filter Sum{
    begin{
        $sum = 0
    }
    process{
        $sum += $_
    }
    end{
        $sum
    }
}

filter SumAdd3([int]$n){
    begin{
        $sum = 0
    }
    process{
        $sum += $_
    }
    end{
        $sum + $n
    }
}

filter SumSwitch([switch]$megse){
    begin{
        $sum = 0
    }
    process{
        $sum += $_
    }
    end{
        if (-not $megse){
            $sum
        } else{
            0
        }
    }
}


1,2,3 | isPrime
1,2,3 | Sum
1,2,3 | SumAdd3 3
1,2,3 | SumSwitch -megse