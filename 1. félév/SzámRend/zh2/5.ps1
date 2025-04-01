param([Parameter(Mandatory=$true)] [uint64]$global:max)

[uint64]$global:mem = 0

function malloc([uint64] $size){
    if($size + $global:mem -gt $global:max) {throw "memória limit meghaladva"}
    $global:mem += $size
    Write-Host "${size} memória sikeresen lefoglalva"
    Write-Host "jelenlegi memoria: ${global:max} / ${global:mem}`n"
}

function free([uint64] $size){
    if($global:mem - $size -lt 0) {throw "memória túlszabadítva"}
    $global:mem -= $size
    Write-Host "${size} memória sikeresen felszabadítva"
    Write-Host "jelenlegi memoria: ${global:max} / ${global:mem}`n"
}

Write-Host "kezdeti memórialimit: ${global:max}`n"

try{
    malloc(20)
} catch{
    try{
        malloc(10)
    } catch{
        Write-Error "sikertelen memória foglalás"
    }
}
try{
    malloc(30)
} catch{
    try{
        malloc(15)
    } catch{
        Write-Error "sikertelen memória foglalás"
    }
}
try{
    malloc(40)
} catch{
    try{
        malloc(20)
    } catch{
        Write-Error "sikertelen memória foglalás"
    }
}
try{
    malloc(100)
} catch{
    try{
        malloc(50)
    } catch{
        Write-Error "sikertelen memória foglalás"
    }
}
try{
    free(1)
} catch {
    Write-Error "memória túlszabadítva"
}
try{
    free(10)
} catch {
    Write-Error "memória túlszabadítva"
}
try{
    free(100)
} catch {
    Write-Error "memória túlszabadítva"
}
try{
    free(100)
} catch {
    Write-Error "memória túlszabadítva"
}

Write-Host "végső memoria: ${global:max} / ${global:mem}"