if (!(Test-Path ./aktualis.dat)){
    Get-Content indulo.dat | ForEach-Object -Begin{$n = 1} -Process{$_ + ":" + $n + ":S"  >> aktualis.dat; $n++}
}
if ($args.Length -lt 1){
    Write-Output "no parameter given!"
    Write-Output "-start <process name>: starts process with the given name"
    Write-Output "-stop <pid>: terminates process with the given pid, if it exists"
    Write-Output "-kill <pid>: kills process with the given pid, if it exists"
    Write-Output "-lista: lists running and terminated processes"
    exit
}
if ((Get-Content aktualis.dat | Measure-Object).Count -eq 0){
    $n = 1
} else{
    $n = [int](Get-Content aktualis.dat -Tail 1).Split(":")[1] + 1
    
}
switch ($args[0]){
    "-start"{
        if ($args.Length -ne 2){
            Write-Error "missing process name"
            exit
        }
        $p = $args[1]
        if ($p -contains ":"){
            Write-Error "invalid name, ':' is not allowed"
        }
        $p+":"+$n+":S" >> aktualis.dat
        $n++
        Write-Output "process $p started"
    }
    "-stop"{
        if ($args.Length -ne 2){
            Write-Error "invalid pid"
            exit
        }
        $p = $args[1]
        $line = Get-Content aktualis.dat | Where-Object {$_ -match "\:$p\:"}
        if ($line.Length -eq 0){
            Write-Error "no process with pid $p found"
            exit
        }
        if ($line -match '\:T$'){
            Write-Error "process with pid $p already terminated"
            exit
        }
        Get-Content aktualis.dat | ForEach-Object {
            if ($_ -ne $line){
                $_ >> tmp
            } else{             
                $_ -replace "\:S$", ":T" >> tmp
            }
        }
        Get-Content tmp > aktualis.dat
        Remove-Item tmp
        Write-Output "process with pid $p terminated"
    }
    "-kill"{
        if ($args.Length -ne 2){
            Write-Error "invalid pid"
            exit
        }
        $p = $args[1]
        $line = Get-Content aktualis.dat | Where-Object {$_ -match "\:$p\:"}
        if ($line.Length -eq 0){
            Write-Error "no process with pid $p found"
            exit
        }
        Get-Content aktualis.dat | Where-Object {$_ -ne $line} >> tmp
        Get-Content tmp > aktualis.dat
        Remove-Item tmp
        Write-Output "process with pid $p killed"
    }
    "-lista"{
        $p = (Get-Content aktualis.dat | Measure-Object -Line).Lines
        Write-Output "current number of processes: $p" 
        Write-Output "name : pid : status"
        Get-Content aktualis.dat
    }
    default{
        Write-Error "invalid paramter"
        exit
    }
}