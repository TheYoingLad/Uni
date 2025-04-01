param([Parameter(Mandatory=$true)] [int]$a, 
      [Parameter(Mandatory=$true)] [int]$b
)

#trap{
#    Write-Error "Nullával való osztás"
#    exit 42
#}
#$a / $b

try{
    ($a / $b).ToString()
    thor New-Object System.Management.Automation.ApplicationFailedException
} catch [System.Management.Automation.ApplicationFailedException]{
    Write-Error "AppFailed"
    exit 42
} catch [System.Management.Automation.RuntimeException]{
    Write-Host "Runtime"
} finally{
    Write-Host "halo"
}