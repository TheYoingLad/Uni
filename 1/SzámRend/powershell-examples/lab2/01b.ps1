param([Parameter(Mandatory=$true)] [int]$a, [Parameter(Mandatory=$true)] [int]$b)

trap [System.Management.Automation.RuntimeException] {
  Write-Error "0-val való osztás" -ErrorAction Stop
}

$a / $b
