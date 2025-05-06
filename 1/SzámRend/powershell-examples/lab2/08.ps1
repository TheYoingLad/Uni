param(
    [Parameter(Mandatory=$true)] [string]$file,
    
    [ValidateSet("Start", "Stop")]
    [string]$action = "Start"
)

foreach ($app in Get-Content $file) {
  if ($action -eq "Start") {
    Start-Process $app -ErrorAction Continue
  } elseif ($action -eq "Stop") {
    $proc = Get-Process -name $app -ErrorAction SilentlyContinue
    
    if ($proc) {
      Stop-Process -name $proc.Name -Force 
    } else {
      Write-Error "Process not running: $app"
    }
  }
}
