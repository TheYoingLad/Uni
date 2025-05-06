param([Parameter(Mandatory=$true)] $file, [Parameter(Mandatory=$true)] $readme)

$readme = Get-Content $readme -ErrorAction Stop
$users = Get-Content $file -ErrorAction Stop

$users | Where-Object {($_ -match "^[0-9a-zA-Z]{6}$") -and -not (Test-Path $_)  } | ForEach-Object {
  New-Item -Path $_ -ItemType Directory
  $PersonalizedReadme = $readme -replace "<neptun>", $_
  $PersonalizedReadme | Out-File "$_//README.md" 
}
