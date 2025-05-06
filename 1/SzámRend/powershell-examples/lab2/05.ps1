param([Parameter(Mandatory=$true)] $file)

$rng = New-Object System.Random

foreach ($user in Get-Content $file) {
  "{0}: {1:x}" -f $user, $rng.next(3452,65000) | Out-File out.txt -Append 
}
