param([Parameter(Mandatory=$true)] $Path, [Parameter(Mandatory=$true)] [uint32]$MaxDepth)

function DirSize($dir, $CurrentDepth) {
  Write-Host $CurrentDepth
  if($CurrentDepth -gt $MaxDepth) {
    return 0
  }
  
  $size = 0
  Get-ChildItem $dir -File | ForEach-Object { $size += $_.Size }
  Get-ChildItem $dir -Directory | ForEach-Object {$size += DirSize $_.FullName ($CurrentDepth + 1) }

  return $size
}

DirSize $Path 0
