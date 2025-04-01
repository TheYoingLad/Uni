param([Parameter(Mandatory=$true)] [String[]]$extensions)

$map = @{}

foreach ($ext in $extensions) {
  $map[$ext] = @()
}

$map["egyéb"] = @()

foreach ($file in Get-ChildItem -File) {
  if ($extensions -contains $file.Extension) {
    $map[$file.Extension] += $file.Name
  } else {
    $map["egyéb"] += $file.Name 
  }
}

foreach ($ext in $map.Keys) {
  $dir = $ext.TrimStart(".")
  New-Item $dir -ItemType Directory
  $files = $map[$ext]
  $files | ForEach-Object { Copy-Item $_ $dir }
}
