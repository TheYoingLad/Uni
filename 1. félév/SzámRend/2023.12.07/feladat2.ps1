param([Parameter(Mandatory=$true)] $file,
      [Parameter(Mandatory=$true)] [string]$pattern
)

foreach ($neptun in Get-Content $file){
    try {
        if ($neptun -match $pattern){
            throw "valami"
        }
    } catch {
        $neptun >> out.txt
    }
}