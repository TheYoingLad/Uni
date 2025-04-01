param([Parameter(Mandatory=$true)] [String]$file)

if(!(Test-Path $file)) {throw "a fájl nem létezik!"}

foreach($line in Get-Content $file){
    if([int]$line % 15 -eq 0) {"fizzbuzz" >> 3out.txt}
    elseif([int]$line % 3 -eq 0) {"fizz" >> 3out.txt}
    elseif([int]$line % 5 -eq 0) {"buzz" >> 3out.txt}
}