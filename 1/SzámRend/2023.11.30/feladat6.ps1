$lines = Get-Content $args[0]
$lines[-1..-$lines.Length]
[array]::Reverse($lines)
$lines