function Assemble( $srcPath, $objPath )
{
	write-host "Assembling $srcPath"
	.\ext\ca65 $srcPath -o $objPath

	$passed = $LastExitCode -eq 0
	$script:assemblyPassed = $script:assemblyPassed -and $passed

	if ( !$passed ) { write-host "" }
}

function CompareBinaryFiles( $left, $right )
{
	write-host "Comparing $left and $right"
	cmd /c fc /b $left $right 2> $null > $null
	return $LastExitCode -eq 0
}

if ( (test-path ext\ca65.exe, ext\ld65.exe, ext\Original.nes) -contains $false )
{
	throw "Missing external. Ensure ext directory contains: ca65.exe, ld65.exe, Original.nes"
}

mkdir obj -ErrorAction ignore
mkdir bin -ErrorAction ignore

$srcPaths = @()
$objPaths = @()

foreach ( $file in (dir src\*.asm) )
{
	$base = [IO.Path]::GetFileNameWithoutExtension( $file )

	$srcPaths += "src\$base.asm"
	$objPaths += "obj\$base.o"
}

$assemblyPassed = $true

foreach ( $i in 0..($srcPaths.length-1) )
{
	Assemble $srcPaths[$i] $objPaths[$i]
}

if ( !$assemblyPassed )
{
	exit
}

echo "Linking"
.\ext\ld65 -o bin\Z.bin -C src\Z.cfg $objPaths
if ( $LastExitCode -ne 0 ) { exit }

echo "Combining raw ROM with NES header"
$out = cmd /c copy /b OriginalNesHeader.bin+bin\Z.bin bin\Z.nes
if ( $LastExitCode -ne 0 ) { echo $out; exit }

if ( CompareBinaryFiles bin\Z.nes ext\Original.nes )
{
	echo "ROM image is OK"
}
else
{
	echo "ROM image mismatch"
}
