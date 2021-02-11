param ( [switch] $NoVerify, [switch] $NoExtract )

# Escalate any statement-terminating error to a script-terminating one.
trap { break }

. (join-path $PSScriptRoot util.ps1)

function Assemble( $srcPath, $objPath )
{
	write-host "Assembling $srcPath"
	.\ext\ca65 $srcPath -o $objPath --bin-include-dir .\bin

	$passed = $LastExitCode -eq 0
	$script:assemblyPassed = $script:assemblyPassed -and $passed

	if ( !$passed ) { write-host "" }
}

function CompareBinaryFiles( $left, $right )
{
	write-host "Comparing $left and $right"
	return CompareFiles $left $right
}

function CheckRequirements()
{
	$message = "Missing external. Ensure ext directory contains: "

	get-command ext\ca65, ext\ld65 > $null 2>&1

	if ( !$? )
	{
		throw ($message + "ca65.exe, ld65.exe")
	}

	if ( !$NoVerify -and !(test-path ext\Original.nes) )
	{
		throw ($message + "Original.nes")
	}
}

function ExtractBins()
{
	$romPath = resolve-path .\ext\Original.nes
	$binXmlPath = resolve-path .\src\bins.xml
	$binRootPath = resolve-path .\bin

	$xml = new-object Xml
	$xml.Load( $binXmlPath )

	$image = [IO.File]::ReadAllBytes( $romPath )

	foreach ( $bin in $xml.Binaries.Binary )
	{
		$binPath = join-path $binRootPath $bin.FileName
		$binDir = split-path $binPath
		mkdir $binDir -ErrorAction ignore > $null

		$offset = [int] $bin.Offset + 16

		$buf = new-object byte[] $bin.Length
		[Array]::Copy( $image, $offset, $buf, 0, $bin.Length )

		[IO.File]::WriteAllBytes( $binPath, $buf )
	}
}


CheckRequirements

mkdir obj -ErrorAction ignore > $null
mkdir bin -ErrorAction ignore > $null

if ( !$NoExtract )
{
	ExtractBins
}

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
JoinFiles bin\Z.nes -in OriginalNesHeader.bin, bin\Z.bin

if ( !$NoVerify )
{
	if ( CompareBinaryFiles bin\Z.nes ext\Original.nes )
	{
		echo "ROM image is OK"
	}
	else
	{
		echo "ROM image mismatch"
	}
}
