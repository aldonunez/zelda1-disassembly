function JoinFiles( [string] $outputPath, [string[]] $inputPaths )
{
	$outFileName = split-path -leaf $outputPath
	$outDir = split-path -parent $outputPath
	$outDir = resolve-path $outDir

	$outputPath = join-path $outDir $outFileName
	$output = [IO.File]::Open( $outputPath, [IO.FileMode]::Create )

	try
	{
		foreach ( $inputPath in $inputPaths )
		{
			$inputPath = resolve-path $inputPath
			$input = [IO.File]::OpenRead( $inputPath )
			$input.CopyTo( $output )
			$input.Close()
		}
	}
	finally
	{
		$output.Close()
	}
}


function CompareFiles(
	[string] $leftPath,
	[string] $rightPath,
	[int] $bufferSize = 0x4000 )
{
	if ( $bufferSize -le 0 )
	{
		throw "Invalid buffer size"
	}

	$leftFile  = new-object IO.FileInfo (resolve-path $leftPath)
	$rightFile = new-object IO.FileInfo (resolve-path $rightPath)

	if ( !$leftFile.Exists -or !$rightFile.Exists -or ($leftFile.Length -ne $rightFile.Length) )
	{
		return $false
	}

	$leftBuf  = new-object byte[] $bufferSize
	$rightBuf = new-object byte[] $bufferSize

	try
	{
		$leftStream  = $leftFile.OpenRead()
		$rightStream = $rightFile.OpenRead()

		do
		{
			$bytesRead = $leftStream.Read( $leftBuf, 0, $bufferSize )
			[void] $rightStream.Read( $rightBuf, 0, $bufferSize )

			for ( $i = 0; $i -lt $bytesRead; $i++ )
			{
				if ( $leftBuf[$i] -ne $rightBuf[$i] )
				{
					return $false
				}
			}
		}
		while ( $bytesRead -eq $bufferSize )
	}
	finally
	{
		$leftStream.Close()
		$rightStream.Close()
	}

	return $true
}
