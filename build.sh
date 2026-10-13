#!/usr/bin/env bash
# Port of build.ps1 for macOS/Linux.
# Usage: ./build.sh [--no-verify] [--no-extract]

set -eu -o pipefail

cd "$(dirname "$0")"

no_verify=0
no_extract=0

for arg in "$@"
do
	case "$arg" in
		--no-verify)  no_verify=1 ;;
		--no-extract) no_extract=1 ;;
		*) echo "Unknown option: $arg" >&2; exit 2 ;;
	esac
done

check_requirements()
{
	local message="Missing external. Ensure ext directory contains: "

	if [ ! -x ext/ca65 ] || [ ! -x ext/ld65 ]
	then
		echo "${message}ca65, ld65" >&2
		exit 1
	fi

	if [ $no_verify -eq 0 ] && [ ! -f ext/Original.nes ]
	then
		echo "${message}Original.nes" >&2
		exit 1
	fi
}

extract_bins()
{
	local line offset length file_name

	while IFS= read -r line
	do
		offset=$(sed -E "s/.*Offset='([0-9]+)'.*/\1/" <<< "$line")
		length=$(sed -E "s/.*Length='([0-9]+)'.*/\1/" <<< "$line")
		file_name=$(sed -E "s/.*FileName='([^']+)'.*/\1/" <<< "$line")

		mkdir -p "bin/$(dirname "$file_name")"

		# Skip the 16-byte iNES header. dd avoids the SIGPIPE that a
		# tail | head pipeline triggers under pipefail.
		dd if=ext/Original.nes of="bin/$file_name" bs=1 \
			skip=$((offset + 16)) count="$length" 2> /dev/null
	done < <(grep -E "<Binary " src/bins.xml | tr -d '\r')
}


check_requirements

mkdir -p obj bin

if [ $no_extract -eq 0 ]
then
	extract_bins
fi

assembly_passed=1
obj_paths=()

for src_path in src/*.asm
do
	base=$(basename "$src_path" .asm)
	obj_path="obj/$base.o"
	obj_paths+=("$obj_path")

	echo "Assembling $src_path"
	if ! ./ext/ca65 "$src_path" -o "$obj_path" --bin-include-dir ./bin
	then
		assembly_passed=0
		echo ""
	fi
done

if [ $assembly_passed -eq 0 ]
then
	exit 1
fi

echo "Linking"
./ext/ld65 -o bin/Z.bin -C src/Z.cfg "${obj_paths[@]}"

echo "Combining raw ROM with NES header"
cat OriginalNesHeader.bin bin/Z.bin >| bin/Z.nes

if [ $no_verify -eq 0 ]
then
	echo "Comparing bin/Z.nes and ext/Original.nes"
	if cmp -s bin/Z.nes ext/Original.nes
	then
		echo "ROM image is OK"
	else
		echo "ROM image mismatch"
		exit 1
	fi
fi
