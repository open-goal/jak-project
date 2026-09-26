# (AI-assisted) Project-local Windows development commands for Jak1.
[CmdletBinding()]
param(
    [ValidateSet('doctor', 'configure', 'build', 'font', 'extract', 'compile', 'run', 'repl')]
    [string]$Action = 'doctor',
    [ValidateRange(1, 64)]
    [int]$Jobs = 8
)
$ErrorActionPreference = 'Stop'
$projectRoot = (Resolve-Path (Join-Path $PSScriptRoot '../..')).Path
$python = Join-Path $PSScriptRoot '.venv/Scripts/python.exe'
$fontGenerator = Join-Path $PSScriptRoot 'build-jak1-korean-font.py'
. (Join-Path $PSScriptRoot 'dev-env.ps1')
$compilerBin = Join-Path $PSScriptRoot 'bin'

function Sync-Jak1CompilerBinaries {
    $buildBin = Join-Path $projectRoot 'out/build/Release/bin'
    foreach ($required in @('goalc.exe', 'extractor.exe', 'common.dll')) {
        if (!(Test-Path -LiteralPath (Join-Path $buildBin $required) -PathType Leaf)) {
            throw "Missing $required in $buildBin. Run build first."
        }
    }
    New-Item -ItemType Directory -Path $compilerBin -Force | Out-Null
    foreach ($source in (Get-ChildItem -LiteralPath $buildBin -File | Where-Object Extension -In '.exe', '.dll')) {
        $destination = Join-Path $compilerBin $source.Name
        $existing = Get-Item -LiteralPath $destination -ErrorAction SilentlyContinue
        if (!$existing -or $existing.Length -ne $source.Length -or $existing.LastWriteTimeUtc -lt $source.LastWriteTimeUtc) {
            Copy-Item -LiteralPath $source.FullName -Destination $destination -Force
        }
    }
}

function Invoke-Jak1Extract {
    if (!(Test-Path 'iso_data/jak1/SCPS_560.03')) {
        throw 'Expected extracted SCPS_560.03. Extract the supplied ISO first.'
    }
    # This uploaded SCPS-56003 release is mapped to ntsc_v1 by extractor_util.cpp.
    $overrides = '{"decompile_code":false,"levels_extract":true,"allowed_objects":[],"expected_elf_name":"SCPS_560.03"}'
    & (Join-Path $compilerBin 'extractor.exe') ./iso_data/jak1 --folder --decompile --game jak1 --proj-path $projectRoot --decomp-config-override $overrides
    if ($LASTEXITCODE -ne 0) { throw 'Jak 1 asset extraction failed' }
}

Push-Location $projectRoot
try {
    switch ($Action) {
        'doctor' {
            foreach ($tool in @('python', 'ninja', 'nasm', 'task', 'cl')) {
                Get-Command $tool | Select-Object Name, Source
            }
            Write-Host "ISO files extracted: $(Test-Path 'iso_data/jak1/SCPS_560.03')"
            Write-Host "Assets converted: $(Test-Path 'decompiler_out/jak1/assets/game_text.txt')"
            Write-Host "Game compiled: $(Test-Path 'out/jak1/iso/GAME.CGO')"
            $global:LASTEXITCODE = 0
        }
        'configure' {
            $buildDir = [IO.Path]::GetFullPath((Join-Path $projectRoot 'out/build/Release'))
            if (!$buildDir.StartsWith($projectRoot + [IO.Path]::DirectorySeparatorChar, [StringComparison]::OrdinalIgnoreCase)) {
                throw "Unexpected CMake build directory: $buildDir"
            }
            $cacheFile = Join-Path $buildDir 'CMakeCache.txt'
            $fresh = $false
            if (Test-Path -LiteralPath $cacheFile) {
                $homeLine = Select-String -LiteralPath $cacheFile -Pattern '^CMAKE_HOME_DIRECTORY:INTERNAL=(.*)$'
                $makeLine = Select-String -LiteralPath $cacheFile -Pattern '^CMAKE_MAKE_PROGRAM:FILEPATH=(.*)$'
                if ($homeLine) {
                    $previousRoot = [IO.Path]::GetFullPath($homeLine.Matches[0].Groups[1].Value)
                    $fresh = ![string]::Equals($previousRoot, $projectRoot, [StringComparison]::OrdinalIgnoreCase)
                }
                if ($makeLine -and !(Test-Path -LiteralPath $makeLine.Matches[0].Groups[1].Value)) { $fresh = $true }
            }
            if ($fresh) {
                Write-Host 'CMake cache refers to an old checkout path; refreshing generated build files.'
                & $python -m cmake --fresh --preset Release-windows-msvc -DBUILD_TESTING=ON
            } else {
                & $python -m cmake --preset Release-windows-msvc -DBUILD_TESTING=ON
            }
        }
        'build' {
            & $python -m cmake --build out/build/Release --target gk goalc decompiler goalc-test --parallel $Jobs
            if ($LASTEXITCODE -ne 0) { throw 'C++ build failed. Run .\dev.ps1 configure if the checkout path changed.' }
            Sync-Jak1CompilerBinaries
        }
        'font' {
            & $python $fontGenerator
        }
        'extract' {
            Sync-Jak1CompilerBinaries
            Invoke-Jak1Extract
        }
        'compile' {
            Sync-Jak1CompilerBinaries
            # Menu and subtitle Korean text share the same generated jamo bank.
            & $python $fontGenerator
            if ($LASTEXITCODE -ne 0) { throw 'Korean font generation failed' }

            # Texture replacements are consumed by the extractor rather than
            # loaded from custom_assets at runtime. Re-extract only when the
            # generated atlas pixels actually changed.
            $fontTextureObject = Get-Item 'decompiler_out/jak1/raw_obj/tpage-1278.go' -ErrorAction SilentlyContinue
            $fontAtlases = @(Get-ChildItem 'custom_assets/jak1/texture_replacements/gamefontnew' -Filter 'ascii.*.png' -File)
            if (!$fontTextureObject -or ($fontAtlases | Where-Object LastWriteTime -gt $fontTextureObject.LastWriteTime)) {
                Write-Host 'Korean font atlas changed; rebuilding extracted texture assets.'
                Invoke-Jak1Extract
            }

            # game_text.gp references its JSON files indirectly, so GOAL's make
            # graph cannot see those timestamps. Touch the project input when a
            # translation or generated encoding changed so COMMON.TXT rebuilds.
            $textProject = Get-Item 'game/assets/jak1/game_text.gp'
            $textOutput = Get-Item 'out/jak1/iso/17COMMON.TXT' -ErrorAction SilentlyContinue
            $textInputs = @(
                $textProject,
                (Get-Item 'game/assets/fonts/jak2_jak3_korean_db.json'),
                (Get-Item 'game/assets/jak1/korean_glyph_map.json'),
                (Get-Item 'goal_src/jak1/engine/gfx/korean-font.gc')
            )
            $textInputs += @(Get-ChildItem 'game/assets/jak1/text' -Filter '*.json' -File)
            if (!$textOutput -or ($textInputs | Where-Object LastWriteTime -gt $textOutput.LastWriteTime)) {
                $textProject.LastWriteTime = Get-Date
            }

            # Subtitle JSON files are indirect project inputs as well. Ensure a
            # newly added or edited Korean bank rebuilds 17SUBTIT.TXT.
            $subtitleProject = Get-Item 'game/assets/jak1/game_subtitle.gp'
            $subtitleOutput = Get-Item 'out/jak1/iso/17SUBTIT.TXT' -ErrorAction SilentlyContinue
            $subtitleInputs = @($subtitleProject)
            $subtitleInputs += @(Get-ChildItem 'game/assets/jak1/subtitle' -Filter '*.json' -File)
            # Subtitle serialization runs in common.dll. A rebuilt converter
            # must regenerate the banks even when their JSON did not change.
            $subtitleInputs += @(Get-Item 'out/build/Release/bin/common.dll')
            if (!$subtitleOutput -or ($subtitleInputs | Where-Object LastWriteTime -gt $subtitleOutput.LastWriteTime)) {
                $subtitleProject.LastWriteTime = Get-Date
            }
            & (Join-Path $compilerBin 'goalc.exe') --game jak1 --proj-path $projectRoot --iso-path (Join-Path $projectRoot 'iso_data/jak1') --cmd '(mi)'
        }
        'run' { Sync-Jak1CompilerBinaries; & (Join-Path $compilerBin 'gk.exe') -v --game jak1 --proj-path $projectRoot -- -boot -fakeiso -debug }
        'repl' { Sync-Jak1CompilerBinaries; & (Join-Path $compilerBin 'goalc.exe') --game jak1 --proj-path $projectRoot --iso-path (Join-Path $projectRoot 'iso_data/jak1') }
    }
    if ($LASTEXITCODE -ne 0) { throw "$Action failed with exit code $LASTEXITCODE" }
} finally {
    Pop-Location
}
