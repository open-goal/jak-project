# (AI-assisted) Dot-source to enable project tools in this PowerShell process only.
$projectRoot = (Resolve-Path (Join-Path $PSScriptRoot '../..')).Path
$projectTools = Join-Path $projectRoot '.tools'
$projectVcvars = Join-Path $projectTools 'vs-buildtools/VC/Auxiliary/Build/vcvars64.bat'
if (!(Test-Path $projectVcvars)) { throw "MSVC Build Tools not found: $projectVcvars" }
$compilerEnvironment = & $env:ComSpec /d /s /c "`"`"$projectVcvars`" >nul && set`""
if ($LASTEXITCODE -ne 0) { throw 'Could not activate MSVC environment.' }
$importedNames = [Collections.Generic.HashSet[string]]::new([StringComparer]::OrdinalIgnoreCase)
foreach ($entry in $compilerEnvironment) {
    if ($entry -match '^([^=]+)=(.*)$') {
        if ($importedNames.Add($Matches[1])) {
            [Environment]::SetEnvironmentVariable($Matches[1], $Matches[2], 'Process')
        }
    }
}
$projectBins = @(
    (Join-Path $PSScriptRoot '.venv/Scripts'),
    (Join-Path $PSScriptRoot 'deps/task'),
    (Join-Path $PSScriptRoot 'deps/nasm/nasm-2.16.01')
)
$env:PATH = ($projectBins -join ';') + ';' + $env:PATH
$env:VIRTUAL_ENV = Join-Path $PSScriptRoot '.venv'
$env:PYTHONNOUSERSITE = '1'
$env:PIP_CACHE_DIR = Join-Path $PSScriptRoot '.cache/pip'
$env:TEMP = Join-Path $PSScriptRoot '.tmp'
$env:TMP = $env:TEMP
New-Item -ItemType Directory -Force $env:TEMP | Out-Null
if (!(Get-Command cl.exe -ErrorAction SilentlyContinue)) {
    throw 'MSVC environment did not provide cl.exe. Check Build Tools installation.'
}
