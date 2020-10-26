# Assumed to be ran from the root directory
# this formats all *.gs and *.gc files in the repository!
# TODO:
# - dry-run (CI check for formatting)
# - fix mode (actually apply formatting)

$gcFiles = Get-ChildItem $PWD -name -recurse *.gc
$gsFiles = Get-ChildItem $PWD -name -recurse *.gs
$lispFiles = $gcFiles + $gsFiles

# TODO - batch prints (always print errors, print success in increments to reduce spam but still show progress)

# Fix any lines that start with a single `;` change them to a line comment convention of `;;`
Write-Host -foregroundColor yellow "Modifying all line-comments that begin with a single semi-colon"
(Measure-Command {
  $lispFiles | ForEach-Object -Parallel {
    $file = $_
    $content = Get-Content $file
    # TODO - this isn't the best way, but this is only a temporary workaround
    # until we fix the output to not use single ; line comments
    $i = 0
    for ($i = 0; $i -lt $content.Count ; $i++) {
      if ($content[$i] -match "^;{1}[^;].*") {
        $content[$i] = ";" + $content[$i]
      }
    }
    Set-Content -Path $file -Value $content
  }
}).TotalMilliseconds

# Test Case
# $expr = "emacs -batch -l ./scripts/emacs/load-srefactor.el ./decompiler/config/all-types.gc --eval '(srefactor-lisp-format-buffer)' -f save-buffer"
# $expr += ';$LastExitCode'
# $exitCode = Invoke-Expression $expr
Write-Host -foregroundColor yellow "Running all files through emac's formatting.  This will take a while!"
(Measure-Command {
  $lispFiles | ForEach-Object -Parallel {
    $file = $_
    $expr = "emacs -batch -l ./scripts/emacs/load-srefactor.el $($file) --eval '(srefactor-lisp-format-buffer)' -f save-buffer"
    $expr += ';$LastExitCode'
    $exitCode = Invoke-Expression $expr
    if($exitCode -ne 0){
      Write-Host -foregroundColor red "Unable to Format - '$($file)'"
    } else {
      Write-Host -foregroundColor cyan "Formatted - $($file)"
    }
  }
}).TotalMilliseconds