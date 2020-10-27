0..9 | ForEach-Object -Parallel {
  Write-Host -foregroundColor yellow "Kicking off emacs Process [$($_+1)/10] this will take a while!"
  $expr = "emacs -batch -l ./scripts/emacs/load-srefactor.el 10 $($_)"
  $exitCode = Invoke-Expression $expr
  Write-Host -foregroundColor green "Finished Process [$($_+1)/10]"
} -ThrottleLimit 10