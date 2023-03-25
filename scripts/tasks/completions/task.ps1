$scriptBlock = {
	param($commandName, $wordToComplete, $cursorPosition)
	$regex = "task(?:.exe)? (.*)$"
	$startsWith = $wordToComplete | Select-String $regex -AllMatches | ForEach-Object { $_.Matches.Groups[1].Value }
	$listOutput = $(task --list-all --silent)
	$listOutput | Where-Object {$_ -like "$startsWith*"}
}

Register-ArgumentCompleter -Native -CommandName task -ScriptBlock $scriptBlock
