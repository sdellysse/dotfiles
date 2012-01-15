Set WshShell = CreateObject("WScript.Shell")
WshShell.run "c:\cygwin\bin\bash.exe --login -i -c 'exit'", 0
Set WshShell = Nothing