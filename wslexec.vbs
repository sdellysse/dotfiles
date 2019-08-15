For each aarg in wscript.arguments
    CMDLINE = CMDLINE & " " & aarg
Next

Dim oShell
Set oShell = CreateObject ("Wscript.Shell") 
oShell.Run "wsl -- " & CMDLINE, 0, false