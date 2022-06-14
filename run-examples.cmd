@ECHO OFF

MKDIR examples\out 2>nul

FOR %%f IN (examples\*.chmmm) DO (
    cabal run -v0 Compiler < "%%f" > "examples\out\%%~nf.out"
)
