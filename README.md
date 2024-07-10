# Bones

# Haskell Interoperability Webservice

Install haskell 

Build and run with stack 

`stack clean`

`stack build`

`stack test` 


`stack exec bones-exe`

to run once built 

`http://localhost:3001/scotty/

http://localhost:3003/servant/

http://localhost:3002/yesod/`

`/csharp` to call into C# exe 

`/cpp` to call into C++ dll

`/cpp/add/x/y` C++ dll add function 

`/python/scriptname.py` to run scripts you place in \ServerDependancies\PythonScripts (still working on calling python in haskell itself... inline-python has disappeared)


I have not tested this on any OS other than windows -- you might need to rebuild DLLs if using another OS

decent guide for getting started on Windows : https://travishorn.com/haskell-development-on-windows
