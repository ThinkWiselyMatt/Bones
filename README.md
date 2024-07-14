# Bones

# Haskell Interoperability Webservice

Install haskell 

Build and run with stack 

`stack clean`

`stack build`

`stack test` 


`stack exec Bones-exe`

to run once built 

`http://localhost:3001/scotty/

http://localhost:3003/servant/

http://localhost:3002/yesod/`

`/csharp` to call into C# exe 

`/cpp` to call into C++ dll

`/cpp/add/x/y` C++ dll add function 

`/python/scriptname.py` to run scripts you place in \ServerDependancies\PythonScripts 


Should build and run on Windows and Ubuntu 

to build the required native exports library in Ubuntu 
`./native`

`g++ -c -fPIC exports.cpp -o exports.o`

`g++ -shared -o libNativeExports.so exports.o`

will need to place a copy of .so in same dir at the Bones-exe executable 
otherwise the cpp endpoints will fail 


