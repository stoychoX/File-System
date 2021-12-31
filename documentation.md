## Functional programming project
*Synopsis*: Program, which simulates a simple file system.

### Data structures used
In order to represent files, we'd need to implement our custom data structure. In this solution we would be using:
 ```haskell 
 data FileSystem =
     File String String |
     Root String [FileSystem]
```
* File <FileName\> <FileContent\>
* Root <PathName\> <PathContent\>




-- dependency: Googleson's article (..)