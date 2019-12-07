# PasPDQSort
This is a direct translation of Orson Peters' [PDQSort](https://github.com/orlp/pdqsort) algorithm from C++ to Pascal.

Basic usage (in the ObjFPC syntax mode) is as follows:

```Pascal
program Example;

{$mode ObjFPC}

uses Math, PasPDQSort;

function Int32LessThan(constref A, B: Int32): Boolean;
begin
  Result := A < B;
end;

var
  I: PtrInt;
  A: array of Int32;

begin
  // generate a random array for example purposes.
  Randomize();
  SetLength(A, 500);
  for I := 0 to 499 do
    A[I] := RandomRange(1, 1001);

  // sort it!
  specialize TPDQSorter<Int32>.Sort(A, @Int32LessThan);
end.
```

Compatible with Free Pascal 3.1.1 and higher.
