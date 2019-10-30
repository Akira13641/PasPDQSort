# PasPDQSort
This is a direct translation of Orson Peters' [PDQSort](https://github.com/orlp/pdqsort) algorithm from C++ to Pascal,
with one exception in that the internally-used-in-some-cases HeapSort algorithm is a translation of the one found in Stjepan Glavina's
[Rust implementation](https://github.com/stjepang/pdqsort) of PDQSort.

Basic usage is roughly as follows:

```Pascal
program Example;

{$mode ObjFPC}

uses Math, PasPDQSort;

type Int32Sorter = specialize TPDQSorter<Int32>;

function Int32LessThan(constref A, B: Int32): Boolean;
begin
  Result := A < B;
end;

var
  I: PtrInt;
  A: array of Int32;

begin
  Randomize();
  SetLength(A, 500);
  for I := 0 to 499 do
    A[I] := RandomRange(1, 1001);
  Int32Sorter.Sort(A, @Int32LessThan);
end.
```

Compatible with Free Pascal 3.1.1 and higher.
