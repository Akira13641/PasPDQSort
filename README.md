# PasPDQSort
This is a direct translation of Orson Peters' [PDQSort](https://github.com/orlp/pdqsort) algorithm from C++ to Free Pascal,
with one difference in that the internally-used-in-some-cases HeapSort algorithm is a translation of the one found in Stjepan Glavina's
[Rust implementation](https://crates.io/crates/pdqsort) of PDQSort.

Basic usage (in the ObjFPC syntax mode) is as follows:

```Pascal
program Example;

{$mode ObjFPC}

uses Math, PasPDQSort;

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
  specialize TPDQSorter<Int32>.Sort(A);

  // Note: This implementation of the algorithm requires that the type being sorted
  // (that is, the one `TPDQSorter` is specialized with) has an existing
  // implementation of the "less than" operator for it, and will fail to compile
  // if handed something for which that is not the case.

  // In the above example, "less than" happens to be built into the language for `Int32`,
  // but user-defined custom implementations of the operator for custom types will also work fine.
end.
```

Compatible with Free Pascal 3.1.1 and higher.
