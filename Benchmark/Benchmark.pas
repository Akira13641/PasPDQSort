program Benchmark;

{$mode Delphi}
{$ImplicitExceptions Off}

uses SysUtils, DateUtils, Math, Generics.Defaults, Generics.Collections, SortBase, PasPDQSort;

type
  TVec4 = record
    X, Y, Z, W: Double;
    class function Create(const IX, IY, IZ, IW: Double): TVec4; static; inline;
    class operator LessThan(constref A, B: TVec4): Boolean; inline;
  end;

  class function TVec4.Create(const IX, IY, IZ, IW: Double): TVec4;
  begin
    with Result do begin
      X := IX;
      Y := IY;
      Z := IZ;
      W := IW;
    end;
  end;

  class operator TVec4.LessThan(constref A, B: TVec4): Boolean;
  begin
    Result := A.X + A.Y + A.Z + A.W < B.X + B.Y + B.Z + B.W;
  end;

  function Vec4OrderTyped(constref A, B: TVec4): Int32;
  var ASum, BSum: Double;
  begin
    with A do ASum := X + Y + Z + W;
    with B do BSum := X + Y + Z + W;
    if ASum < BSum then Result := -1
    else if ASum = BSum then Result := 0
    else Result := 1;
  end;

  function Vec4OrderUntyped(Item1, Item2, Context: Pointer): Int32;
  var ASum, BSum: Double;
  begin
    with TVec4(Item1^) do ASum := X + Y + Z + W;
    with TVec4(Item2^) do BSum := X + Y + Z + W;
    if ASum < BSum then Result := -1
    else if ASum = BSum then Result := 0
    else Result := 1;
  end;

var
  I: PtrUInt;
  T1, T2, T3: TDateTime;
  A, B, C: array of TVec4;

begin
  Randomize();

  SetLength(A, 25000000);
  for I := 0 to 24999999 do A[I] := TVec4.Create(RandomRange(1, 50000001), RandomRange(1, 50000001),
                                                 RandomRange(1, 50000001), RandomRange(1, 50000001));
  T1 := Now();
  TPDQSorter<TVec4>.Sort(A);
  WriteLn('PDQSort: ', MillisecondsBetween(Now(), T1) / 1000 : 0 : 4);
  SetLength(A, 0);

  SetLength(B, 25000000);
  for I := 0 to 24999999 do B[I] := TVec4.Create(RandomRange(1, 50000001), RandomRange(1, 50000001),
                                                 RandomRange(1, 50000001), RandomRange(1, 50000001));
  T2 := Now();
  TArrayHelper<TVec4>.Sort(B, TComparer<TVec4>.Construct(Vec4OrderTyped));
  WriteLn('RTL-Generics QuickSort: ', MillisecondsBetween(Now(), T2) / 1000 : 0 : 4);
  SetLength(B, 0);

  SetLength(C, 25000000);
  for I := 0 to 24999999 do C[I] := TVec4.Create(RandomRange(1, 50000001), RandomRange(1, 50000001),
                                                 RandomRange(1, 50000001), RandomRange(1, 50000001));
  T3 := Now();
  QuickSort_ItemList_Context(@C[0], 25000000, SizeOf(TVec4), Vec4OrderUntyped, nil);
  WriteLn('SortBase QuickSort: ', MillisecondsBetween(Now(), T3) / 1000 : 0 : 4);
  SetLength(C, 0);
end.
