program Benchmark;

{$mode Delphi}
{$ImplicitExceptions Off}

uses SysUtils, DateUtils, Math, Generics.Defaults, Generics.Collections, PasPDQSort;

type
  TVec4 = record
    X, Y, Z, W: Double;
    class function Create(const IX, IY, IZ, IW: Double): TVec4; static; inline;
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

  function Vec4Less(constref A, B: TVec4): Boolean;
  var ASum, BSum: Double;
  begin
    with A do ASum := X + Y + Z + W;
    with B do BSum := X + Y + Z + W;
    Result := ASum < BSum;
  end;

  function Vec4Order(constref A, B: TVec4): Int32;
  var ASum, BSum: Double;
  begin
    with A do ASum := X + Y + Z + W;
    with B do BSum := X + Y + Z + W;
    if ASum < BSum then Result := -1
    else if ASum = BSum then Result := 0
    else Result := 1;
  end;

var
  I: PtrUInt;
  T1, T2: TDateTime;
  A, B: array of TVec4;

begin
  Randomize();
  
  SetLength(A, 100000000);
  for I := 0 to 99999999 do A[I] := TVec4.Create(RandomRange(1, 200000001), RandomRange(1, 200000001), RandomRange(1, 200000001), RandomRange(1, 200000001));
  T1 := Now();
  TPDQSorter<TVec4>.Sort(A, Vec4Less);
  WriteLn('PDQSort: ', MillisecondsBetween(Now(), T1) / 1000 : 0 : 4);
  SetLength(A, 0);

  SetLength(B, 100000000);
  for I := 0 to 99999999 do B[I] := TVec4.Create(RandomRange(1, 200000001), RandomRange(1, 200000001), RandomRange(1, 200000001), RandomRange(1, 200000001));
  T2 := Now();
  TArrayHelper<TVec4>.Sort(B, TComparer<TVec4>.Construct(Vec4Order));
  WriteLn('QuickSort: ', MillisecondsBetween(Now(), T2) / 1000 : 0 : 4);
  SetLength(B, 0);
end.
