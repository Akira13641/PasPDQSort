unit PasPDQSort;

{$Mode Delphi}
{$PointerMath On}
{$COperators On}
{$TypedAddress On}
{$ImplicitExceptions Off}

interface

const
  PARTIAL_INSERTION_SORT_LIMIT = 8;
  INSERTION_SORT_THRESHOLD = 24;
  BLOCK_SIZE = 64;
  CACHE_LINE_SIZE = 64;
  NINTHER_THRESHOLD = 128;

type
  TPair<K, V> = record
    Key: K;
    Value: V;
    class function Create(constref AKey: K; constref AValue: V): TPair<K, V>; static; inline;
  end;

  Util<T> = record
    class function PreInc(var P: T): T; static; inline;
    class function PostInc(var P: T): T; static; inline;
    class function PreDec(var P: T): T; static; inline;
    class function PostDec(var P: T): T; static; inline;
    class function AlignCacheLine(const P: T): T; static; inline;
    class function Min(const A, B: T): T; static; inline;
  end;

  TPDQSorter<T> = record
  public type
    PT = ^T;
    TIsLess = function(constref A, B: T): Boolean;
  private type
    PtrUtil = Util<PT>;
    ByteUtil = Util<Byte>;
    BytePtrUtil = Util<PByte>;
    IntUtil = Util<PtrInt>;
  strict private
    class procedure Swap(const L, R: PT); static; inline;
    class procedure Sort3(const A, B, C: PT; const IsLess: TIsLess); static; inline;
    class procedure InsertionSort(const _Begin, _End: PT; const IsLess: TIsLess); static;
    class procedure UnguardedInsertionSort(const _Begin, _End: PT; const IsLess: TIsLess); static;
    class function PartialInsertionSort(const _Begin, _End: PT; const IsLess: TIsLess): Boolean; static;
    class procedure HeapSort(const _Begin, _End: PT; const IsLess: TIsLess); static;
    class procedure SwapOffsets(const First, Last: PT;
                                const OffsetsL, OffsetsR: PByte;
                                const Num: PtrUInt;
                                const UseSwaps: Boolean); static;
    class function PartitionRightBranchless(const _Begin, _End: PT; const IsLess: TIsLess): TPair<PT, Boolean>; static;
    class function PartitionRight(const _Begin, _End: PT; const IsLess: TIsLess): TPair<PT, Boolean>; static;
    class function PartitionLeft(const _Begin, _End: PT; const IsLess: TIsLess): PT; static;
    class procedure PDQSortLoop(var _Begin: PT;
                                const _End: PT;
                                const IsLess: TIsLess;
                                BadAllowed: PtrInt;
                                LeftMost: Boolean = True;
                                const Branchless: Boolean = True); static;
  public
    class procedure Sort(_Begin: PT;
                         const _End: PT;
                         const IsLess: TIsLess;
                         const Branchless: Boolean = True); static; inline; overload;
    class procedure Sort(constref Arr: array of T;
                         const IsLess: TIsLess;
                         const Branchless: Boolean = True); static; overload;
    class procedure Sort(constref Arr: TArray<T>;
                         const IsLess: TIsLess;
                         const Branchless: Boolean = True); static; inline; overload;
  end;

implementation

class function TPair<K, V>.Create(constref AKey: K; constref AValue: V): TPair<K, V>;
begin
  with Result do begin
    Key := AKey;
    Value := AValue;
  end;
end;

class function Util<T>.PreInc(var P: T): T;
begin
  Inc(P);
  Result := P;
end;

class function Util<T>.PostInc(var P: T): T;
begin
  Result := P;
  Inc(P);
end;

class function Util<T>.PreDec(var P: T): T;
begin
  Dec(P);
  Result := P;
end;

class function Util<T>.PostDec(var P: T): T;
begin
  Result := P;
  Dec(P);
end;

class function Util<T>.AlignCacheLine(const P: T): T;
var IP: PtrUInt;
begin
  IP := PtrUInt(P);
  IP := (IP + CACHE_LINE_SIZE - 1) and -CACHE_LINE_SIZE;
  Result := T(IP);
end;

class function Util<T>.Min(const A, B: T): T;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

class procedure TPDQSorter<T>.Swap(const L, R: PT);
var Tmp: T;
begin
  Tmp := L^;
  L^ := R^;
  R^ := Tmp;
end;

class procedure TPDQSorter<T>.Sort3(const A, B, C: PT; const IsLess: TIsLess);
begin
  if IsLess(B^, A^) then Swap(A, B);
  if IsLess(C^, B^) then Swap(B, C);
  if IsLess(B^, A^) then Swap(A, B);
end;

class procedure TPDQSorter<T>.InsertionSort(const _Begin, _End: PT; const IsLess: TIsLess);
var
  Tmp: T;
  Cur, Sift, SiftL: PT;
begin
  if _Begin = _End then Exit;
  Cur := _Begin + 1;
  while Cur <> _End do begin
    Sift := Cur;
    SiftL := Cur - 1;
    if IsLess(Sift^, SiftL^) then begin
      Tmp := Sift^;
      repeat
        PtrUtil.PostDec(Sift)^ := SiftL^;
      until (Sift = _Begin) or (IsLess(Tmp, PtrUtil.PreDec(SiftL)^) = False);
      Sift^ := Tmp;
    end;
    Cur += 1;
  end;
end;

class procedure TPDQSorter<T>.UnguardedInsertionSort(const _Begin, _End: PT; const IsLess: TIsLess);
var
  Tmp: T;
  Cur, Sift, SiftL: PT;
begin
  if _Begin = _End then Exit;
  Cur := _Begin + 1;
  while Cur <> _End do begin
    Sift := Cur;
    SiftL := Cur - 1;
    if IsLess(Sift^, SiftL^) then begin
      Tmp := Sift^;
      repeat
        PtrUtil.PostDec(Sift)^ := SiftL^;
      until IsLess(Tmp, PtrUtil.PreDec(SiftL)^) = False;
      Sift^ := Tmp;
    end;
    Cur += 1;
  end;
end;

class function TPDQSorter<T>.PartialInsertionSort(const _Begin, _End: PT; const IsLess: TIsLess): Boolean;
var
  Limit: PtrUInt;
  Tmp: T;
  Cur, Sift, SiftL: PT;
begin
  if _Begin = _End then Exit(True);
  Limit := 0;
  Cur := _Begin + 1;
  while Cur <> _End do begin
    if Limit > PARTIAL_INSERTION_SORT_LIMIT then Exit(False);
    Sift := Cur;
    SiftL := Cur - 1;
    if IsLess(Sift^, SiftL^) then begin
      Tmp := Sift^;
      repeat
        PtrUtil.PostDec(Sift)^ := SiftL^;
      until (Sift = _Begin) or (IsLess(Tmp, PtrUtil.PreDec(SiftL)^) = False);
      Sift^ := Tmp;
      Limit += Cur - Sift;
    end;
    Cur += 1;
  end;
  Result := True;
end;

class procedure TPDQSorter<T>.HeapSort(const _Begin, _End: PT; const IsLess: TIsLess);
var
  I, OuterLen: PtrUInt;

  procedure SiftDown(const _Begin2: PT; const InnerLen: PtrUInt; Node: PtrUInt); inline;
  var Left, Right, Greater: PtrUInt;
  begin
    while True do begin
      Left := 2 * Node + 1;
      Right := 2 * Node + 2;
      if (Right < InnerLen) and IsLess(_Begin2[Left], _Begin2[Right]) then
        Greater := Right
      else
        Greater := Left;
      if (Greater >= InnerLen) or not IsLess(_Begin2[Node], _Begin2[Greater]) then
        Break;
      Swap(@_Begin2[Node], @_Begin2[Greater]);
      Node := Greater;
    end;
  end;

begin
  OuterLen := (_End - _Begin) + 1;
  for I := OuterLen div 2 - 1 downto 0 do
    SiftDown(_Begin, OuterLen, I);
  for I := OuterLen - 1 downto 1 do begin
    Swap(_Begin, @_Begin[I]);
    SiftDown(_Begin, I, 0);
  end;
end;

class procedure TPDQSorter<T>.SwapOffsets(const First, Last: PT;
                                          const OffsetsL, OffsetsR: PByte;
                                          const Num: PtrUInt;
                                          const UseSwaps: Boolean);
var
  Tmp: T;
  I: PtrUInt;
  L, R: PT;
begin
  if UseSwaps then begin
    I := 0;
    while I < Num do begin
      Swap(First + OffsetsL[I], Last - OffsetsR[I]);
      Inc(I);
    end;
  end else if Num > 0 then begin
    L := First + OffsetsL[0];
    R := Last - OffsetsR[0];
    Tmp := L^;
    L^ := R^;
    for I := 1 to Pred(Num) do begin
      L := First + OffsetsL[I];
      R^ := L^;
      R := Last - OffsetsR[I];
      L^ := R^;
    end;
    R^ := Tmp;
  end;
end;

class function TPDQSorter<T>.PartitionRightBranchless(const _Begin, _End: PT; const IsLess: TIsLess): TPair<PT, Boolean>;
var
  I: Byte;
  AlreadyPartitioned: Boolean;
  Pivot: T;
  First, Last, It, PivotPos: PT;
  OffsetsL, OffsetsR: PByte;
  Num, NumL, NumR, StartL, StartR, LSize, RSize, UnknownLeft: PtrInt;
  OffsetsLStorage, OffsetsRStorage: array[0..Pred(BLOCK_SIZE + CACHE_LINE_SIZE)] of Byte;
begin
  Pivot := _Begin^;
  First := _Begin;
  Last := _End;
  while IsLess(PtrUtil.PreInc(First)^, Pivot) do
    ;
  if First - 1 = _Begin then begin
    while (First < Last) and not IsLess(PtrUtil.PreDec(Last)^, Pivot) do
      ;
  end else
    while not IsLess(PtrUtil.PreDec(Last)^, Pivot) do
      ;
  AlreadyPartitioned := First >= Last;
  if not AlreadyPartitioned then begin
    Swap(First, Last);
    First += 1;
  end;
  OffsetsL := BytePtrUtil.AlignCacheLine(@OffsetsLStorage[0]);
  OffsetsR := BytePtrUtil.AlignCacheLine(@OffsetsRStorage[0]);
  NumL := 0;
  NumR := 0;
  StartL := 0;
  StartR := 0;
  while Last - First > 2 * BLOCK_SIZE do begin
    if NumL = 0 then begin
      StartL := 0;
      It := First;
      I := 0;
      while I < BLOCK_SIZE do begin
        OffsetsL[NumL] := ByteUtil.PostInc(I);
        NumL += PtrInt(not IsLess(It^, Pivot));
        It += 1;
        OffsetsL[NumL] := ByteUtil.PostInc(I);
        NumL += PtrInt(not IsLess(It^, Pivot));
        It += 1;
        OffsetsL[NumL] := ByteUtil.PostInc(I);
        NumL += PtrInt(not IsLess(It^, Pivot));
        It += 1;
        OffsetsL[NumL] := ByteUtil.PostInc(I);
        NumL += PtrInt(not IsLess(It^, Pivot));
        It += 1;
        OffsetsL[NumL] := ByteUtil.PostInc(I);
        NumL += PtrInt(not IsLess(It^, Pivot));
        It += 1;
        OffsetsL[NumL] := ByteUtil.PostInc(I);
        NumL += PtrInt(not IsLess(It^, Pivot));
        It += 1;
        OffsetsL[NumL] := ByteUtil.PostInc(I);
        NumL += PtrInt(not IsLess(It^, Pivot));
        It += 1;
        OffsetsL[NumL] := ByteUtil.PostInc(I);
        NumL += PtrInt(not IsLess(It^, Pivot));
        It += 1;
      end;
    end;
    if NumR = 0 then begin
      StartR := 0;
      It := Last;
      I := 0;
      while I < BLOCK_SIZE do begin
        OffsetsR[NumR] := ByteUtil.PreInc(I);
        NumR += PtrInt(IsLess(PtrUtil.PreDec(It)^, Pivot));
        OffsetsR[NumR] := ByteUtil.PreInc(I);
        NumR += PtrInt(IsLess(PtrUtil.PreDec(It)^, Pivot));
        OffsetsR[NumR] := ByteUtil.PreInc(I);
        NumR += PtrInt(IsLess(PtrUtil.PreDec(It)^, Pivot));
        OffsetsR[NumR] := ByteUtil.PreInc(I);
        NumR += PtrInt(IsLess(PtrUtil.PreDec(It)^, Pivot));
        OffsetsR[NumR] := ByteUtil.PreInc(I);
        NumR += PtrInt(IsLess(PtrUtil.PreDec(It)^, Pivot));
        OffsetsR[NumR] := ByteUtil.PreInc(I);
        NumR += PtrInt(IsLess(PtrUtil.PreDec(It)^, Pivot));
        OffsetsR[NumR] := ByteUtil.PreInc(I);
        NumR += PtrInt(IsLess(PtrUtil.PreDec(It)^, Pivot));
        OffsetsR[NumR] := ByteUtil.PreInc(I);
        NumR += PtrInt(IsLess(PtrUtil.PreDec(It)^, Pivot));
      end;
    end;
    Num := IntUtil.Min(NumL, NumR);
    SwapOffsets(First, Last, OffsetsL + StartL, OffsetsR + StartR, Num, NumL = NumR);
    NumL -= Num;
    NumR -= Num;
    StartL += Num;
    StartR += Num;
    if NumL = 0 then
      First += BLOCK_SIZE;
    if NumR = 0 then
      Last -= BLOCK_SIZE;
  end;
  LSize := 0;
  RSize := 0;
  if Boolean(NumR or NumL) then
    UnknownLeft := (Last - First) - BLOCK_SIZE
  else
    UnknownLeft := (Last - First);
  if Boolean(NumR) then begin
    LSize := UnknownLeft;
    RSize := BLOCK_SIZE;
  end else if Boolean(NumL) then begin
    LSize := BLOCK_SIZE;
    RSize := UnknownLeft;
  end else begin
    LSize := UnknownLeft div 2;
    RSize := UnknownLeft - LSize;
  end;
  if Boolean(UnknownLeft) and not Boolean(NumL) then begin
    StartL := 0;
    It := First;
    I := 0;
    while I < LSize do begin
      OffsetsL[NumL] := ByteUtil.PostInc(I);
      NumL += PtrInt(not IsLess(It^, Pivot));
      It += 1;
    end;
  end;
  if Boolean(UnknownLeft) and not Boolean(NumR) then begin
    StartR := 0;
    It := Last;
    I := 0;
    while I < RSize do begin
      OffsetsR[NumR] := ByteUtil.PreInc(I);
      NumR += PtrInt(IsLess(PtrUtil.PreDec(It)^, Pivot));
    end;
  end;
  Num := IntUtil.Min(NumL, NumR);
  SwapOffsets(First, Last, OffsetsL + StartL, OffsetsR + StartR, Num, NumL = NumR);
  NumL -= Num;
  NumR -= Num;
  StartL += Num;
  StartR += Num;
  if NumL = 0 then
    First += LSize;
  if NumR = 0 then
    Last -= RSize;
  if Boolean(NumL) then begin
    OffsetsL += StartL;
    while Boolean(IntUtil.PostDec(NumL)) do
      Swap(First + OffsetsL[NumL], PtrUtil.PreDec(Last));
    First := Last;
  end;
  if Boolean(NumR) then begin
    OffsetsR += StartR;
    while Boolean(IntUtil.PostDec(NumR)) do begin
      Swap(Last - OffsetsR[NumR], First);
      First += 1;
    end;
    Last := First;
  end;
  PivotPos := First - 1;
  _Begin^ := PivotPos^;
  PivotPos^ := Pivot;
  Result := TPair<PT, Boolean>.Create(PivotPos, AlreadyPartitioned);
end;

class function TPDQSorter<T>.PartitionRight(const _Begin, _End: PT; const IsLess: TIsLess): TPair<PT, Boolean>;
var
  AlreadyPartitioned: Boolean;
  Pivot: T;
  First, Last, PivotPos: PT;
begin
  Pivot := _Begin^;
  First := _Begin;
  Last := _End;
  while IsLess(PtrUtil.PreInc(First)^, Pivot) do
    ;
  if First - 1 = _Begin then begin
    while (First < Last) and not IsLess(PtrUtil.PreDec(Last)^, Pivot) do
      ;
  end else
    while not IsLess(PtrUtil.PreDec(Last)^, Pivot) do
      ;
  AlreadyPartitioned := First >= Last;
  while First < Last do begin
    Swap(First, Last);
    while IsLess(PtrUtil.PreInc(First)^, Pivot) do
      ;
    while not IsLess(PtrUtil.PreDec(Last)^, Pivot) do
      ;
  end;
  PivotPos := First - 1;
  _Begin^ := PivotPos^;
  PivotPos^ := Pivot;
  Result := TPair<PT, Boolean>.Create(PivotPos, AlreadyPartitioned);
end;

class function TPDQSorter<T>.PartitionLeft(const _Begin, _End: PT; const IsLess: TIsLess): PT;
var
  Pivot: T;
  First, Last, PivotPos: PT;
begin
  Pivot := _Begin^;
  First := _Begin;
  Last := _End;
  while IsLess(Pivot, PtrUtil.PreDec(Last)^) do
    ;
  if Last + 1 = _End then begin
    while (First < Last) and not IsLess(Pivot, PtrUtil.PreInc(First)^) do
      ;
  end else
    while not IsLess(Pivot, PtrUtil.PreInc(First)^) do
      ;
  while First < Last do begin
    Swap(First, Last);
    while IsLess(Pivot, PtrUtil.PreDec(Last)^) do
      ;
    while not IsLess(Pivot, PtrUtil.PreInc(First)^) do
      ;
  end;
  PivotPos := Last;
  _Begin^ := PivotPos^;
  PivotPos^ := Pivot;
  Result := PivotPos;
end;

class procedure TPDQSorter<T>.PDQSortLoop(var _Begin: PT;
                                          const _End: PT;
                                          const IsLess: TIsLess;
                                          BadAllowed: PtrInt;
                                          LeftMost: Boolean;
                                          const Branchless: Boolean);
var
  AlreadyPartitioned, HighlyUnbalanced: Boolean;
  PivotPos: PT;
  Size, S2, LSize, RSize: PtrInt;
  PartResult: TPair<PT, Boolean>;
begin
  while True do begin
    Size := _End - _Begin;
    if Size < INSERTION_SORT_THRESHOLD then begin
      if LeftMost then
        InsertionSort(_Begin, _End, IsLess)
      else
        UnguardedInsertionSort(_Begin, _End, IsLess);
      Exit;
    end;
    S2 := Size div 2;
    if Size > NINTHER_THRESHOLD then begin
      Sort3(_Begin, _Begin + S2, _End - 1, IsLess);
      Sort3(_Begin + 1, _Begin + (S2 - 1), _End - 2, IsLess);
      Sort3(_Begin + 2, _Begin + (S2 + 1), _End - 3, IsLess);
      Sort3(_Begin + (S2 - 1), _Begin + S2, _Begin + (S2 + 1), IsLess);
      Swap(_Begin, _Begin + S2);
    end else
      Sort3(_Begin + S2, _Begin, _End - 1, IsLess);
    if (not LeftMost) and (not IsLess((_Begin - 1)^, _Begin^)) then begin
      _Begin := PartitionLeft(_Begin, _End, IsLess) + 1;
      Continue;
    end;
    if Branchless then
      PartResult := PartitionRightBranchless(_Begin, _End, IsLess)
    else
      PartResult := PartitionRight(_Begin, _End, IsLess);
    PivotPos := PartResult.Key;
    AlreadyPartitioned := PartResult.Value;
    LSize := PivotPos - _Begin;
    RSize := _End - (PivotPos + 1);
    HighlyUnbalanced := (LSize < Size div 8) or (RSize < Size div 8);
    if HighlyUnbalanced then begin
      if IntUtil.PreDec(BadAllowed) = 0 then begin
        HeapSort(_Begin, _End, IsLess);
        Exit;
      end;
      if LSize >= INSERTION_SORT_THRESHOLD then begin
        Swap(_Begin, _Begin + LSize div 4);
        Swap(PivotPos - 1, PivotPos - LSize div 4);
        if LSize > NINTHER_THRESHOLD then begin
          Swap(_Begin + 1, _Begin + (LSize div 4 + 1));
          Swap(_Begin + 2, _Begin + (LSize div 4 + 2));
          Swap(PivotPos - 2, PivotPos - (LSize div 4 + 1));
          Swap(PivotPos - 3, PivotPos - (LSize div 4 + 2));
        end;
      end;
      if RSize >= INSERTION_SORT_THRESHOLD then begin
        Swap(PivotPos + 1, PivotPos + (1 + RSize div 4));
        Swap(_End - 1, _End - RSize div 4);
        if RSize > NINTHER_THRESHOLD then begin
          Swap(PivotPos + 2, PivotPos + (2 + RSize div 4));
          Swap(PivotPos + 3, PivotPos + (3 + RSize div 4));
          Swap(_End - 2, _End - (1 + RSize div 4));
          Swap(_End - 3, _End - (2 + RSize div 4));
        end;
      end;
    end else begin
      if (AlreadyPartitioned and
          PartialInsertionSort(_Begin, PivotPos, IsLess) and
          PartialInsertionSort(PivotPos + 1, _End, IsLess)) then
        Exit;
    end;
    PDQSortLoop(_Begin, PivotPos, IsLess, BadAllowed, LeftMost, Branchless);
    _Begin := PivotPos + 1;
    LeftMost := false;
  end;
end;

class procedure TPDQSorter<T>.Sort(_Begin: PT; const _End: PT; const IsLess: TIsLess; const Branchless: Boolean);
begin
  if _Begin = _End then Exit;
  PDQSortLoop(_Begin, _End, IsLess, BSRQWord(_End - _Begin) + 1, True, Branchless);
end;

class procedure TPDQSorter<T>.Sort(constref Arr: array of T; const IsLess: TIsLess; const Branchless: Boolean);
begin
  Sort(@Arr[0], @Arr[High(Arr)] + 1, IsLess, Branchless);
end;

class procedure TPDQSorter<T>.Sort(constref Arr: TArray<T>; const IsLess: TIsLess; const Branchless: Boolean);
begin
  Sort(@Arr[0], @Arr[High(Arr)] + 1, IsLess, Branchless);
end;

end.