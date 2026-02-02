program SmeddumMirror;

const
  GRID_SIZE = 10; 

type
  TStencil = array[1..GRID_SIZE, 1..GRID_SIZE] of Integer;

var
  TargetObj, CandidateObj : TStencil;
  
procedure Clear(var S: TStencil);
var r, c: Integer;
begin
  for r := 1 to GRID_SIZE do for c := 1 to GRID_SIZE do S[r, c] := 0;
end;

procedure PrintStencil(Name: String; var S: TStencil);
var r, c: Integer;
begin
  writeln('   Object: ', Name);
  writeln('   +--------------------+');
  for r := 1 to GRID_SIZE do
  begin
    write('   | ');
    for c := 1 to GRID_SIZE do
      if S[r, c] = 0 then write('. ') else write('# ');
    writeln('|');
  end;
  writeln('   +--------------------+');
end;

procedure Normalize(var Input, Output: TStencil);
var 
  r, c, minR, minC: Integer;
  Temp: TStencil; 
begin
  minR := GRID_SIZE; minC := GRID_SIZE;
  Clear(Temp);
  for r := 1 to GRID_SIZE do for c := 1 to GRID_SIZE do
    if Input[r, c] = 1 then begin
      if r < minR then minR := r;
      if c < minC then minC := c;
    end;
  for r := 1 to GRID_SIZE do for c := 1 to GRID_SIZE do
    if Input[r, c] = 1 then
      if (r - minR + 1 <= GRID_SIZE) and (c - minC + 1 <= GRID_SIZE) then
        Temp[r - minR + 1, c - minC + 1] := 1;
  Output := Temp; 
end;

procedure Rotate90(var Input, Output: TStencil);
var r, c, maxDim: Integer; Buffer: TStencil;
begin
  Clear(Buffer); maxDim := 5; 
  for r := 1 to maxDim do for c := 1 to maxDim do
    if Input[r, c] = 1 then Buffer[c, maxDim - r + 1] := 1;
  Normalize(Buffer, Output); 
end;

procedure InitData;
begin
  Clear(TargetObj);
  { Define Target: Standard L }
  TargetObj[2, 2] := 1; TargetObj[3, 2] := 1; TargetObj[4, 2] := 1;
  TargetObj[4, 3] := 1; 

  { GENERATE CANDIDATE AUTOMATICALLY }
  { This guarantees it is a valid rotation }
  Rotate90(TargetObj, CandidateObj);
end;

function AreIdentical(var A, B: TStencil): Boolean;
var r, c: Integer;
begin
  AreIdentical := True;
  for r := 1 to GRID_SIZE do for c := 1 to GRID_SIZE do
    if A[r, c] <> B[r, c] then Exit(False);
end;

procedure SolveIdentity;
var NormTarget, NormCand, Temp: TStencil; i: Integer; Matched: Boolean;
begin
  writeln('>> SMEDDUM MODULE 2: SYMMETRY SOLVER (AUTO-GENERATED)');
  Normalize(TargetObj, NormTarget);
  Normalize(CandidateObj, NormCand);
  PrintStencil('Target', NormTarget);
  PrintStencil('Candidate', NormCand);

  writeln('>> Testing Symmetry Group Actions...');
  Matched := False; Temp := NormCand; 

  for i := 0 to 3 do begin
    if AreIdentical(NormTarget, Temp) then begin
      writeln('>> MATCH FOUND! Relation: Candidate rotated ', i * 90, ' deg = Target');
      Matched := True; Break;
    end;
    if i < 3 then Rotate90(Temp, Temp); 
  end;

  if not Matched then writeln('>> FAIL: No Relation Found.')
  else writeln('>> INVARIANCE CONFIRMED.');
end;

begin
  InitData;
  SolveIdentity;
end.
