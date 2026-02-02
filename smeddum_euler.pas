program SmeddumEuler;
const
  SIZE = 12;
  C_EMPTY = 0; C_OBJECT = 1; C_OCEAN = 9; C_HOLE = 8;
type
  TGrid = array[1..SIZE, 1..SIZE] of Integer;
var
  Manifold : TGrid;

procedure FloodFill(StartR, StartC, TargetCol, FillCol: Integer; var G: TGrid);
var
  Q: array[1..400] of record r,c: Integer; end;
  Head, Tail, r, c, k, nr, nc: Integer;
  dr: array[1..4] of Integer = (-1, 1, 0, 0);
  dc: array[1..4] of Integer = (0, 0, -1, 1);
begin
  if (StartR < 1) or (StartR > SIZE) or (StartC < 1) or (StartC > SIZE) then Exit;
  if G[StartR, StartC] <> TargetCol then Exit;
  Head := 1; Tail := 1;
  Q[1].r := StartR; Q[1].c := StartC;
  G[StartR, StartC] := FillCol;
  while Head <= Tail do begin
    r := Q[Head].r; c := Q[Head].c;
    Inc(Head);
    for k := 1 to 4 do begin
      nr := r + dr[k]; nc := c + dc[k];
      if (nr>=1) and (nr<=SIZE) and (nc>=1) and (nc<=SIZE) then
        if G[nr, nc] = TargetCol then begin
          G[nr, nc] := FillCol;
          Inc(Tail); Q[Tail].r := nr; Q[Tail].c := nc;
        end;
    end;
  end;
end;

procedure DrawManifold(Title: String);
var r, c: Integer;
begin
  writeln('   ', Title);
  writeln('   +------------------------+');
  for r := 1 to SIZE do begin
    write('   | ');
    for c := 1 to SIZE do begin
      case Manifold[r, c] of
        C_EMPTY : write('. ');
        C_OBJECT: write('# ');
        C_OCEAN : write('~ ');
        C_HOLE  : write('O ');
      end;
    end;
    writeln('|');
  end;
  writeln('   +------------------------+');
end;

procedure InitScene;
var r, c: Integer;
begin
  for r := 1 to SIZE do for c := 1 to SIZE do Manifold[r, c] := C_EMPTY;
  for r := 3 to 5 do for c := 3 to 5 do Manifold[r, c] := C_OBJECT;
  for r := 7 to 9 do for c := 7 to 9 do Manifold[r, c] := C_OBJECT;
  Manifold[8, 8] := C_EMPTY;
end;

procedure ScanForHoles;
var r, c, HoleCount: Integer;
begin
  writeln('>> EULER SCOPE: Scanning Topology...');
  FloodFill(1, 1, C_EMPTY, C_OCEAN, Manifold);
  DrawManifold('Phase 1: Ocean Flood');
  HoleCount := 0;
  for r := 1 to SIZE do for c := 1 to SIZE do begin
    if Manifold[r, c] = C_EMPTY then begin
      Inc(HoleCount);
      FloodFill(r, c, C_EMPTY, C_HOLE, Manifold);
    end;
  end;
  DrawManifold('Phase 2: Hole Detection');
  writeln('>> ANALYSIS COMPLETE.');
  writeln('>> Total Holes Detected: ', HoleCount);
  if HoleCount > 0 then writeln('>> Topology: Complex (Contains Holes).')
  else writeln('>> Topology: Simple (Solid).');
end;

begin
  InitScene;
  DrawManifold('Initial State');
  ScanForHoles;
end.
