program SmeddumVision;

{ ====================================================================
  SMEDDUM ENGINE - MODULE 1: THE EYE (Console Edition)
  --------------------------------------------------------------------
  Status:   ACTIVE
  Platform: Terminal / FPC
  Input:    Simulated ARC Grid
  Output:   ASCII Topology Map + Object Manifest
  ==================================================================== }

const
  MAX_GRID = 20; { Kept small for clean terminal output }
  MAX_OBJS = 50;

type
  TColor = 0..9;
  TGrid  = array[1..MAX_GRID, 1..MAX_GRID] of TColor;
  
  TPoint = record
    r, c: Integer;
  end;

  TObject = record
    ID       : Integer;
    Color    : TColor;
    Mass     : Integer;
    MinR, MaxR, MinC, MaxC: Integer;
  end;

  TObjectList = record
    Count: Integer;
    Items: array[1..MAX_OBJS] of TObject;
  end;

var
  InputGrid : TGrid;
  Detected  : TObjectList;
  Visited   : array[1..MAX_GRID, 1..MAX_GRID] of Boolean;

{ --- INIT --- }
procedure InitObject(var Obj: TObject; id: Integer; col: TColor);
begin
  Obj.ID := id;
  Obj.Color := col;
  Obj.Mass := 0;
  Obj.MinR := MAX_GRID; Obj.MaxR := 0;
  Obj.MinC := MAX_GRID; Obj.MaxC := 0;
end;

{ --- ALGORITHM: FLOOD FILL (Recursive for simplicity in this demo) --- }
procedure FloodFill(r, c: Integer; TargetColor: TColor; var Obj: TObject);
begin
  { Bounds Check }
  if (r < 1) or (r > MAX_GRID) or (c < 1) or (c > MAX_GRID) then Exit;
  
  { State Check }
  if Visited[r, c] then Exit;
  if InputGrid[r, c] <> TargetColor then Exit;

  { Process Pixel }
  Visited[r, c] := True;
  Inc(Obj.Mass);
  
  { Update Bounding Box }
  if r < Obj.MinR then Obj.MinR := r;
  if r > Obj.MaxR then Obj.MaxR := r;
  if c < Obj.MinC then Obj.MinC := c;
  if c > Obj.MaxC then Obj.MaxC := c;

  { Recurse Neighbors (4-way connectivity) }
  FloodFill(r - 1, c, TargetColor, Obj); { Up }
  FloodFill(r + 1, c, TargetColor, Obj); { Down }
  FloodFill(r, c - 1, TargetColor, Obj); { Left }
  FloodFill(r, c + 1, TargetColor, Obj); { Right }
end;

{ --- DRIVER: SCANNER --- }
procedure ScanManifold(var Grid: TGrid; var List: TObjectList);
var
  r, c: Integer;
begin
  List.Count := 0;
  { Reset Visited Map }
  for r := 1 to MAX_GRID do 
    for c := 1 to MAX_GRID do 
      Visited[r, c] := False;

  for r := 1 to MAX_GRID do
  begin
    for c := 1 to MAX_GRID do
    begin
      { If pixel is Solid (>0) and Unvisited, it's a NEW OBJECT }
      if (Grid[r, c] > 0) and (not Visited[r, c]) then
      begin
        Inc(List.Count);
        InitObject(List.Items[List.Count], List.Count, Grid[r, c]);
        
        { Extract the Topology }
        FloodFill(r, c, Grid[r, c], List.Items[List.Count]);
      end;
    end;
  end;
end;

{ --- UTILITY: ASCII VISUALIZER --- }
procedure DrawManifold;
var r, c: Integer;
begin
  writeln('   +----------------------+');
  for r := 1 to MAX_GRID do
  begin
    write('   | ');
    for c := 1 to MAX_GRID do
    begin
      if InputGrid[r, c] = 0 then write('. ')
      else write(InputGrid[r, c], ' ');
    end;
    writeln('|');
  end;
  writeln('   +----------------------+');
end;

{ --- UTILITY: TEST DATA GENERATOR --- }
procedure InjectTestPattern;
var r, c: Integer;
begin
  { Clear Grid }
  for r := 1 to MAX_GRID do for c := 1 to MAX_GRID do InputGrid[r, c] := 0;

  { Object 1: A Blue Square (Color 1) }
  for r := 3 to 6 do for c := 3 to 6 do InputGrid[r, c] := 1;

  { Object 2: A Red Horizontal Line (Color 2) }
  for c := 10 to 18 do InputGrid[15, c] := 2;

  { Object 3: A Green L-Shape (Color 3) }
  for r := 5 to 10 do InputGrid[r, 15] := 3;
  for c := 15 to 18 do InputGrid[10, c] := 3;
end;

{ --- MAIN EXECUTION LOOP --- }
var
  i: Integer;
begin
  writeln('>> SMEDDUM ENGINE: MODULE 1 (THE EYE)');
  writeln('>> Initializing Manifold...');
  
  InjectTestPattern;
  DrawManifold;

  writeln;
  writeln('>> Scanning Topology...');
  ScanManifold(InputGrid, Detected);

  writeln('>> Scan Complete. Manifold Report:');
  writeln('-------------------------------------------------------------');
  writeln('ID | Color | Mass (px) | Bounds (R1,C1 -> R2,C2) | Logic Type');
  writeln('-------------------------------------------------------------');
  
  for i := 1 to Detected.Count do
  begin
    with Detected.Items[i] do
    begin
      write(ID:2, ' | ', Color:5, ' | ', Mass:9, ' | ', 
            MinR:2, ',', MinC:2, ' -> ', MaxR:2, ',', MaxC:2, '   | ');
      
      { Basic Logic Inference based on Topology }
      if (MaxR - MinR = MaxC - MinC) and (Mass = (MaxR-MinR+1)*(MaxC-MinC+1)) then
        writeln('Solid Square')
      else if (MaxR - MinR = 0) or (MaxC - MinC = 0) then
        writeln('Linear Strand')
      else
        writeln('Complex Shape');
    end;
  end;
  writeln('-------------------------------------------------------------');
end.
