program sim_rotate;

{$MODE OBJFPC}
{$H+}

uses SysUtils;

const
  SIZE = 3;

var
  InputGrid, OutputGrid: array[0..SIZE-1, 0..SIZE-1] of Integer;
  r, c: Integer;

begin
  writeln('--- SMEDDUM NODE 01: ROTATION TRAINING ---');

  { 1. Clear grids and Load an asymmetrical L-shape }
  FillChar(InputGrid, SizeOf(InputGrid), 0);
  InputGrid[0,0]:=4; 
  InputGrid[1,0]:=4; 
  InputGrid[2,0]:=4; InputGrid[2,1]:=4;

  { 2. Execute 90-degree Clockwise Rotation }
  { Formula: NewCol = (Size - 1) - OldRow; NewRow = OldCol }
  for r := 0 to SIZE-1 do
    for c := 0 to SIZE-1 do
      OutputGrid[c, (SIZE - 1) - r] := InputGrid[r, c];

  { 3. Display Results }
  writeln('Input (L-Shape):');
  for r := 0 to SIZE-1 do
  begin
    for c := 0 to SIZE-1 do write(InputGrid[r,c], ' ');
    writeln;
  end;

  writeln('Output (Rotated 90 Deg CW):');
  for r := 0 to SIZE-1 do
  begin
    for c := 0 to SIZE-1 do write(OutputGrid[r,c], ' ');
    writeln;
  end;

  { Verify the logic: The pixel at [2,1] should move to [1,0] }
  if OutputGrid[1,0] = 4 then
    writeln('STATUS: Rotation Parity Verified.')
  else
    writeln('STATUS: Geometric Failure.');
end.
