program sim_species;
{$MODE OBJFPC}
{$H+}
uses SysUtils;

const SIZE = 5;
type TGrid = array[0..SIZE-1, 0..SIZE-1] of Integer;

var
  TestGrid: TGrid;
  r, c, b1: Integer;

begin
  writeln('--- SMEDDUM: GESTALT SPECIES DETECTOR ---');
  FillChar(TestGrid, SizeOf(TestGrid), 0);
  
  { Create an "Eye" Species - A ring of pixels (Betti-1 = 1) }
  for r := 1 to 3 do begin
    TestGrid[r, 1] := 1; TestGrid[r, 3] := 1;
    TestGrid[1, r] := 1; TestGrid[3, r] := 1;
  end;

  { Simple Betti-1 logic: If center is empty but surrounded, it is an EYE }
  if (TestGrid[2,2] = 0) and (TestGrid[1,2] = 1) then b1 := 1 else b1 := 0;

  writeln('Manifold Analysis:');
  if b1 = 1 then 
    writeln('SPECIES IDENTIFIED: [1, 1] - THE EYE')
  else 
    writeln('SPECIES IDENTIFIED: [1, 0] - THE SOLID');
    
  writeln('STATUS: Gestalt Primitive Registered.');
end.
