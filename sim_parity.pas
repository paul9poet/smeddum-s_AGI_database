program sim_parity;

{$MODE OBJFPC}
{$H+}

uses SysUtils;

const
  GRID_SIZE = 10;

var
  Manifold: array[0..GRID_SIZE-1, 0..GRID_SIZE-1] of Integer;
  StoredChecksum: Integer;
  r, c: Integer;

function CalculateCurrentChecksum: Integer;
var
  sum, tr, tc: Integer;
begin
  sum := 0;
  for tr := 0 to GRID_SIZE-1 do
    for tc := 0 to GRID_SIZE-1 do
      if Manifold[tr, tc] <> 0 then Inc(sum);
  Result := sum;
end;

begin
  writeln('--- SMEDDUM NODE 01: MANIFOLD PARITY CHECK ---');
  
  { 1. Load Data (2x2 Square) }
  Manifold[2,2] := 2; Manifold[2,3] := 2;
  Manifold[3,2] := 2; Manifold[3,3] := 2;
  
  { 2. Commit the Checksum }
  StoredChecksum := CalculateCurrentChecksum;
  writeln('CHECKPOINT: Parity Anchor set at ', StoredChecksum);

  { 3. Simulate a "Glitch" (Bit Flip) }
  // Uncomment the next line to see the system catch a corruption error:
  // Manifold[5,5] := 9; 

  { 4. Integrity Verification }
  if CalculateCurrentChecksum = StoredChecksum then
    writeln('STATUS: Integrity Verified. Manifold is Rock Solid.')
  else
  begin
    writeln('CRITICAL: PARITY ERROR DETECTED.');
    writeln('ACTION: Memory Wipe Triggered. System Locked.');
  end;
end.
