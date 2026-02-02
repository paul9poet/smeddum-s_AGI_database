program SmeddumAlgebra;

{ ====================================================================
  SMEDDUM ENGINE - MODULE 4: THE ALCHEMIST (Logic/Mutation)
  --------------------------------------------------------------------
  Objective: Apply a conditional transformation rule to the manifold.
  Scenario:  "Contact Infection" (Blue + Red Neighbor -> Green).
  Equation:  f(x) = If (x=Blue) AND (HasRedNeighbor(x)) Then Green Else x
  ==================================================================== }

const
  SIZE = 10;
  C_VOID = 0;
  C_BLUE = 1; { Healthy }
  C_RED  = 2; { Virus }
  C_GRN  = 3; { Mutated }

type
  TGrid = array[1..SIZE, 1..SIZE] of Integer;

var
  InputState, OutputState : TGrid;

{ --- UTILITY: DRAW --- }
procedure DrawGrid(Name: String; var G: TGrid);
var r, c: Integer;
begin
  writeln('   State: ', Name);
  writeln('   +--------------------+');
  for r := 1 to SIZE do begin
    write('   | ');
    for c := 1 to SIZE do begin
      case G[r, c] of
        C_VOID: write('. ');
        C_BLUE: write('1 '); { Blue }
        C_RED : write('2 '); { Red }
        C_GRN : write('3 '); { Green }
      end;
    end;
    writeln('|');
  end;
  writeln('   +--------------------+');
end;

{ --- SETUP: THE PETRI DISH --- }
procedure InitScenario;
var r, c: Integer;
begin
  { Fill with Void }
  for r := 1 to SIZE do for c := 1 to SIZE do InputState[r, c] := C_VOID;

  { Create a Block of Blue Cells (Healthy Tissue) }
  for r := 3 to 7 do for c := 3 to 7 do InputState[r, c] := C_BLUE;

  { Insert a Single Red Virus in the Center }
  InputState[5, 5] := C_RED;
end;

{ --- THE LOGIC KERNEL --- }
function HasRedNeighbor(r, c: Integer; var G: TGrid): Boolean;
var k, nr, nc: Integer;
    dr: array[1..4] of Integer = (-1, 1, 0, 0);
    dc: array[1..4] of Integer = (0, 0, -1, 1);
begin
  HasRedNeighbor := False;
  for k := 1 to 4 do begin
    nr := r + dr[k]; nc := c + dc[k];
    if (nr>=1) and (nr<=SIZE) and (nc>=1) and (nc<=SIZE) then
      if G[nr, nc] = C_RED then Exit(True);
  end;
end;

procedure ApplyMutation;
var r, c: Integer;
begin
  writeln('>> LOGIC KERNEL: Applying Rule [Contact -> Mutate]...');
  
  { Copy Input to Output first (State persistence) }
  OutputState := InputState;

  { Iterate Manifold }
  for r := 1 to SIZE do begin
    for c := 1 to SIZE do begin
      
      { THE ALGEBRA: If Blue AND Touching Red -> Become Green }
      if (InputState[r, c] = C_BLUE) and (HasRedNeighbor(r, c, InputState)) then
      begin
        OutputState[r, c] := C_GRN;
      end;
      
    end;
  end;
end;

begin
  writeln('>> SMEDDUM ALGEBRA: CONTEXTUAL LOGIC');
  InitScenario;
  DrawGrid('INPUT (T=0)', InputState);
  
  ApplyMutation;
  
  DrawGrid('OUTPUT (T=1)', OutputState);
  writeln('>> Mutation Complete. The Virus has interacted with the Host.');
end.
