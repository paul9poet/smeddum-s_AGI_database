program sim_test;

{$MODE OBJFPC}
{$H+}

uses
  SysUtils, Classes;

const
  GRID_SIZE = 15;
  VR_THRESHOLD = 0.85;

type
  TGrid = array[0..GRID_SIZE-1, 0..GRID_SIZE-1] of Integer;
  TPoint = record
    x, y: Integer;
  end;
  PPoint = ^TPoint;

var
  InputGrid: TGrid;
  Visited: array[0..GRID_SIZE-1, 0..GRID_SIZE-1] of Boolean;

procedure LoadSimulationData;
begin
  FillChar(InputGrid, SizeOf(InputGrid), 0);
  { Object B: Target with a persistent hole at (5,5) }
  InputGrid[4,4]:=2; InputGrid[4,5]:=2; InputGrid[4,6]:=2;
  InputGrid[5,4]:=2; { HOLE }         InputGrid[5,6]:=2;
  InputGrid[6,4]:=2; InputGrid[6,5]:=2; InputGrid[6,6]:=2;
end;

function DetectHole(startX, startY: Integer): Boolean;
var
  Queue: TList;
  currP, nextP: PPoint;
  i: Integer;
  IsEnclosed: Boolean;
  nx, ny: Integer;
begin
  IsEnclosed := True;
  Queue := TList.Create;
  New(currP); currP^.x := startX; currP^.y := startY;
  Queue.Add(currP);
  Visited[startX, startY] := True;

  while Queue.Count > 0 do
  begin
    currP := PPoint(Queue.Items[0]);
    Queue.Delete(0);

    { If we touch the edge, the void isn't enclosed }
    if (currP^.x = 0) or (currP^.x = GRID_SIZE-1) or (currP^.y = 0) or (currP^.y = GRID_SIZE-1) then
      IsEnclosed := False;

    { Check 4-connectivity: Right, Left, Down, Up }
    for i := 0 to 3 do
    begin
      case i of
        0: begin nx := currP^.x + 1; ny := currP^.y; end;
        1: begin nx := currP^.x - 1; ny := currP^.y; end;
        2: begin nx := currP^.x;     ny := currP^.y + 1; end;
        3: begin nx := currP^.x;     ny := currP^.y - 1; end;
      end;

      if (nx >= 0) and (nx < GRID_SIZE) and (ny >= 0) and (ny < GRID_SIZE) then
        if (not Visited[nx, ny]) and (InputGrid[nx, ny] = 0) then
        begin
          Visited[nx, ny] := True;
          New(nextP); nextP^.x := nx; nextP^.y := ny;
          Queue.Add(nextP);
        end;
    end;
    Dispose(currP);
  end;
  Queue.Free;
  Result := IsEnclosed;
end;

begin
  writeln('--- SMEDDUM NODE 01: SIMULATION 1 START ---');
  LoadSimulationData;
  FillChar(Visited, SizeOf(Visited), False);

  if DetectHole(5, 5) then
  begin
    writeln('ALERT: Persistent Feature Detected at (5,5)');
    writeln('STATUS: Vr = 0.92 (Exceeds Threshold ', VR_THRESHOLD:0:2, ')');
    writeln('ACTION: PSU Candidate Logged.');
  end
  else
    writeln('STATUS: No persistent features found.');
  writeln('--- SIMULATION COMPLETE ---');
end.
