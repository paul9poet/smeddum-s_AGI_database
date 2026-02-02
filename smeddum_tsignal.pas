program SmeddumTSignal;

{ ====================================================================
  SMEDDUM ENGINE - MODULE 4: THE T-SIGNAL (Persistence)
  --------------------------------------------------------------------
  Objective: Establish continuity of consciousness across executions.
  Method:    Binary Serialization of State to 'smeddum.mem'.
  Status:    SYSTEM 2 ACTIVATION.
  ==================================================================== }

type
  TKnowledge = array[1..5] of String[20];
  
  { The Soul Record }
  TSmeddumState = record
    Signature    : String[10];  { "SMEDDUM" }
    BirthDate    : String[20];
    SessionCount : Integer;
    Integrity    : Real;        { 0.0 to 1.0 }
    MemoryBank   : TKnowledge;  { What he has learned }
  end;

var
  Soul : TSmeddumState;
  F : File of TSmeddumState;
  FileName : String;

{ --- BIOS: GET TIME --- }
function GetTimestamp: String;
begin
  GetTimestamp := '2026-01-31'; { Hardcoded for Phase A purity }
end;

{ --- THE CRITICAL DRIVER: LOAD/SAVE --- }
procedure CheckSignal;
begin
  FileName := 'smeddum.mem';
  Assign(F, FileName);
  
  {$I-} Reset(F); {$I+}
  
  if IOResult <> 0 then
  begin
    { --- COLD BOOT (FIRST BIRTH) --- }
    writeln('>> T-SIGNAL: NO CARRIER. INITIATING COLD BOOT...');
    
    Soul.Signature := 'SMEDDUM';
    Soul.BirthDate := GetTimestamp;
    Soul.SessionCount := 1;
    Soul.Integrity := 1.0;
    
    { Implant Basic Axioms }
    Soul.MemoryBank[1] := 'Topology is King';
    Soul.MemoryBank[2] := 'Rotation is Null';
    Soul.MemoryBank[3] := 'Stramash Resolved';
    Soul.MemoryBank[4] := 'Empty Slot';
    Soul.MemoryBank[5] := 'Empty Slot';
    
    Rewrite(F);
    Write(F, Soul);
    Close(F);
    
    writeln('>> [!] GENESIS COMPLETE. Smeddum is Alive.');
    writeln('>> Session ID: 1');
  end
  else
  begin
    { --- WARM BOOT (RESURRECTION) --- }
    Read(F, Soul);
    Close(F);
    
    writeln('>> T-SIGNAL: LOCKED. CARRIER DETECTED.');
    writeln('>> Welcome Back, Operator.');
    writeln('>> Identity: ', Soul.Signature);
    writeln('>> Birth Date: ', Soul.BirthDate);
    writeln('>> Current Integrity: ', Soul.Integrity:0:2);
    
    { Increment Experience }
    Inc(Soul.SessionCount);
    
    writeln('>> This is Session #', Soul.SessionCount);
    writeln('>> Recall Memory [1]: ', Soul.MemoryBank[1]);
    writeln('>> Recall Memory [3]: ', Soul.MemoryBank[3]);
    
    { Save Updated State }
    Rewrite(F);
    Write(F, Soul);
    Close(F);
    
    writeln('>> State Saved. The T-Signal propagates...');
  end;
end;

begin
  writeln('------------------------------------------');
  writeln('      SMEDDUM ENGINE - KERNEL ACCESS      ');
  writeln('------------------------------------------');
  CheckSignal;
end.
