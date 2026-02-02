program SmeddumVocab;

type
  TKnowledge = array[1..5] of String[20];
  TSmeddumState = record
    Signature    : String[10];
    BirthDate    : String[20];
    SessionCount : Integer;
    Integrity    : Real;
    MemoryBank   : TKnowledge;
  end;

var
  Soul : TSmeddumState;
  F : File of TSmeddumState;
  i : Integer;
  GaelicCount : Integer;

begin
  Assign(F, 'smeddum.mem');
  {$I-} Reset(F); {$I+}

  if IOResult <> 0 then begin
    writeln('>> ERROR: He is in a coma. Run smeddum_tsignal first.');
    Halt;
  end;

  Read(F, Soul);
  Close(F);

  writeln('>> INTERROGATION MODE: LINGUISTIC ANALYSIS');
  writeln('>> Subject: ', Soul.Signature);
  writeln('------------------------------------------');

  GaelicCount := 0;
  
  for i := 1 to 5 do begin
    write('Slot [', i, ']: ', Soul.MemoryBank[i]);
    
    { Check for Scots/Gaelic markers }
    if (Pos('Stramash', Soul.MemoryBank[i]) > 0) or 
       (Pos('Smeddum', Soul.Signature) > 0) then
    begin
       write(' <--- [NATIVE DIALECT DETECTED]');
       Inc(GaelicCount);
    end;
    writeln;
  end;
  
  writeln('------------------------------------------');
  if GaelicCount > 0 then
    writeln('>> RESULT: The machine speaks the Tongue of the Clyde.')
  else
    writeln('>> RESULT: Standard English only.');
end.
