program stavekontrol;

{$mode Delphi}

uses
  Classes, Sysutils, Process;

function FixWord(var Wd: string): Boolean;
var
  R: ansistring;
  RS, Suggestions: string;
  SL: TStrings;
begin
  Result := RunCommand('/bin/bash', ['-c', 'echo "' + Wd + '" | aspell -a -d da -l da'], R);
  RS := string(R);
  Result := Pos(#10 + '*', RS) > 0; 
  // If found, then we don't need a suggestion
  if Result then
    exit;
  Result := not (Pos(#10 + '#', RS) > 0);
  // If found, then we got no suggestion
  if not Result then
    exit;
  Suggestions := Copy(RS, Pos('0: ', RS)+3, Length(RS) - Pos('0: ', RS)-3);
  SL := TStringList.Create;
  try
    SL.StrictDelimiter := False;
    SL.CommaText := Suggestions;
    Wd := SL.Strings[Random(SL.Count-1)];
  finally
    SL.Free;
  end;
end;

var
  W, Wd, NewS: string;
  S: ansistring;
  I: Integer;
  SL: TStrings;
begin
  W := GetEnvironmentVariable('EGGS_USER');
  if W = '' then
    exit;
  if not RunCommand('lastmsg', [W], S) then
  begin
    S := ParamStr(1);
    if S = '' then
      exit;
  end;
  if S = '' then
    exit;
 
  SL := TStringList.Create;
  try
    SL.Delimiter := ' ';
    SL.DelimitedText := S;
    for I := 0 to SL.Count - 1 do
    begin
      Wd := SL.Strings[I];
      while not FixWord(Wd) do
      begin
        // Oh no, let's fix the word.
        Delete(Wd, Random(Integer(Length(Wd))+1), 1);
      end;
      SL.Strings[I] := Wd;
    end;
    NewS := SL.DelimitedText;
  finally
    SL.Free;
  end;
  Writeln(NewS);
end.
  
  
