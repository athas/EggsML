program Skinkefy;
{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

uses Classes, RegExpr;

const SkinkeFactor: Integer = 60;

function Skinke: string;
var
  SkinkeContainer: TStringList;
  S, Sb: String;
  wordSplitR: TRegExpr;
  function IsSkinke(Str: string): Boolean;
  var
    J: Integer;
  begin
    Result := false;
    for J := 0 to SkinkeContainer.Count - 1 do begin
      if SkinkeContainer.Strings[J] = Str then
        Result := true;
    end;
  end;
  function Skinkefy(Str: string): string;
  begin
    Result := Str;
    if (Random(100) > SkinkeFactor) or IsSkinke(S) then begin
      SkinkeContainer.Add(Str);
      Result := 'skinke';
    end;
  end;
begin
  SkinkeContainer := TStringList.Create();
  wordSplitR := TRegExpr.Create;
  Try
    wordSplitR.Expression := '(\w+)';
    Randomize;
    while not eoln do begin
      read(S);
      Sb := wordSplitR.ReplaceEx(S, Skinkefy);
      
      if Result <> '' then
        Result := Result + ' ';
      Result := Result + Sb;
      Result := Result + S;
    end;
  Finally
    wordSplitR.Free;
    SkinkeContainer.Free;
  end;
end;

begin
  writeln(Skinke);
end.
