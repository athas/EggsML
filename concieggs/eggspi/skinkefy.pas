program Skinkefy;
{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

uses Classes, RegExpr;

const SkinkeFactor: Integer = 60;

type
  TSkinke = class(TObject)
  private
    Skinker: TStringList;
    IkkeSkinker: TStringList;
    function Skinkefy(ObjR: TRegExpr): string;
    function IsSkinke(Str: string): boolean;
    function IsNotSkinke(Str: string): boolean;
  public
    function ToSkinke: string;
    constructor Create;
    destructor Destroy; override;
    class function Skinke: string;
  end;

constructor TSkinke.Create;
begin
  Skinker := TStringList.Create;
  IkkeSkinker := TStringList.Create;
end;

destructor TSkinke.Destroy;
begin
  Skinker.Free;
  IkkeSkinker.Free;
end;

function TSkinke.IsSkinke(Str: string): Boolean;
var
  J: Integer;
begin
  Result := false;
  for J := 0 to Skinker.Count - 1 do begin
    if Skinker.Strings[J] = Str then
      Result := true;
  end;
end;

function TSkinke.IsNotSkinke(Str: string): boolean;
var
  J: Integer;
begin
  Result := false;
  for J := 0 to IkkeSkinker.Count - 1 do begin
    if IkkeSkinker.Strings[J] = Str then
      Result := true;
  end;
end;

function TSkinke.Skinkefy(ObjR: TRegExpr): string;
var
  Str: string;
begin
  Str := ObjR.Match[1];
  Result := Str;
  if ((Random(100) > SkinkeFactor) and not IsNotSkinke(str)) or IsSkinke(Str) then begin
    Skinker.Add(Str);
    Result := 'skinke';
  end else
    IkkeSkinker.Add(Str);
end;

function TSkinke.ToSkinke: string;
var
  S, Sb: String;
  wordSplitR: TRegExpr;
begin
  wordSplitR := TRegExpr.Create;
  try
    wordSplitR.Expression := '(\w+)';
    Randomize;
    while not eoln do begin
      // Read from stdin
      read(S);
      Sb := wordSplitR.ReplaceEx(S, Skinkefy);
      
      if Result <> '' then
        Result := Result + ' ';
      Result := Result + Sb;
    end;
  finally
    wordSplitR.Free;
  end;
end;

class function TSkinke.Skinke: string;
var
  ske: TSkinke;
begin
  ske := TSkinke.Create;
  try
    Result := ske.ToSkinke;
  finally
    ske.Free;
  end;
end;

begin
  writeln(TSkinke.Skinke);
end.
