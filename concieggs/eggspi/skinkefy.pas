program Skinkefy;
{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

uses Classes, RegExpr, SysUtils;

const
  // Chance of skinke in percentage
  SkinkeFactor: Integer = 25;
  // Required number of letters before turning it into skinke.
  SkinkeLength: Integer = 3;

type
  TSkinkeCondition = (scRegular, scCapitalised, scFullUpper);
  
  TSkinke = class(TObject)
  private
    Skinker: TStringList;
    IkkeSkinker: TStringList;
    function OneSkinke(Cnd: TSkinkeCondition; Multi: boolean): string;
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
  Str := UpperCase(Str);
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
  Str := UpperCase(Str);
  Result := false;
  for J := 0 to IkkeSkinker.Count - 1 do begin
    if IkkeSkinker.Strings[J] = Str then
      Result := true;
  end;
end;

function TSkinke.OneSkinke(Cnd: TSkinkeCondition; Multi: boolean): string;
begin
  if Multi then
    Result := 'skinker'
  else
    Result := 'skinke';
  case Cnd of
    scFullUpper:
      Result := UpperCase(Result);
    scCapitalised:
      Result[1] := 'S';
  end;
end;

function TSkinke.Skinkefy(ObjR: TRegExpr): string;
var
  Str: string;
  Cnd: TSkinkeCondition;
  camelCaseCheckR, camelCaseR: TRegExpr;
begin
  Str := ObjR.Match[1];
  Result := Str;
  camelCaseCheckR := TRegExpr.Create;
  camelCaseR := TRegExpr.Create;
  try
    camelCaseCheckR.Expression := '[a-z][A-Z][a-zA-Z]+';
    camelCaseR.Expression := '([A-Z][a-z]+|[A-Z]+)';
    if camelCaseCheckR.Exec(Str) then
      Result := camelCaseR.ReplaceEx(Str, Skinkefy)
    else begin
      if ((Random(100) <= SkinkeFactor)
          and not IsNotSkinke(str)
          and (Length(str) >= SkinkeLength))
        or IsSkinke(Str) then begin
        Skinker.Add(UpperCase(Str));
        if UpperCase(str) = str then
          Cnd := scFullUpper
        else if UpperCase(str[1]) = str[1] then
          Cnd := scCapitalised
        else
          Cnd := scRegular;
        Result := OneSkinke(Cnd, UpperCase(str[Length(str)]) = 'R');
      end else
        IkkeSkinker.Add(UpperCase(Str));
    end;
  finally
    camelCaseCheckR.Free;
    camelCaseR.Free;
  end;
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
