{$MODE DELPHI}
program lykkehjuletGoCommand;

uses sysutils;//, LazUtils, LazUTF8;

// Stolen from http://bjoern.hoehrmann.de/utf-8/decoder/dfa/
const UTF8_ACCEPT=0;
const UTF8_REJECT=1;

const utf8d: array[0..399] of Byte = (
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, // 00..1f
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, // 20..3f
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, // 40..5f
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, // 60..7f
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9, // 80..9f
  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7, // a0..bf
  8,8,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, // c0..df
  $a,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$3,$4,$3,$3, // e0..ef
  $b,$6,$6,$6,$5,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8,$8, // f0..ff
  $0,$1,$2,$3,$5,$8,$7,$1,$1,$1,$4,$6,$1,$1,$1,$1, // s0..s0
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,1,1,1,0,1,0,1,1,1,1,1,1, // s1..s2
  1,2,1,1,1,1,1,2,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1, // s3..s4
  1,2,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1,3,1,3,1,1,1,1,1,1, // s5..s6
  1,3,1,1,1,1,1,3,1,3,1,1,1,1,1,1,1,3,1,1,1,1,1,1,1,1,1,1,1,1,1,1  // s7..s8
);

type PLongword = ^Longword;

function UTF8Decode(State: PLongword; Codep: PLongword; By: Longword): Longword;
var
  Tp: Longword;
begin
  Tp := utf8d[By];

  if state^ <> UTF8_ACCEPT then
    codep^ := (By and cardinal($3f){u}) or (codep^ shl 6)
  else
    codep^ := ($ff shr Tp) and (By);

  state^ := utf8d[256 + (state^ * 16) + Tp];
  exit(state^);
end;

function UTF8Count(S: string): Integer;
var
  Count, I: Integer;
  State: Longword;
  CodePoint: Longword;
begin
  Count := 1;
  for I := 1 to Length(S) do
  begin
    if UTF8Decode(@State, @CodePoint, Longword(S[I])) = UTF8_ACCEPT then
      Inc(Count);
  end;
  
  exit(Count);
end;

function UTF8Copy(S: string; AIndex, ALength: Integer): string;
var
  FullCount, Count, I: Integer;
  StartIndex, RealLength: Integer;
  State: Longword;
  CodePoint: Longword;
begin
  FullCount := UTF8Count(S);
  if FullCount < AIndex then
    exit(S);
  
  Count := 1;
  StartIndex := -1;
  RealLength := -1;
  for I := 1 to Length(S) do
  begin
    if UTF8Decode(@State, @CodePoint, Longword(S[I])) = UTF8_ACCEPT then
      Inc(Count);
    if StartIndex = -1 then
    begin
      if Count = AIndex then
        StartIndex := I;
    end 
    else
    begin
      if Count = AIndex + ALength then
      begin
        RealLength := I - StartIndex;
        break;
      end;
    end;
  end;
  
  if RealLength = -1 then
    RealLength := Length(S) - StartIndex;
  
  exit(Copy(S, StartIndex, RealLength));
end;

var
  Command, Sentence, State, Guess: string;
  I: Integer;
  
begin
  Command  := ParamStr(1);
  Sentence := ParamStr(2);
  State    := ParamStr(3);
  WriteLn(UTF8Copy(Command, 1, 6));
  exit;
  if UTF8Copy(Command, 1, 6) = 'gæt på' then
  begin
    Guess := Copy(Command, 7, Length(Command)-7);
    if CompareText(Guess, Sentence) = 1 then
    begin
      WriteLn(Sentence);
      exit;
    end;
    
    if Length(Guess) = 1 then
    begin
      for I := 1 to Length(Sentence) do
      begin
        if CompareText(Guess, Sentence[I]) = 1 then
        begin
          State[I] := Guess[1];
        end;
      end;
      
      WriteLn(State);
      exit;
    end;
    
    WriteLn(State);
    exit;
  end;  
end.
