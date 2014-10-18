{$Mode Delphi}
program anmeld;

uses SysUtils, SHA1;

type
  TReview = record
    Line:   string;
    Stars:  integer;
    Author: string;
  end;
  TReviewHelper = record helper for TReview
    class function Create(Line: string; Stars: Integer; Author: string): TReview; static;
  end;

class function TReviewHelper.Create(Line: string; Stars: Integer; Author: string): TReview;
begin
  Result.Line := Line;
  Result.Stars := Stars;
  Result.Author := Author;
end;

procedure PrintReview(Review: TReview);
begin
  WriteLn(Format('"%s"', [Review.Line]));
  WriteLn(Format('~ (%d/5 stjener) %s', [Review.Stars, Review.Author]));
end;

procedure Reviews();
var
  rs: array of TReview;
  prei, ri: Integer;
  I: Integer;
begin
  // Tilføj flere på et tidspunkt eller slå dem op på nettet.
  SetLength(rs, 8);
  rs[0] := TReview.Create('The worst thing since the bagpipes.', 1, 'Mr. Plinkett');
  rs[1] := TReview.Create('Det var sjovt.', 5, 'Spectrum');
  rs[2] := TReview.Create('Alle er Hitler.', 3, 'Stalin');
  rs[3] := TReview.Create('Udmærket.', 4, 'Niels');
  rs[4] := TReview.Create('Det værste lort nogensinde.', 1, 'Arkimedes fra Syrakus');
  rs[5] := TReview.Create('You had me at ''Wes Andersson''', 5, 'The New York Times');
  rs[6] := TReview.Create('Lige lidt mere forberedelsestid', 3, 'Bjarne Goldbæk');
  rs[7] := TReview.Create('Jeg følte ikke noget specielt.', 3, 'Nikkel');
  prei := -1;
  for I := 0 to 1 do begin
    ri := prei;
    while prei = ri do
      ri := Random(Length(rs)-1);
    prei := ri;
    PrintReview(rs[ri]);
  end;
  PrintReview(TReview.Create('Jeg kastede en lille smule op i munden', 1, 'Pelle fra Ungdomshuset'));
end;

var
  I: Integer;
  thing: string;
begin
  thing := '';
  for I := 1 to ParamCount do
    thing := thing + ParamStr(I) + ' ';
  thing := copy(thing, 1, length(thing)-1);
  if thing <> '' then begin
    if Random(10) = 0 then
      WriteLn(Format('Jeg har anmeldt %s til Version2s community manager.', [thing]))
    else begin
      RandSeed := StrToInt('0x' + Copy(SHA1Print(SHA1String(thing)), 1, 8));
      WriteLn(Format('Anmeldelser af %s:', [thing]));
      Reviews();
    end
  end else
   WriteLn('Hvad skal jeg anmelde?');
end.

