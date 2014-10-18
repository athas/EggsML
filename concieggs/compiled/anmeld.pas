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
const
  rs: array[0..7] of TReview =
  (
    (Line: 'The worst thing since the bagpipes.'; Stars: 1; Author: 'Mr. Plinkett'),
    (Line: 'Det var sjovt.'; Stars: 5; Author: 'Spectrum'),
    (Line: 'Alle er Hitler.'; Stars: 3; Author: 'Stalin'),
    (Line: 'Udmærket.'; Stars: 4; Author: 'Niels'),
    (Line: 'Det værste lort nogensinde.'; Stars: 1; Author: 'Arkimedes fra Syrakus'),
    (Line: 'You had me at ''Wes Andersson'''; Stars: 5; Author: 'The New York Times'),
    (Line: 'Lige lidt mere forberedelsestid'; Stars: 3; Author: 'Bjarne Goldbæk'),
    (Line: 'Jeg følte ikke noget specielt.'; Stars: 3; Author: 'Nikkel')
  );
var
  prei, ri: Integer;
  I: Integer;
begin
  // Tilføj flere på et tidspunkt eller slå dem op på nettet.
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

