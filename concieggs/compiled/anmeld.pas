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
  rs: array[0..23] of TReview =
  (
    // Brainfuck
    (Line: 'Alle prutter jo bare på hinanden.'; Stars: 0; Author: 'Brainfuck'),
    // 1
    (Line: 'Det værste lort nogensinde.'; Stars: 1; Author: 'Arkimedes fra Syrakus'),
    (Line: 'The worst thing since the bagpipes.'; Stars: 1; Author: 'Mr. Plinkett'),
    (Line: 'Det er et los i bollerne og en kniv i hjertet.'; Stars: 1; Author: 'Oscar Davidsen'),
    (Line: 'Den bedste måde at vinde på; undgå det som pesten!'; Stars: 1; Author: 'Sun Tzu'),
    (Line: 'I like to remember what my father taught me; that at one point or another, we were all new to this world.'; Stars: 1; Author: 'Dolores Abernathy'),
    // 2
    (Line: 'Jeg mener, at det er moderat sjovt.'; Stars: 2; Author: 'Sword_Smith'),
    (Line: 'Better than my first ex-wife.'; Stars: 2; Author: 'King Henry VIII'),
    (Line: '2 skinker for 1 skinkes pris!'; Stars: 2; Author: 'Ekstrabladet'),
    (Line: 'Slipset var ret sjovt.'; Stars: 2; Author: 'Politiken'),
    // 3
    (Line: 'Alle er Hitler.'; Stars: 3; Author: 'Stalin'),
    (Line: 'Lige lidt mere forberedelsestid'; Stars: 3; Author: 'Bjarne Goldbæk'),
    (Line: 'Jeg følte ikke noget specielt.'; Stars: 3; Author: 'Nikkel'),
    (Line: 'Vi bliver alle gladere når vi accepterer det.'; Stars: 3; Author: 'George Orwell'),
    (Line: 'You can''t play God without being acquainted with the devil.'; Stars: 3; Author: 'Robert Ford'),
    // 4
    (Line: 'Udmærket.'; Stars: 4; Author: 'Niels'),
    (Line: 'Minder mig om dengang med Mona.'; Stars: 4; Author: 'Leonardo da Vinci'),
    (Line: 'Som en god kop morgenurin.'; Stars: 4; Author: 'Mohandas Karamchand Gandhi'),
    (Line: 'Stort set intet mangler her; det er dig der ikke kan værdsætte det.'; Stars: 4; Author: 'Marcus Aurelius'),
    // 5
    (Line: 'Det var sjovt.'; Stars: 5; Author: 'Spectrum'),
    (Line: 'Hvis pik er Gud er det her Jesus.'; Stars: 5; Author: 'Carl-Mar Møller'),
    (Line: 'You had me at ''Wes Andersson'''; Stars: 5; Author: 'The New York Times'),
    (Line: 'I choose to see the beauty.'; Stars: 5; Author: 'Dolores Abernathy'),
    (Line: 'Livet begynder her.'; Stars: 5; Author: 'Lao Tzu')
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

