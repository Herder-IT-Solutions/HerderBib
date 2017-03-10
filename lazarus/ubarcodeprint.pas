unit uBarcodePrint;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, Process, fileutil, FPImage, FPCanvas, FPImgCanv,ShellApi,
     FPWritePNG;

{Diese Klasse ist als Singleton realisiert. Das bedeutet, es kann nur eine einzige Instanz von ihr geben.
 Auf diese wird über TBarcodePrinter.instance zugegriffen. Auf das zurückgegebene Objekt können
 alle im der Schnittstelle spezifizierten Methoden und Funktionen angewendet werden}
type TBarcodePrinter = class
     public

           class function instance : TBarcodePrinter; static;

            {Das n-te (von 1 an) Druckerblatt mehrmals verwenden, used gibt an, wie viele Stellen auf diesem Blatt schon benutzt wurden}
            procedure add_used_sheet(n : Cardinal; used: Cardinal);

            procedure add_barcode(code: Cardinal; title: String);

            procedure print;  //Alle hinzugefügten Barcodes drucken

            //Alle eingegebenen Daten löschen
            procedure clear;


    private
          constructor create;

          procedure generate_latex_code;
          procedure compile_latex_code;
          procedure generate_barcodes;
          procedure run_command(text : string);
          {Jede Seite enthält <<page_size>> Strichcodes, an manchen Stellen, erscheint aber kein Strichcode.
           Das ist der Fall, wenn eine Stelle auf dem Druckerpapier schon benutzt wurde, oder wenn
           das letzte Blatt nicht ganz mit Strichcodes gefüllt werden kann.
           Für beide Fälle müssen an die entsprechende Stelle in die Barcode- und in die Titelliste "leere Strichcodes" eingefügt werden}
          procedure fill_empty_spaces;

          procedure create_barcode(str: string);

          type TIntegerMap = specialize TFPGMap<Integer, Integer>;
          type TStringList = specialize TFPGList<String>;

          var unused_space_on_sheets : TIntegerMap;
          var titles, barcodes : TStringList;
end;

implementation

  var printer : TBarcodePrinter = nil;

  const Tab = #9;
  const NewLine = #13#10;

  //Druckeinstellungen
  const columns = 3;
  const column_size = 8;
  const page_size = columns * column_size;

  //Dateinamen und Pfade für die verwendeten Dateien

  const path = '.\BarcodePrinter\';
  const working_path = path + 'tmp\';

  const latex_file = 'latex.tex';
  const latex_output = 'latex.pdf';
  const empty_barcode = 'empty.png';

constructor TBarcodePrinter.create;
begin
     unused_space_on_sheets := TIntegerMap.create;
     barcodes := TStringList.create;
     titles := TStringList.create;
end;



class function TBarcodePrinter.instance: TBarcodePrinter;static;
begin
     if printer = nil then
        printer := TBarcodePrinter.create;

     result := printer;
end;



procedure TBarcodePrinter.add_used_sheet(n : Cardinal; used: Cardinal);
begin
     unused_space_on_sheets.add(n, used);
end;


procedure TBarcodePrinter.add_barcode(code: Cardinal; title: String);
begin
     //Fehlerbehandlung, falls Code falsche Länge hat, implementieren
     barcodes.add(IntToStr(code));
     titles.add(title);
end;



procedure TBarcodePrinter.print;
    var curr_page, i, j, empty : Integer;
    var code : Integer;
begin
     //Fehlerbehandlung, wenn Verzeichnis nicht erstellt werden kann?
     //Ein temporäres Arbeitsverzeichnis wird angelegt
     createDir(working_path);
     copyFile(path + empty_barcode, working_path + empty_barcode); //Den leeren Barcode brauchen wir auch im Arbeitsverzeichnis

     fill_empty_spaces;
     generate_barcodes;
     generate_latex_code;
     compile_latex_code;

     //pdf ausdrucken
     ShellExecute(0, 'print', PChar(working_path + latex_output), nil, nil, 0);
     //run_command(print_command + working_path + latex_output);
     //das Arbeitsverzeichnis wieder löschen
     deleteDirectory(working_path, false);

end;



procedure TBarcodePrinter.generate_latex_code;
    var latex : TextFile;
    var i : Cardinal;
    var barcode, title : String;
begin
     //Schreibfehler in Datei?
     //Datei für den Latex-Code erstellen
     AssignFile(latex, working_path + latex_file);
     rewrite(latex);

     writeln(latex, '\documentclass[10pt]{article}');
     writeln(latex, '\usepackage{graphicx}', NewLine, '\usepackage{multicol}');
     writeln(latex, '\usepackage[paper = a4paper, left = 5mm, right = 5mm, top = 5mm, bottom = 5mm]{geometry}');
     writeln(latex, '\begin{document}', NewLine, Tab, '\begin{multicols}{', columns, '}');

    for i := 0 to barcodes.count - 1 do
    begin
         barcode := barcodes.items[i];
         title := titles.items[i];


         writeln(latex, Tab, Tab, '\centering');
         //Die leeren Strichcodes beachten
         if barcode = '' then
         begin
            writeln(latex, Tab, Tab,'\includegraphics{empty}\\');
            writeln(latex, Tab, Tab, '\vspace{7pt}');
            writeln(latex, Tab, Tab,'\parbox[t][0.88cm]{134px}{\phantom{\Large{unsichtbar} \\ \Large{unsichtbar}}}\\');
         end
         else
         begin
             writeln(latex, Tab, Tab,'\includegraphics{', barcode, '}\\');
             writeln(latex, Tab, Tab, '\vspace{7pt}');
             writeln(latex, Tab, Tab,'\parbox[t][0.88cm]{134px}{\centering \Large{', barcode, '}\\\Large{', title, '}}\\');
         end;

         //Am Ende einer Spalte angelangt
         if ((i + 1) mod column_size = 0) and (i < barcodes.count - 1) then
         begin
            writeln(latex, Tab, Tab, '\vfill\null', NewLine,  Tab, Tab, '\columnbreak')
         end
         else
             writeln(latex, Tab, Tab,'\vspace{14pt}', NewLine);
    end;

    writeln(latex, Tab, '\end{multicols}');
    write(latex, '\end{document}');

    closeFile(latex);
end;



procedure TBarcodePrinter.generate_barcodes;
    var i : Integer;
begin
    for i := 0 to barcodes.count - 1 do
    begin
         if barcodes.items[i] = '' then continue;  //Für einen leeren Barcode muss nichts gedruckt werden
         create_barcode(barcodes.items[i]);
    end;
end;



procedure TBarcodePrinter.fill_empty_spaces;
    var curr_page, i, j, used : Integer;
        var code : Integer;
    begin
         for i := 0 to unused_space_on_sheets.count - 1 do
         begin
              curr_page := unused_space_on_sheets.keys[i];
              used := unused_space_on_sheets.data[i];

              //Das Listenelemente [page_size * (n - 1)] ist das erste Element auf der n-ten Seite

              //Die aktuelle Seite wird nicht zum Ausdrucken benötigt
              if page_size * (curr_page - 1) >= barcodes.count then
                 continue;

              //Am Beginn der aktuellen Seite die schon verbrauchten Plätze mit leeren Strichcodes auffüllen
              for j := 1 to used do
              begin
                   barcodes.insert(page_size * (curr_page -1), '');
                   titles.insert(page_size * (curr_page - 1), '');
              end;
         end;

         //Die Leeren Stellen auf der letzten Seite auffüllen
         while barcodes.count mod page_size <> 0 do
         begin
              barcodes.add('');
              titles.add('');
         end;

end;



procedure TBarcodePrinter.compile_latex_code;
begin
     run_command('cd ' + working_path + ' && pdflatex ' + latex_file);
end;


//Fehlerbehandlung wenn Befehl nicht ausgeführt werden konnte
procedure TBarcodePrinter.run_command(text : string);
var command : TProcess;
begin
     command := TProcess.create(nil);
     command.Options := command.Options + [poWaitOnExit];// + [poNoConsole];
     {$IFDEF LINUX}
     command.executable := '/bin/sh';
     command.parameters.add('-c');
     command.parameters.add(text);
     command.execute;
     {$ENDIF}

     {$IFDEF WIN32}
     command.executable := 'c:\windows\system32\cmd.exe';
     command.parameters.add('/C');
     command.parameters.add(text);
     command.execute;
     {$ENDIF}
end;

procedure TBarcodePrinter.clear;
begin
     barcodes.clear;
     titles.clear;
     unused_space_on_sheets.clear;
end;

procedure TBarcodePrinter.create_barcode(str: string);
const
    lCodes: array[0 .. 9, 0 .. 6] of boolean = ((False,False,False,True,True,False,True),
                                                (False,False,True,True,False,False,True),
                                                (False,False,True,False,False,True,True),
                                                (False,True,True,True,True,False,True),
                                                (False,True,False,False,False,True,True),
                                                (False,True,True,False,False,False,True),
                                                (False,True,False,True,True,True,True),
                                                (False,True,True,True,False,True,True),
                                                (False,True,True,False,True,True,True),
                                                (False,False,False,True,False,True,True));
    rCodes: array[0 .. 9, 0 .. 6] of boolean = ((True,True,True,False,False,True,False),
                                                (True,True,False,False,True,True,False),
                                                (True,True,False,True,True,False,False),
                                                (True,False,False,False,False,True,False),
                                                (True,False,True,True,True,False,False),
                                                (True,False,False,True,True,True,False),
                                                (True,False,True,False,False,False,False),
                                                (True,False,False,False,True,False,False),
                                                (True,False,False,True,False,False,False),
                                                (True,True,True,False,True,False,False));

    type TBarcode = array[0..66] of boolean;


var
    canvas : TFPCustomCanvas;
    image : TFPCustomImage;
    writer : TFPCustomImageWriter;
    barcode: TBarcode;
    i,j: cardinal;
begin
    image := TFPMemoryImage.Create (134, 50);
    canvas := TFPImageCanvas.Create (image);
    writer := TFPWriterPNG.Create;

    barcode[0] := True;
    barcode[1] := False;
    barcode[2] := True;

    for i := 0 to 3 do
    begin
      for j := 0 to 6 do
        barcode[7*i + j + 3] := lCodes[StrToInt(str[i + 1])][j]
    end;

    barcode[31] := False;
    barcode[32] := True;
    barcode[33] := False;
    barcode[34] := True;
    barcode[35] := False;

    for i := 4 to 7 do
    begin
      for j := 0 to 6 do
        barcode[7*i + j + 8] := rCodes[StrToInt(str[i + 1])][j]
    end;

    barcode[64] := True;
    barcode[65] := False;
    barcode[66] := True;

    canvas.Brush.FPColor:=colWhite;
    canvas.Rectangle(0,0,image.Width,image.Height);
    canvas.Brush.FPColor:=colBlack;

    for i := 0 to 66 do
    begin
      if barcode[i] then
        canvas.Rectangle(2*i,0,2*i+1,50);
    end;
  image.SaveToFile(working_path + str + '.png', writer);
  image.free;
end;


end.

