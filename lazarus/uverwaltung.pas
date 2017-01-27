unit uVerwaltung;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqlite3conn, sqldb;

type TVerwaltung = class
  //Methoden
  public
    //Erg: Initialisierung der Datenbankverwaltung       //fertig
    constructor create(qy :TSQLQuery; ta : TSQLTransaction; cn : TSQLite3Connection);

    //Vor: Eine Buch Id                                  //fertig
    //Eff: Überprüft, ob eine Buch Id bereits vergeben ist
    //Erg: Wahr, wenn vergeben
    function BIdPruef(BId :Cardinal):Boolean;

    //Vor: Die ISBN
    //Erg: Wahr, wenn isbn bereits vorhanden            //fertig
    function BTypePruef (isbn:Cardinal): Boolean;

    //Vor: Eine Schüler Id                                  //fertig
    //Eff: Überprüft, ob eine Schüler Id bereits vergeben ist
    //Erg: Wahr, wenn vergeben
    function SIdPruef(SId :Cardinal):Boolean;

    //Vor: Buch Id und Schüler Id                        //fertig
    //Eff: Rückgabe eines Buches mit Schüler
    //Erg: Trägt aktuelles Datum als Rückgabedatum in die Datenbank ein
    procedure BuchAusSchueler(BId, SId :Cardinal);

    //Vor: ISBN nur mit Zahlen
    //Eff: Hinzufügen eines neuen Buches
    //Erg: Buch in Tabelle book
    procedure BuchHinzu(isbn : Cardinal);

    //Vor: Buch Id und seine Qualität                   //fertig
    //Erg: Trägt übergebene Qualität in die Datenbank ein
    procedure BuchQualiAend(BId, quali :Cardinal);

    //Vor: isbn nur mit Zahlen, Titel und Fach dews Buches
    //Eff: Neuer Buchtyp
    procedure BuchTypHinzu(isbn :Cardninal; title, subject :String);

    //Vor: Buch Id und Schüler Id
    //Eff: Neue Vergabe                                   //fertig
    //Erg: Trägt in procedure BuchTypHinzu(isbn, title, subject); der Datenbank in Tabelle rental das ausgeliehene Buch zu dem Schüler ein
    procedure BuchZuSchueler(BId, SId :Cardinal);

    //Vor: Nachname, Vorname und Klassenname              //fertig
    //Eff: Neuen Schüler erstellen
    //Erg: Neuer Schüler in bTabelle student mit zufälliger Id
    procedure NewStudent (lastN, firstN, classN : String);

  //Atribute
  private
    query : TSQLQuery;             //Query
    tran  : TSQLTransaction;       //Transaction
    conn  : TSQLite3Connection;    //Connection

end;

implementation

constructor TVerwaltung.create(qy :TSQLQuery; ta : TSQLTransaction; cn : TSQLite3Connection);
begin
  //Initialisierung

  query := qy;    //Query
  tran  := ta;    //Transaction
  conn  := cn;    //Connection

  conn.DatabaseName:='buchverleih.sqlite';
  tran.Database:=conn;
  query.Transaction:=tran;

  Randomize;
end;

function TVerwaltung.BIdPruef(BId :Cardinal):Boolean;
Var ret : Boolean;
begin
  ret:=false;
  query.Close;
  query.SQL.text:='Select id from book where id = :Id';
  query.ParamByName('Id').AsInteger:=BId;
  query.Open;
  if not query.EOF then ret:=true;
  result:=ret;
end;

function TVerwaltung.SIdPruef(SId :Cardinal):Boolean;
Var ret : Boolean;
begin
  ret:=false;
  query.Close;
  query.SQL.text:='Select id from student where id = :Id';
  query.ParamByName('Id').AsInteger:=SId;
  query.Open;
  if not query.EOF then ret:=true;
  result:=ret;
end;

function TVerwaltung.BTypePruef (isbn:Cardinal): Boolean;
Var ret : Boolean;
begin
  ret:=false;
  query.Close;
  query.SQL.text:='Select isbn from booktype where isbn = :ISBN';
  query.ParamByName('ISBN').AsInteger:=isbn;
  query.Open;
  if not query.EOF then ret:=true;
  result:=ret;
end;

procedure TVerwaltung.BuchAusSchueler(BId, SId :Cardinal);
begin
  query.Close;
  query.SQL.text:='Update rental Set return_date = :Datum where student_id = :SId and book_id = :BId';
  query.ParamByName('Datum').AsDate:=FormatDateTime('yyyy-mm-dd', now);
  query.ParamByName('BId').AsInteger:=BId;
  query.ParamByName('Sid').AsInteger:=SId;
  query.ExecSQL;
  tran.Commit;

end;

procedure TVerwaltung.BuchHinzu(isbn : Cardinal);
Var id: Cardinal;
begin
  repeat
     id:= Random(50000000) + 30000000; //Bereich von 30Mio bis 80 Mio
  until BIdPruef(id)=false;            //Wiederholung bis id nicht vergeben

  query.Close;
  query.SQL.Text:='Insert into book Values (:Id, :isbn, :con)';
  query.ParamByName('Id').AsInteger:=id;
  query.ParamByName('isbn').AsInteger:=isbn;
  query.ParamByName('con').AsInteger:=1;
  query.ExecSQL;
  tran.Commit;
end;

procedure TVerwaltung.BuchQualiAend(BId, quali :Cardinal);
begin
  query.Close;
  query.SQL.text:='Update book Set condition = :Quali where id = :BId';
  query.ParamByName('Quali').AsInteger:=quali;
  query.ParamByName('BId').AsInteger:=BId;
  query.ExecSQL;
  tran.Commit;
end;

procedure TVerwaltung.BuchTypHinzu(isbn :Cardinal; title, subject :String);
begin
  query.Close;
  query.SQL.Text:='Insert into booktype Values (:isbn, :title, :sub)';
  query.ParamByName('isbn').AsInteger:=isbn;
  query.ParamByName('title').As:=isbn;
  query.ParamByName('con').AsInteger:=1;
  query.ExecSQL;
  tran.Commit;
end;

procedure TVerwaltung.BuchZuSchueler(BId, SId :Cardinal);
begin
  query.Close;
  query.SQL.text:='Insert into rental Values(:BId, :SId, NULL, :Datum)';
  query.ParamByName('Datum').AsDate:=FormatDateTime('yyyy-mm-dd', now);
  query.ParamByName('BId').AsInteger:=BId;
  query.ParamByName('Sid').AsInteger:=SId;
  query.ExecSQL;
  tran.Commit;

end;

procedure TVerwaltung.NewStudent (lastN, firstN, classN : String);
Var id: Cardinal;
begin
  repeat
     id:= Random(10000000) + 10000000; //Bereich von 10Mio bis 20 Mio
  until SIdPruef(id)=false;            //Wiederholung bis id nicht vergeben

  query.Close;
  query.SQL.Text:='Insert into student Values (:Id, :lastN, :firstN, classN)';
  query.ParamByName('Id').AsInteger:=id;
  query.ParamByName('lastN').AsInteger:=lastN;
  query.ParamByName('firstN').AsInteger:=firstN;
  query.ParamByName('classN').AsInteger:=classN;
  query.ExecSQL;
  tran.Commit;
end;


end.

