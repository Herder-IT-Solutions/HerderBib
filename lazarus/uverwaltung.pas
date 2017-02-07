unit uVerwaltung;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqlite3conn, sqldb;

type TVerwaltung = class
  //Methoden
  public
    //Erg: Initialisierung der Datenbankverwaltung
    constructor create(qy :TSQLQuery; ta : TSQLTransaction; cn : TSQLite3Connection);

    //Vor: Eine Buch Id
    //Eff: Überprüft, ob eine Buch Id bereits vergeben ist
    //Erg: Wahr, wenn vergeben
    function BIdPruef(BId :Cardinal):Boolean;

    //Vor: Eine Buch Id
    //Eff: Überprüft die Buchqualität
    //Erg: Die ehemalige Quaität
    function BQualiPruef(BId: Cardinal): Cardinal;

    //Vor: Die ISBN
    //Eff: Prüft den Buchtyp
    //Erg: Wahr, wenn isbn bereits vorhanden
    function BTypePruef (isbn:String): Boolean;

    //Vor: Eine Schüler Id
    //Eff: Überprüft, ob eine Schüler Id bereits vergeben ist
    //Erg: Wahr, wenn vergeben
    function SIdPruef(SId :Cardinal):Boolean;

    //Vor: Buch Id und Schüler Id
    //Eff: Rückgabe eines Buches mit Schüler
    //Erg: Trägt aktuelles Datum (als Double (nicht lesbar)) als Rückgabedatum in die Datenbank ein
    procedure BuchAusSchueler(BId, SId :Cardinal);

    //Vor: ISBN nur mit Zahlen
    //Eff: Hinzufügen eines neuen Buches
    //Erg: Buch in Tabelle book
    procedure BuchHinzu(isbn : String);

    //Vor: Die Buch Id
    //Eff: Löscht ein Buch aus dem Bestand
    procedure BuchLoesch(BId:Cardinal);

    //Vor: Buch Id und seine Qualität
    //Eff: Ändert die Buchqualität
    //Erg: Trägt übergebene Qualität in die Datenbank ein
    procedure BuchQualiAend(BId, quali :Cardinal);

    //Vor: isbn nur mit Zahlen, Titel und Fach des Buches, isbn darf nicht existieren
    //Eff: Neuer Buchtyp
    procedure BuchTypHinzu(isbn :String; title, subject :String);

    //Vor: Buch Id und Schüler Id
    //Eff: Neue Vergabe eines Buches
    //Erg: Trägt in procedure BuchTypHinzu(isbn, title, subject); der Datenbank in Tabelle rental das ausgeliehene Buch zu dem Schüler ein
    procedure BuchZuSchueler(BId, SId :Cardinal);

    //Vor: Nachname, Vorname und Klassenname, Geburtsdatum als yyyymmtt (20101104)
    //Eff: Neuen Schüler erstellen
    //Erg: Neuer Schüler
    procedure NewStudent (lastN, firstN, classN : String; birth: Cardinal);

    //Vor: Eine Schüler Id
    //Eff: Löscht einen Schüler
    procedure SchueLoesch(SId : Cardinal);

    //Vor: Eine Datum, bis wohin der Verlauf des Verleihs gelöscht werden soll
    //Eff: Löscht jeden Verleih, welches Rückgabedatum kleiner gleich ist als das Datum
    procedure VerleihLoesch(datum: TDate);  //bzw TDateTime

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

  query := qy;    //Query    (
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

function TVerwaltung.BQualiPruef(BId: Cardinal): Cardinal;
Var ret : Cardinal;
begin
  ret:=9;
  query.Close;
  query.SQL.text:='Select condition from book where id = :Id';
  query.ParamByName('Id').AsInteger:=BId;
  query.Open;
  if not query.EOF then ret:=query.Fields[0].AsInteger;
  result:=ret;
end;

function TVerwaltung.BTypePruef (isbn:String): Boolean;
Var ret : Boolean;
begin
  ret:=false;
  query.Close;
  query.SQL.text:='Select isbn from booktype where isbn = :ISBN';
  query.ParamByName('ISBN').AsString:=isbn;
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

procedure TVerwaltung.BuchAusSchueler(BId, SId :Cardinal);
begin
  query.Close;
  query.SQL.text:='Update rental Set return_date = :Datum where student_id = :SId and book_id = :BId';
  query.ParamByName('Datum').AsDate:= now;                 //Vorher: FormatDateTime(..)
  query.ParamByName('BId').AsInteger:=BId;
  query.ParamByName('Sid').AsInteger:=SId;
  query.ExecSQL;
  tran.Commit;

end;

procedure TVerwaltung.BuchHinzu(isbn : String);
Var id: Cardinal;
begin
  repeat
     id:= Random(5000000) + 23000001; //Bereich von 23Mio1 bis 28Mio1
  until BIdPruef(id)=false;            //Wiederholung bis id nicht vergeben

  query.Close;
  query.SQL.Text:='Insert into book Values (:Id, :isbn, :con)';
  query.ParamByName('Id').AsInteger:=id;
  query.ParamByName('isbn').AsString:=isbn;
  query.ParamByName('con').AsInteger:=1;
  query.ExecSQL;
  tran.Commit;
end;

procedure TVerwaltung.BuchLoesch(BId:Cardinal);
begin
  query.Close;
  query.SQL.Text:='Delete from book where Id = (:BId)';
  query.ParamByName('BId').AsInteger:=BId;
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

procedure TVerwaltung.BuchTypHinzu(isbn, title, subject :String);
begin
  query.Close;
  query.SQL.Text:='Insert into booktype Values (:isbn, :title, :sub, NULL)';
  query.ParamByName('isbn').AsString:=isbn;
  query.ParamByName('title').AsString:=title;
  query.ParamByName('sub').AsString:=subject;
  query.ExecSQL;
  tran.Commit;
end;

procedure TVerwaltung.BuchZuSchueler(BId, SId :Cardinal);
begin
  query.Close;
  query.SQL.text:='Insert into rental Values(:BId, :SId, NULL, :Datum)';
  query.ParamByName('Datum').AsDate:=now;         //FormatDateTime('yyyy-mm-dd',
  query.ParamByName('BId').AsInteger:=BId;
  query.ParamByName('Sid').AsInteger:=SId;
  query.ExecSQL;
  tran.Commit;

end;


procedure TVerwaltung.NewStudent (lastN, firstN, classN : String; birth: Cardinal);
Var id: Cardinal;
begin
  repeat
     id:= Random(2000000) + 21000000; //Bereich von 21Mio bis 23 Mio
  until SIdPruef(id)=false;            //Wiederholung bis id nicht vergeben

  query.Close;
  query.SQL.Text:='Insert into student Values (:Id, :lastN, :firstN, :classN, :birth)';
  query.ParamByName('Id').AsInteger:=id;
  query.ParamByName('lastN').AsString:=lastN;
  query.ParamByName('firstN').AsString:=firstN;
  query.ParamByName('classN').AsString:=classN;
  query.ParamByName('birth').AsDate:=birth;
  query.ExecSQL;
  tran.Commit;
end;

procedure TVerwaltung.SchueLoesch(SId : Cardinal);
begin
  query.Close;
  query.SQL.Text:='Delete from student where Id = (:SId)';
  query.ParamByName('SId').AsInteger:=SId;
  query.ExecSQL;
  tran.Commit;
end;

procedure TVerwaltung.VerleihLoesch(datum: TDate);
begin
  query.Close;
  query.SQL.Text:='Delete from rental where return_date <= (:date)';
  query.ParamByName('date').AsDate:=datum;
  query.ExecSQL;
  tran.Commit;
end;

end.

