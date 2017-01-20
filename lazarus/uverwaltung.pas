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

    //Vor: Buch Id und Schüler Id
    //Erg: Trägt in der Datenbank das ausgeleihte Buch zu dem Schüler ein
    procedure BuchZuSchueler(BId, SId :Cardinal);

    //Vor: Buch Id und Schüler Id
    //Erg: Trägt aktuelles Datum als Rückgabedatum in die Datenbank ein
    procedure  BuchAusSchueler(BId, SId :Cardinal);


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

end;

procedure TVerwaltung.BuchZuSchueler(BId, SId :Cardinal);
begin
  query.Close;
  query.SQL.text:='Insert into rental Values(:BId, :SId, NULL, :Datum)';
  query.ParamByName('Datum').AsString:=FormatDateTime('yyyy-mm-dd', now);
  query.ParamByName('BId').AsInteger:=BId;
  query.ParamByName('Sid').AsInteger:=SId;
  query.ExecSQL;
  tran.Commit;

end;

procedure TVerwaltung.BuchAusSchueler(BId, SId :Cardinal);
begin
  query.Close;
  query.SQL.text:='Update rental Set return_date = :Datum where student_id = :SId and book_id = :BId';
  query.ParamByName('Datum').AsString:=FormatDateTime('yyyy-mm-dd', now);
  query.ParamByName('BId').AsInteger:=BId;
  query.ParamByName('Sid').AsInteger:=SId;
  query.ExecSQL;
  tran.Commit;

end;

end.

