unit DBConnection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqlite3conn, sqldb, DB, FileUtil, Forms, Controls,
  Graphics, Dialogs, DBGrids, StdCtrls, book, booktype, rental, student;

type
  ArrayOfStudents = array of TStudent;

  { TForm1 }
  TDBConnection = class
  public
    function getAllStudents: ArrayOfStudents;
    constructor Create;
  private
    DataSource: TDataSource;
    DBGrid: TDBGrid;
    SQLite3Connection: TSQLite3Connection;
    SQLQuery: TSQLQuery;
    SQLTransaction: TSQLTransaction;
  end;

{var
  DBConn: DBConnection;}

implementation

{ TForm1 }

function TDBConnection.getAllStudents: ArrayOfStudents;
begin
  SQLQuery.Close;
  SQLQuery.SQL.Text := 'SELECT * FROM student';
  SQLQuery.Open;

  Result:=nil;

  try
    with SQLQuery do
    begin
      First;
      while not EOF do
      begin
        //new row

        setLength(Result, length(Result));
        Result[length(Result) - 1].setId(FieldByName('id').AsLongint);
        Next;
      end;
    end;

  finally
    //nix? todo
  end;
end;

{ TForm1 }
constructor TDBConnection.Create;
begin
  self.SQLite3Connection := TSQLite3Connection.Create(nil);
  self.SQLTransaction := TSQLTransaction.Create(nil);
  self.SQLQuery := TSQLQuery.Create(nil);

  self.SQLite3Connection.DatabaseName := '../buchverleih.sqlite';
  self.SQLite3Connection.Transaction := self.SQLTransaction;

  self.SQLTransaction.Database := self.SQLite3Connection;

  self.SQLQuery.Database := self.SQLite3Connection;
  self.SQLQuery.Transaction := self.SQLTransaction;

  self.SQLite3Connection.Open;
  if self.SQLite3Connection.Connected then
  begin
    ShowMessage('connected -great');
  end;
end;

end.
