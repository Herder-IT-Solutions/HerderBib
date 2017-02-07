unit DBConnection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqlite3conn, sqldb, DB, FileUtil, Forms, Controls,
  Graphics, Dialogs, DBGrids, StdCtrls, book, booktype, rental, student;

type
  ArrayOfStudents = array of TStudent;
  TIntegerList = TList<int64>;

  { TForm1 }
  DBConnection = class
  public
    function getAllStudents: ArrayOfStudents;
    constructor Create();
  private
    DataSource: TDataSource;
    DBGrid: TDBGrid;
    Label1: TLabel;
    SQLite3Connection: TSQLite3Connection;
    SQLQuery: TSQLQuery;
    SQLTransaction: TSQLTransaction;
  end;

var
  DBConn: DBConnection;

implementation

{$R *.lfm}

{ TForm1 }

function DBConnection.getAllStudents: ArrayOfStudents;
begin
  SQLQuery.Close;
  SQLQuery.SQL.Text := 'SELECT * FROM student';
  SQLQuery.Open;


  rowCount := 0;
  try
    with SQLQuery do
    begin
      First;
      while not EOF do
      begin
        //new row

        setLength(Result, length(Result));
        Result[length(Result) - 1].setId(FieldByName('id'));
        idList.Add(FieldByName('id'));
        Next;
      end;
    end;

  finally
    //nix? todo
  end;
end;

{ TForm1 }
constructor DBConnection.Create();
begin
  SQLite3Connection.DatabaseName := 'buchverleih.sqlite';
  SQLite3Connection.Transaction := SQLTransaction;

  SQLTransaction.Database := SQLite3Connection;

  SQLQuery.Database := SQLite3Connection;
  SQLQuery.Transaction := SQLTransaction;

  DataSource.dataset := SQLQuery;
  DBGrid.DataSource := DataSource;
  SQLite3Connection.Open;
  if SQLite3Connection.Connected then
  begin
    Label1.Caption := 'connected -great';
  end;
  SQLQuery.Open;
end;

end.
