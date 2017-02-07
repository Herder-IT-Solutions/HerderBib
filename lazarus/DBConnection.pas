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
    function getStudents: ArrayOfStudents;
    function getStudentsByFirstNamePattern(firstName: String): ArrayOfStudents;
    function getStudentsByLastNamePattern(lastName: String): ArrayOfStudents;
    function getStudentsByClassName(className: String): ArrayOfStudents;
    constructor Create;
  private
    SQLite3Connection: TSQLite3Connection;
    SQLQuery: TSQLQuery;
    SQLTransaction: TSQLTransaction;
  end;

{var
  DBConn: DBConnection;}

implementation

{ TForm1 }

function TDBConnection.getStudents: ArrayOfStudents;
begin
  SQLQuery.Close;
  SQLQuery.SQL.Text := 'SELECT * FROM student';
  SQLQuery.Open;

  Result := nil;

  try
    with SQLQuery do
    begin
      First;
      while not EOF do
      begin
        //new row
        setLength(Result, length(Result) + 1);
        Result[length(Result) - 1] := TStudent.Create; //create new student object
        Result[length(Result) - 1].setId(FieldByName('id').AsLongint); //set id
        Result[length(Result) - 1].setLastName(FieldByName('last_name').ToString);
        Result[length(Result) - 1].setFirstName(FieldByName('first_name').AsString);
        Result[length(Result) - 1].setClassName(FieldByName('class_name').AsString);
        Result[length(Result) - 1].setBirth(FieldByName('birth').AsDateTime);
        Next;
      end;
    end;

  finally
    //nix? todo
  end;
end;

function TDBConnection.getStudentsByFirstNamePattern(firstName: String): ArrayOfStudents;
begin
  SQLQuery.Close;
  SQLQuery.SQL.Text := 'SELECT * FROM student WHERE first_name LIKE ''' + firstName + '''';
  SQLQuery.Open;

  Result := nil;

  try
    with SQLQuery do
    begin
      First;
      while not EOF do
      begin
        //new row
        setLength(Result, length(Result) + 1);
        Result[length(Result) - 1] := TStudent.Create; //create new student object
        Result[length(Result) - 1].setId(FieldByName('id').AsLongint); //set id
        Result[length(Result) - 1].setLastName(FieldByName('last_name').ToString);
        Result[length(Result) - 1].setFirstName(FieldByName('first_name').AsString);
        Result[length(Result) - 1].setClassName(FieldByName('class_name').AsString);
        Result[length(Result) - 1].setBirth(FieldByName('birth').AsDateTime);
        Next;
      end;
    end;

  finally
    //nix? todo
  end;
end;

function TDBConnection.getStudentsByLastNamePattern(lastName: String): ArrayOfStudents;
begin
  SQLQuery.Close;
  SQLQuery.SQL.Text := 'SELECT * FROM student WHERE last_name LIKE ''' + lastName + '''';
  SQLQuery.Open;

  Result := nil;

  try
    with SQLQuery do
    begin
      First;
      while not EOF do
      begin
        //new row
        setLength(Result, length(Result) + 1);
        Result[length(Result) - 1] := TStudent.Create; //create new student object
        Result[length(Result) - 1].setId(FieldByName('id').AsLongint); //set id
        Result[length(Result) - 1].setLastName(FieldByName('last_name').ToString);
        Result[length(Result) - 1].setFirstName(FieldByName('first_name').AsString);
        Result[length(Result) - 1].setClassName(FieldByName('class_name').AsString);
        Result[length(Result) - 1].setBirth(FieldByName('birth').AsDateTime);
        Next;
      end;
    end;

  finally
    //nix? todo
  end;
end;

function TDBConnection.getStudentsByClassName(className: String): ArrayOfStudents;
begin
  SQLQuery.Close;
  SQLQuery.SQL.Text := 'SELECT * FROM student WHERE class_name = ''' + className + '''';
  SQLQuery.Open;

  Result := nil;

  try
    with SQLQuery do
    begin
      First;
      while not EOF do
      begin
        //new row
        setLength(Result, length(Result) + 1);
        Result[length(Result) - 1] := TStudent.Create; //create new student object
        Result[length(Result) - 1].setId(FieldByName('id').AsLongint); //set id
        Result[length(Result) - 1].setLastName(FieldByName('last_name').ToString);
        Result[length(Result) - 1].setFirstName(FieldByName('first_name').AsString);
        Result[length(Result) - 1].setClassName(FieldByName('class_name').AsString);
        Result[length(Result) - 1].setBirth(FieldByName('birth').AsDateTime);
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
