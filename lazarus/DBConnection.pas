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
    // Returns an array of all students
    // result: array of student objects
    function getStudents: ArrayOfStudents;

    // Returns an array of all students with given first name
    // parameter: first name pattern. "%" can be used as a placeholder
    // result: array of student objects
    function getStudentsByFirstNamePattern(firstName: string): ArrayOfStudents;

    // Returns an array of all students with given last name
    // parameter: last name pattern. "%" can be used as a placeholder
    // result: array of student objects
    function getStudentsByLastNamePattern(lastName: string): ArrayOfStudents;

    // Returns an array of all students with given class name
    // parameter: class name
    // result: array of student objects
    function getStudentsByClassName(classN: string): ArrayOfStudents;

    // Returns student object with given id
    // parameter: student id
    // result: student object
    function getStudentById(id: int64): TStudent;

    // Persists student object into database. Either updates an existing one or inserts a new one
    // parameter: student object
    // result: TRUE on success
    function persistStudent(student: TStudent): boolean;

    // Deletes a student
    // parameter: student id
    // result: TRUE on success
    function deleteStudent(id: int64): boolean;
    constructor Create;
  private
    SQLite3Connection: TSQLite3Connection;
    SQLQuery: TSQLQuery;
    SQLTransaction: TSQLTransaction;
  end;

{var
  DBConn: DBConnection;}

implementation

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

function TDBConnection.getStudentsByFirstNamePattern(firstName: string): ArrayOfStudents;
begin
  SQLQuery.Close;
  SQLQuery.SQL.Text := 'SELECT * FROM student WHERE first_name LIKE ''' +
    firstName + '''';
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
        Result[length(Result) - 1].setLastName(FieldByName('last_name').AsString);
        Result[length(Result) - 1].setFirstName(FieldByName('first_name').AsString);
        Result[length(Result) - 1].setClassName(FieldByName('class_name').AsString);
        Result[length(Result) - 1].setBirth(FieldByName('birth').AsDateTime);
        Next;
      end;
    end;

  finally
    //todo
  end;
end;

function TDBConnection.getStudentsByLastNamePattern(lastName: string): ArrayOfStudents;
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
        Result[length(Result) - 1].setLastName(FieldByName('last_name').AsString);
        Result[length(Result) - 1].setFirstName(FieldByName('first_name').AsString);
        Result[length(Result) - 1].setClassName(FieldByName('class_name').AsString);
        Result[length(Result) - 1].setBirth(FieldByName('birth').AsDateTime);
        Next;
      end;
    end;

  finally
    //todo
  end;
end;

function TDBConnection.getStudentsByClassName(classN: string): ArrayOfStudents;
begin
  SQLQuery.Close;
  SQLQuery.SQL.Text := 'SELECT * FROM student WHERE class_name = ''' + ClassName + '''';
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
        Result[length(Result) - 1].setLastName(FieldByName('last_name').AsString);
        Result[length(Result) - 1].setFirstName(FieldByName('first_name').AsString);
        Result[length(Result) - 1].setClassName(FieldByName('class_name').AsString);
        Result[length(Result) - 1].setBirth(FieldByName('birth').AsDateTime);
        Next;
      end;
    end;

  finally
    //todo
  end;
end;

function TDBConnection.getStudentById(id: int64): TStudent;
begin
  SQLQuery.Close;
  SQLQuery.SQL.Text := 'SELECT * FROM student WHERE id = ' + IntToStr(id);
  SQLQuery.Open;

  Result := nil;

  try
    with SQLQuery do
    begin
      First;
      //new row
      if not EOF then
      begin
        Result := TStudent.Create; //create new student object
        Result.setId(FieldByName('id').AsLongint); //set id
        Result.setLastName(FieldByName('last_name').AsString);
        Result.setFirstName(FieldByName('first_name').AsString);
        Result.setClassName(FieldByName('class_name').AsString);
        Result.setBirth(FieldByName('birth').AsDateTime);
      end;
    end;

  finally
    //todo
  end;
end;

function TDBConnection.persistStudent(student: TStudent): boolean;
begin
  SQLQuery.Close;
  //get object from database if exists
  SQLQuery.SQL.Text := 'SELECT * FROM student WHERE id = ' + IntToStr(student.getId);
  SQLQuery.Open;

  try
    with SQLQuery do
    begin
      First;
      //new row

      if EOF then
      begin //object does not exist
        //ShowMessage('append');
        Append; //insert mode
      end
      else
      begin
        //ShowMessage('name ' + FieldByName('last_name').AsString);
        //ShowMessage('edit');
        Edit; //update mode
      end;

      //update object
      FieldByName('last_name').AsString := student.getLastName;
      FieldByName('first_name').AsString := student.getFirstName;
      FieldByName('class_name').AsString := student.getClassName;
      FieldByName('birth').AsDateTime := student.getBirth;
      Post; //add to change buffer
      ApplyUpdates; //commit change buffer to db
      SQLTransaction.commit;
      //ShowMessage('applied');
    end;

  finally
    //todo
  end;
end;

function TDBConnection.deleteStudent(id: int64): boolean;
begin
  SQLQuery.Close;
  SQLQuery.SQL.Text := 'SELECT * FROM student WHERE id = ' + IntToStr(id);
  SQLQuery.Open;

  try
    with SQLQuery do
    begin
      First;
      //new row
      if not EOF then
      begin
        Edit;
        Delete;
        Post; //add to change buffer
        ApplyUpdates; //commit change buffer to db
        SQLTransaction.commit;
      end;
    end;

  finally
    //todo
  end;
end;

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
