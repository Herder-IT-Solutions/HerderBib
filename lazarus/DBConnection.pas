unit DBConnection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqlite3conn, sqldb, DB, FileUtil, Forms, Controls,
  Graphics, Dialogs, DBGrids, book, booktype, rental, student;

type
  ArrayOfStudents = array of TStudent;
  ArrayOfRentals = array of TRental;
  ArrayOfBooks = array of TBook;
  ArrayOfBooktypes = array of TBooktype;

  { TForm1 }
  TDBConnection = class
  public
    /////////////////////////////////////////////////////////
    //             STUDENT                                 //
    /////////////////////////////////////////////////////////

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
    // parameter: student object
    // result: TRUE on success
    function deleteStudent(student: TStudent): boolean;

    /////////////////////////////////////////////////////////
    //             RENTAL
    /////////////////////////////////////////////////////////

    // Returns an array of all rentals
    // result: array of rental objects
    function getRentals: ArrayOfRentals;

    // Persists rental object into database. Either updates an existing one or inserts a new one
    // parameter: rental object
    // result: TRUE on success
    function persistRental(rental: TRental): boolean;

    // Deletes a student
    // parameter: rental object
    // result: TRUE on success
    function deleteRental(rental: TRental): boolean;

    /////////////////////////////////////////////////////////
    //             BOOK
    /////////////////////////////////////////////////////////

    // Returns an array of all books
    // result: array of book objects
    function getBooks: ArrayOfBooks;

    // Persists book object into database. Either updates an existing one or inserts a new one
    // parameter: book object
    // result: TRUE on success
    function persistBook(book: TBook): boolean;

    // Deletes a book
    // parameter: book object
    // result: TRUE on success
    function deleteBook(book: TBook): boolean;

    /////////////////////////////////////////////////////////
    //             BOOKTYPE
    /////////////////////////////////////////////////////////

    // Returns an array of all books
    // result: array of booktype objects
    function getBooktypes: ArrayOfBooktypes;

    // Persists booktype object into database. Either updates an existing one or inserts a new one
    // parameter: booktype object
    // result: TRUE on success
    function persistBooktype(booktype: TBooktype): boolean;

    // Deletes a book
    // parameter: booktype object
    // result: TRUE on success
    function deleteBooktype(booktype: TBooktype): boolean;

    /////////////////////////////////////////////////////////

    constructor Create;
    function getError:EDatabaseError;
  private
    SQLite3Connection: TSQLite3Connection;
    SQLQuery: TSQLQuery;
    SQLTransaction: TSQLTransaction;
    DBError: EDatabaseError;
  end;

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
        Result[length(Result) - 1].setLastName(FieldByName('last_name').AsString);
        Result[length(Result) - 1].setFirstName(FieldByName('first_name').AsString);
        Result[length(Result) - 1].setClassName(FieldByName('class_name').AsString);
        Result[length(Result) - 1].setBirth(FieldByName('birth').AsDateTime);
        Next;
      end;
    end;

  except
    on E: EDatabaseError do
    begin
      DBError := E;
      Result := nil;
    end;
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

  except
    on E: EDatabaseError do
    begin
      DBError := E;
      Result := nil;
    end;
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

  except
    on E: EDatabaseError do
    begin
      DBError := E;
      Result := nil;
    end;
  end;
end;

function TDBConnection.getStudentsByClassName(classN: string): ArrayOfStudents;
begin
  SQLQuery.Close;
  SQLQuery.SQL.Text := 'SELECT * FROM student WHERE class_name = ''' + classN + '''';
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

  except
    on E: EDatabaseError do
    begin
      DBError := E;
      Result := nil;
    end;
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

  except
    on E: EDatabaseError do
    begin
      DBError := E;
      Result := nil;
    end;
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

  except
    on E: EDatabaseError do
    begin
      DBError := E;
      Result := false;
    end;
  end;
end;

function TDBConnection.deleteStudent(student: TStudent): boolean;
begin
  SQLQuery.Close;
  SQLQuery.SQL.Text := 'delete from student where id = (:id)';
  SQLQuery.ParamByName('id').AsInteger := student.getId;


  try
    with SQLQuery do
    begin
      SQLQuery.ExecSQL;
      ApplyUpdates; //commit change buffer to db
      SQLTransaction.commit;
      //ShowMessage('applied');
    end;

  except
    on E: EDatabaseError do
    begin
      DBError := E;
      Result := false;
    end;
  end;
end;

////////////////////////////////////////////////////////

function TDBConnection.getRentals: ArrayOfRentals;
begin
  SQLQuery.Close;
  SQLQuery.SQL.Text := 'SELECT * FROM rental';
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
        Result[length(Result) - 1] := TRental.Create; //create new rental object
        Result[length(Result) - 1].setBookId(FieldByName('book_id').AsLongint);
        Result[length(Result) - 1].setStudentId(FieldByName('student_id').AsLongint);
        Result[length(Result) - 1].setReturnDate(FieldByName('return_date').AsDateTime);
        Result[length(Result) - 1].setRentalDate(FieldByName('rental_date').AsDateTime);
        Next;
      end;
    end;

  except
    on E: EDatabaseError do
    begin
      DBError := E;
      Result := nil;
    end;
  end;
end;

function TDBConnection.persistRental(rental: TRental): boolean;
begin
  SQLQuery.Close;
  //get object from database if exists
  SQLQuery.SQL.Text := 'SELECT * FROM rental WHERE id = ' + IntToStr(rental.getId);
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
      FieldByName('id').AsLongInt := rental.getId;
      FieldByName('student_id').AsLongInt := rental.getStudentId;
      FieldByName('book_id').AsLongInt := rental.getBookId;
      FieldByName('rental_date').AsDateTime := rental.getRentalDate;
      FieldByName('return_date').AsDateTime := rental.getReturnDate;
      Post; //add to change buffer
      ApplyUpdates; //commit change buffer to db
      SQLTransaction.commit;
      //ShowMessage('applied');
    end;

  except
    on E: EDatabaseError do
    begin
      DBError := E;
      Result := false;
    end;
  end;
end;

function TDBConnection.deleteRental(rental: TRental): boolean;
begin
  SQLQuery.Close;
  SQLQuery.SQL.Text := 'delete from rental where id = (:BId)';
  SQLQuery.ParamByName('BId').AsInteger := rental.getId;


  try
    with SQLQuery do
    begin
      SQLQuery.ExecSQL;
      ApplyUpdates; //commit change buffer to db
      SQLTransaction.commit;
      //ShowMessage('applied');
    end;

  except
    on E: EDatabaseError do
    begin
      DBError := E;
      Result := false;
    end;
  end;
end;

////////////////////////////////////////////////////////

function TDBConnection.getBooks: ArrayOfBooks;
begin
  SQLQuery.Close;
  SQLQuery.SQL.Text := 'SELECT * FROM book';
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
        Result[length(Result) - 1] := TBook.Create; //create new book object
        Result[length(Result) - 1].setId(FieldByName('id').AsLongint);
        Result[length(Result) - 1].setIsbn(FieldByName('isbn').AsString);
        Result[length(Result) - 1].setCondition(FieldByName('condition').AsInteger);
        Next;
      end;
    end;

  except
    on E: EDatabaseError do
    begin
      DBError := E;
      Result := nil;
    end;
  end;
end;

function TDBConnection.persistBook(book: TBook): boolean;
begin
  SQLQuery.Close;
  //get object from database if exists
  SQLQuery.SQL.Text := 'SELECT * FROM book WHERE id = ' + IntToStr(book.getId);
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
      FieldByName('id').AsLongint := book.getId;
      FieldByName('isbn').AsString := book.getIsbn;
      FieldByName('condition').AsInteger := book.getCondition;
      Post; //add to change buffer
      ApplyUpdates; //commit change buffer to db
      SQLTransaction.commit;
      //ShowMessage('applied');
    end;

  except
    on E: EDatabaseError do
    begin
      DBError := E;
      Result := false;
    end;
  end;
end;

function TDBConnection.deleteBook(book: TBook): boolean;
begin
  SQLQuery.Close;
  SQLQuery.SQL.Text := 'delete from book where id = (:id)';
  SQLQuery.ParamByName('id').AsInteger := book.getId;


  try
    with SQLQuery do
    begin
      SQLQuery.ExecSQL;
      ApplyUpdates; //commit change buffer to db
      SQLTransaction.commit;
      //ShowMessage('applied');
    end;

  except
    on E: EDatabaseError do
    begin
      DBError := E;
      Result := false;
    end;
  end;
end;

////////////////////////////////////////////////////////

function TDBConnection.getBooktypes: ArrayOfBooktypes;
begin
  SQLQuery.Close;
  SQLQuery.SQL.Text := 'SELECT * FROM booktype';
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
        Result[length(Result) - 1] := TBooktype.Create; //create new booktype object
        Result[length(Result) - 1].setIsbn(FieldByName('isbn').AsString);
        Result[length(Result) - 1].setTitle(FieldByName('title').AsString);
        Result[length(Result) - 1].setSubject(FieldByName('subject').AsString);
        Result[length(Result) - 1].setStorage(FieldByName('storage').AsInteger);
        Next;
      end;
    end;

  except
    on E: EDatabaseError do
    begin
      DBError := E;
      Result := nil;
    end;
  end;
end;

function TDBConnection.persistBooktype(booktype: TBooktype): boolean;
begin
  SQLQuery.Close;
  //get object from database if exists
  SQLQuery.SQL.Text := 'SELECT * FROM booktype WHERE isbn = ''' +
    booktype.getIsbn + '''';
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
      FieldByName('isbn').AsString := booktype.getIsbn;
      FieldByName('title').AsString := booktype.getTitle;
      FieldByName('subject').AsString := booktype.getSubject;
      FieldByName('storage').AsInteger := booktype.getStorage;
      Post; //add to change buffer
      ApplyUpdates; //commit change buffer to db
      SQLTransaction.commit;
      //ShowMessage('applied');
    end;

  except
    on E: EDatabaseError do
    begin
      DBError := E;
      Result := false;
    end;
  end;
end;

function TDBConnection.deleteBooktype(booktype: TBooktype): boolean;
begin
  SQLQuery.Close;
  SQLQuery.SQL.Text := 'delete from booktype where isbn = (:isbn)';
  SQLQuery.ParamByName('isbn').AsString := booktype.getIsbn;


  try
    with SQLQuery do
    begin
      SQLQuery.ExecSQL;
      ApplyUpdates; //commit change buffer to db
      SQLTransaction.commit;
      //ShowMessage('applied');
    end;

  except
    on E: EDatabaseError do
    begin
      DBError := E;
      Result := false;
    end;
  end;
end;

////////////////////////////////////////////////////////

function getError:EDatabaseError;
begin
  result := DBError;
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
