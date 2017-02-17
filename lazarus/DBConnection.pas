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
    //             RENTAL                                  //
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
    //             BOOK                                    //
    /////////////////////////////////////////////////////////

    // Returns an array of all books
    // result: array of book objects
    function getBooks: ArrayOfBooks;

    // Returns the book object by given book id or NIL if the book does not exist
    // parameter: book id
    // result: book object | nil
    function getBookById(id: int64): TBook;

    // Persists book object into database. Either updates an existing one or inserts a new one
    // parameter: book object
    // result: TRUE on success
    function persistBook(book: TBook): boolean;

    // Deletes a book
    // parameter: book object
    // result: TRUE on success
    function deleteBook(book: TBook): boolean;

    /////////////////////////////////////////////////////////
    //             BOOKTYPE                                //
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

    // Returns the current Error Object
    // result: Error Object (DBError, Type: EDatabaseError)
    function getError: EDatabaseError;

    /////////////////////////////////////////////////////////

    // Opens database connection
    // parameter: file path to sqlite file
    constructor Create(databasePath: string);

    // Closes the database connection
    destructor Destroy;

    // Checks if conncetion to database was successful
    // result: TRUE on success
    function isConnected: boolean;

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
  SQLQuery.SQL.Text := 'SELECT * FROM student WHERE first_name LIKE ''(:name)''';
  SQLQuery.ParamByName('name').AsString := firstName;
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
  SQLQuery.SQL.Text := 'SELECT * FROM student WHERE last_name LIKE ''(:name)''';
  SQLQuery.ParamByName('name').AsString := lastName;
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
  SQLQuery.SQL.Text := 'SELECT * FROM student WHERE class_name = ''(:name)''';
  SQLQuery.ParamByName('name').AsString := classN;
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
  SQLQuery.SQL.Text := 'SELECT * FROM student WHERE id = (:id)';
  SQLQuery.ParamByName('id').AsInteger := id;
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
  SQLQuery.SQL.Text := 'SELECT * FROM student WHERE id = (:id)';
  SQLQuery.ParamByName('id').AsInteger := student.getId;
  SQLQuery.Open;

  try
    with SQLQuery do
    begin
      First;
      //new row

      if EOF then
        Append //insert mode
      else
        Edit; //update mode

      //update object
      FieldByName('last_name').AsString := student.getLastName;
      FieldByName('first_name').AsString := student.getFirstName;
      FieldByName('class_name').AsString := student.getClassName;
      FieldByName('birth').AsDateTime := student.getBirth;
      Post; //add to change buffer
      ApplyUpdates; //commit change buffer to db
      SQLTransaction.commit;
    end;

  except
    on E: EDatabaseError do
    begin
      DBError := E;
      Result := False;
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
    end;

  except
    on E: EDatabaseError do
    begin
      DBError := E;
      Result := False;
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
  SQLQuery.SQL.Text := 'SELECT * FROM rental WHERE id = (:id)';
  SQLQuery.ParamByName('id').AsInteger := rental.getId;
  SQLQuery.Open;

  try
    with SQLQuery do
    begin
      First;
      //new row

      if EOF then
        Append //insert mode
      else
        Edit; //update mode

      //update object
      FieldByName('id').AsLongInt := rental.getId;
      FieldByName('student_id').AsLongInt := rental.getStudentId;
      FieldByName('book_id').AsLongInt := rental.getBookId;
      FieldByName('rental_date').AsDateTime := rental.getRentalDate;
      FieldByName('return_date').AsDateTime := rental.getReturnDate;
      Post; //add to change buffer
      ApplyUpdates; //commit change buffer to db
      SQLTransaction.commit;
    end;

  except
    on E: EDatabaseError do
    begin
      DBError := E;
      Result := False;
    end;
  end;
end;

function TDBConnection.deleteRental(rental: TRental): boolean;
begin
  SQLQuery.Close;
  SQLQuery.SQL.Text := 'delete from rental where id = (:id)';
  SQLQuery.ParamByName('id').AsInteger := rental.getId;


  try
    with SQLQuery do
    begin
      SQLQuery.ExecSQL;
      ApplyUpdates; //commit change buffer to db
      SQLTransaction.commit;
    end;

  except
    on E: EDatabaseError do
    begin
      DBError := E;
      Result := False;
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

function TDBConnection.getBookById(book: int64): TBook;
begin
  SQLQuery.Close;
  SQLQuery.SQL.Text := 'SELECT * FROM book WHERE id = (:id)';
  SQLQuery.ParamByName('id').AsInteger := id;
  SQLQuery.Open;

  Result := nil;

  try
    with SQLQuery do
    begin
      First;
      //new row
      if not EOF then
      begin
        Result := TBook.Create; //create new book object
        Result.setId(FieldByName('id').AsLongint); //set id
        Result.setIsbn(FieldByName('isbn').AsString);
        Result.setCondition(FieldByName('condition').AsInteger);
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
  SQLQuery.SQL.Text := 'SELECT * FROM book WHERE id = (:id)';
  SQLQuery.ParamByName('id').AsInteger := book.getId;
  SQLQuery.Open;

  try
    with SQLQuery do
    begin
      First;
      //new row

      if EOF then
        Append //insert mode
      else
        Edit; //update mode

      //update object
      FieldByName('id').AsLongint := book.getId;
      FieldByName('isbn').AsString := book.getIsbn;
      FieldByName('condition').AsInteger := book.getCondition;
      Post; //add to change buffer
      ApplyUpdates; //commit change buffer to db
      SQLTransaction.commit;
    end;

  except
    on E: EDatabaseError do
    begin
      DBError := E;
      Result := False;
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
    end;

  except
    on E: EDatabaseError do
    begin
      DBError := E;
      Result := False;
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
  SQLQuery.SQL.Text := 'SELECT * FROM booktype WHERE isbn = ''(:isbn)''';
  SQLQuery.ParamByName('isbn').AsString := booktype.getIsbn;
  SQLQuery.Open;

  try
    with SQLQuery do
    begin
      First;
      //new row

      if EOF then
        Append //insert mode
      else
        Edit; //update mode

      //update object
      FieldByName('isbn').AsString := booktype.getIsbn;
      FieldByName('title').AsString := booktype.getTitle;
      FieldByName('subject').AsString := booktype.getSubject;
      FieldByName('storage').AsInteger := booktype.getStorage;
      Post; //add to change buffer
      ApplyUpdates; //commit change buffer to db
      SQLTransaction.commit;
    end;

  except
    on E: EDatabaseError do
    begin
      DBError := E;
      Result := False;
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
    end;

  except
    on E: EDatabaseError do
    begin
      DBError := E;
      Result := False;
    end;
  end;
end;

////////////////////////////////////////////////////////

function TDBConnection.getError: EDatabaseError;
begin
  Result := DBError;
end;

constructor TDBConnection.Create(databasePath: string);
begin
  try
    self.SQLite3Connection := TSQLite3Connection.Create(nil);
    self.SQLTransaction := TSQLTransaction.Create(nil);
    self.SQLQuery := TSQLQuery.Create(nil);

    if not FileExists(databasePath) then
      exit;

    self.SQLite3Connection.DatabaseName := databasePath;
    self.SQLite3Connection.Transaction := self.SQLTransaction;

    self.SQLTransaction.Database := self.SQLite3Connection;

    self.SQLQuery.Database := self.SQLite3Connection;
    self.SQLQuery.Transaction := self.SQLTransaction;

    self.SQLite3Connection.Open;
  except
    on E: EDatabaseError do
      DBError := E;
  end;
end;

destructor TDBConnection.Destroy;
begin
  SQLQuery.Close;
  SQLQuery.Destroy;
  SQLTransaction.Destroy;
  SQLite3Connection.Destroy;
end;

function TDBConnection.isConnected: boolean;
begin
  Result := self.SQLite3Connection.Connected;
end;

end.
