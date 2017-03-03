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
    function getStudentById(id: longint): TStudent;

    // Updates or inserts a student object. Either updates an existing one or inserts a new one
    // parameter: student object
    // result: TRUE on success
    function updateinsertStudent(student: TStudent): boolean;

    // Deletes a student
    // parameter: student object
    // result: TRUE on success
    function deleteStudent(student: TStudent): boolean;

    // Returns TStudent object with given birthdate
    // parameter: TDate object (birthdate)
    // result: TStudent object
    function getStudentByBirthdate(birthdate: TDate): TStudent;

    /////////////////////////////////////////////////////////
    //             RENTAL                                  //
    /////////////////////////////////////////////////////////

    // Returns an array of all rentals
    // result: array of rental objects
    function getRentals: ArrayOfRentals;

    // Returns an array of all rentals with given student and book
    // parameter: student, book
    // result: array of rental objects
    function getAllRentalsByBookAndStudent(student: TStudent;
      book: TBook): ArrayOfRentals;

    // Updates or inserts a rental object. Either updates an existing one or inserts a new one
    // parameter: rental object
    // result: TRUE on success
    function updateinsertRental(rental: TRental): boolean;

    // Deletes a student
    // parameter: rental object
    // result: TRUE on success
    function deleteRental(rental: TRental): boolean;

    // Deletes all returned rentals older than a certain date
    // parameter: Date
    // result: Amount of deleted rentals on success, -1 on error
    function deleteReturnedRentalOlderThan(date: TDate): integer;

    /////////////////////////////////////////////////////////
    //             BOOK                                    //
    /////////////////////////////////////////////////////////

    // Returns an array of all books
    // result: array of book objects
    function getBooks: ArrayOfBooks;

    // Returns the book object by given book id or NIL if the book does not exist
    // parameter: book id
    // result: book object | nil
    function getBookById(id: longint): TBook;

    // Updates or inserts book object. Either updates an existing one or inserts a new one
    // parameter: book object
    // result: TRUE on success
    function updateinsertBook(book: TBook): boolean;

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

    // Updates or inserts booktype object. Either updates an existing one or inserts a new one
    // parameter: booktype object
    // result: TRUE on success
    function updateinsertBooktype(booktype: TBooktype): boolean;

    // Returns the Booktype of an ISBN Number
    // parameter: Isbn (String type)
    // result: TBooktype on success, NIL on failure
    function getBooktypeByIsbn(isbn: string): TBooktype;

    // Deletes a book
    // parameter: booktype object
    // result: TRUE on success
    function deleteBooktype(booktype: TBooktype): boolean;

    /////////////////////////////////////////////////////////

    // Returns the current Error Object
    // result: Error Object (DBError, Type: Exception)
    function getError: Exception;

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
    procedure setStudentFields(var resultVar: ArrayOfStudents; returnOne: boolean);
    procedure setRentalFields(var resultVar: ArrayOfRentals; returnOne: boolean);
    procedure setBookFields(var resultVar: ArrayOfBooks; returnOne: boolean);
    procedure setBooktypeFields(var resultVar: ArrayOfBooktypes; returnOne: boolean);

  private
    SQLite3Connection: TSQLite3Connection;
    SQLQuery: TSQLQuery;
    SQLTransaction: TSQLTransaction;
    DBError: Exception;
  end;

implementation

procedure TDBConnection.setStudentFields(var resultVar: ArrayOfStudents;
  returnOne: boolean);
begin
  try
    with SQLQuery do
    begin
      setLength(resultVar, 0);
      First;
      while not EOF do
      begin
        //new row
        setLength(resultVar, length(resultVar) + 1);
        resultVar[length(resultVar) - 1] := TStudent.Create;
        //create new student object
        resultVar[length(resultVar) - 1].setId(FieldByName('id').AsLargeInt); //set id
        resultVar[length(resultVar) - 1].setLastName(FieldByName('last_name').AsString);
        resultVar[length(resultVar) - 1].setFirstName(
          FieldByName('first_name').AsString);
        resultVar[length(resultVar) - 1].setClassName(
          FieldByName('class_name').AsString);
        resultVar[length(resultVar) - 1].setBirth(FieldByName('birth').AsDateTime);
        if returnOne then
          exit;
        Next;
      end;

    end;

  except
    on E: Exception do
    begin
      DBError := E;
      resultVar := nil;
    end;
  end;
end;

function TDBConnection.getStudents: ArrayOfStudents;
begin
  DBError := nil;
  try
    with SQLQuery do
    begin
      SQLQuery.Close;
      SQLQuery.SQL.Text := 'SELECT * FROM student';
      SQLQuery.Open;
    end;

  except
    on E: Exception do
    begin
      DBError := E;
      Result := nil;
      exit;
    end;
  end;
  Result := nil;
  setStudentFields(Result, False);
end;

function TDBConnection.getStudentsByFirstNamePattern(firstName: string): ArrayOfStudents;
begin
  DBError := nil;
  try
    with SQLQuery do
    begin
      SQLQuery.Close;
      SQLQuery.SQL.Text := 'SELECT * FROM student WHERE first_name LIKE ''(:name)''';
      SQLQuery.ParamByName('name').AsString := firstName;
      SQLQuery.Open;
    end;

  except
    on E: Exception do
    begin
      DBError := E;
      Result := nil;
      exit;
    end;
  end;

  Result := nil;

  setStudentFields(Result, False);
end;

function TDBConnection.getStudentsByLastNamePattern(lastName: string): ArrayOfStudents;
begin
  DBError := nil;
  try
    with SQLQuery do
    begin
      SQLQuery.Close;
      SQLQuery.SQL.Text := 'SELECT * FROM student WHERE last_name LIKE ''(:name)''';
      SQLQuery.ParamByName('name').AsString := lastName;
      SQLQuery.Open;
    end;

  except
    on E: Exception do
    begin
      DBError := E;
      Result := nil;
      exit;
    end;
  end;
  Result := nil;

  setStudentFields(Result, False);
end;

function TDBConnection.getStudentsByClassName(classN: string): ArrayOfStudents;
begin
  DBError := nil;
  try
    with SQLQuery do
    begin
      SQLQuery.Close;
      SQLQuery.SQL.Text := 'SELECT * FROM student WHERE class_name = ''(:name)''';
      SQLQuery.ParamByName('name').AsString := classN;
      SQLQuery.Open;
    end;

  except
    on E: Exception do
    begin
      DBError := E;
      Result := nil;
      exit;
    end;
  end;
  Result := nil;

  setStudentFields(Result, False);
end;

function TDBConnection.getStudentById(id: longint): TStudent;
var
  arr: ArrayOfStudents;
begin
  DBError := nil;
  try
    with SQLQuery do
    begin
      SQLQuery.Close;
      SQLQuery.SQL.Text := 'SELECT * FROM student WHERE id = (:id)';
      SQLQuery.ParamByName('id').AsInteger := id;
      SQLQuery.Open;
    end;

  except
    on E: Exception do
    begin
      DBError := E;
      Result := nil;
      exit;
    end;
  end;
  setStudentFields(arr, True);

  Result := nil;
  if (arr[0] <> nil) then
    Result := arr[0];
end;

function TDBConnection.updateinsertStudent(student: TStudent): boolean;
begin
  DBError := nil;
  SQLQuery.Close;
  SQLQuery.SQL.Text := 'SELECT * FROM student WHERE id = (:id)';
  SQLQuery.ParamByName('id').AsInteger := student.getId;
  SQLQuery.Open;

  try
    with SQLQuery do
    begin
      First;
      //new row

      if EOF or (student.getId = -1) then
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

    Result := True;
  except
    on E: Exception do
    begin
      DBError := E;
      Result := False;
    end;
  end;
end;

function TDBConnection.deleteStudent(student: TStudent): boolean;
begin
  DBError := nil;
  SQLQuery.Close;
  SQLQuery.SQL.Text := 'delete from student where id = (:id)';
  SQLQuery.ParamByName('id').AsLargeInt := student.getId;


  try
    with SQLQuery do
    begin
      SQLQuery.ExecSQL;
      SQLTransaction.commit;
    end;

  except
    on E: Exception do
    begin
      DBError := E;
      Result := False;
    end;
  end;
end;

function TDBConnection.getStudentByBirthdate(birthdate: TDate): TStudent;
begin
  DBError := nil;
  SQLQuery.Close;
  SQLQuery.SQL.Text := 'SELECT FROM student WHERE birth = (:birthdate)';
  SQLQuery.ParamByName('birthdate').AsDate := birthdate;

  try
    with SQLQuery do
    begin
      SQLQuery.Open;
    end;
  except
    on E: Exception do
    begin
      DBError := E;
      Result := nil;
    end;
  end;
end;

////////////////////////////////////////////////////////

procedure TDBConnection.setRentalFields(var resultVar: ArrayOfRentals;
  returnOne: boolean);
begin
  try
    with SQLQuery do
    begin
      setLength(resultVar, 0);
      First;
      while not EOF do
      begin
        //new row
        setLength(resultVar, length(resultVar) + 1);

        resultVar[length(resultVar) - 1] := TRental.Create; //create new rental object
        resultVar[length(resultVar) - 1].setId(FieldByName('id').AsLargeInt);
        resultVar[length(resultVar) - 1].setBookId(FieldByName('book_id').AsLargeInt);
        resultVar[length(resultVar) - 1].setStudentId(
          FieldByName('student_id').AsLargeInt);
        resultVar[length(resultVar) - 1].setReturnDate(
          FieldByName('return_date').AsDateTime);
        resultVar[length(resultVar) - 1].setRentalDate(
          FieldByName('rental_date').AsDateTime);
        if returnOne then
          exit;
        Next;
      end;
    end;

  except
    on E: Exception do
    begin
      DBError := E;
      resultVar := nil;
    end;
  end;
end;

function TDBConnection.getRentals: ArrayOfRentals;
begin
  DBError := nil;
  try
    with SQLQuery do
    begin
      SQLQuery.Close;
      SQLQuery.SQL.Text := 'SELECT * FROM rental';
      SQLQuery.Open;
    end;

  except
    on E: Exception do
    begin
      DBError := E;
      Result := nil;
      exit;
    end;
  end;

  Result := nil;

  setRentalFields(Result, False);
end;

function TDBConnection.getAllRentalsByBookAndStudent(student: TStudent;
  book: TBook): ArrayOfRentals;
begin
  DBError := nil;
  SQLQuery.Close;
  try
    with SQLQuery do
    begin
      SQLQuery.SQL.Text :=
        'SELECT * FROM rental where book_id = (:book) and student_id = (:student)';
      SQLQuery.ParamByName('book').AsLargeInt := book.getId;
      SQLQuery.ParamByName('student').AsLargeInt := student.getId;
      SQLQuery.Open;
    end;

  except
    on E: Exception do
    begin
      DBError := E;
      Result := nil;
      exit;
    end;
  end;

  Result := nil;

  setRentalFields(Result, False);
end;

function TDBConnection.updateinsertRental(rental: TRental): boolean;
begin
  DBError := nil;
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

      if EOF or (rental.getId = -1) then
        Append //insert mode
      else
        Edit; //update mode

      //update object
      FieldByName('student_id').AsLargeInt := rental.getStudentId;
      FieldByName('book_id').AsLargeInt := rental.getBookId;
      FieldByName('rental_date').AsDateTime := rental.getRentalDate;
      FieldByName('return_date').AsDateTime := rental.getReturnDate;
      Post; //add to change buffer
      ApplyUpdates; //commit change buffer to db
      SQLTransaction.commit;
    end;

    Result := True;
  except
    on E: Exception do
    begin
      DBError := E;
      Result := False;
    end;
  end;
end;

function TDBConnection.deleteRental(rental: TRental): boolean;
begin
  DBError := nil;
  SQLQuery.Close;
  SQLQuery.SQL.Text := 'delete from rental where id = (:id)';
  SQLQuery.ParamByName('id').AsLargeInt := rental.getId;

  try
    with SQLQuery do
    begin
      ExecSQL;
      SQLTransaction.commit;
    end;

  except
    on E: Exception do
    begin
      DBError := E;
      Result := False;
    end;
  end;
end;

function TDBConnection.deleteReturnedRentalOlderThan(date: TDate): integer;
begin
  DBError := nil;
  SQLQuery.Close;
  SQLQuery.SQL.Text :=
    'delete from rental where return_date not null and date(date()) <= (:date)';
  SQLQuery.ParamByName('date').AsDate := date;

  try
    with SQLQuery do
    begin
      SQLQuery.ExecSQL;
      SQLTransaction.commit;

      Close;
      SQL.Text := 'SELECT @@ROWCOUNT as deleted';
      Open;

      First;
      while not EOF do
      begin
        Result := FieldByName('deleted').AsInteger;
      end;
    end;
  except
    on E: Exception do
    begin
      DBError := E;
      Result := -1;
    end;
  end;

end;

////////////////////////////////////////////////////////

procedure TDBConnection.setBookFields(var resultVar: ArrayOfBooks; returnOne: boolean);
begin
  try
    with SQLQuery do
    begin
      setLength(resultVar, 0);
      First;
      while not EOF do
      begin
        //new row
        setLength(resultVar, length(resultVar) + 1);

        resultVar[length(resultVar) - 1] := TBook.Create; //create new book object
        resultVar[length(resultVar) - 1].setId(FieldByName('id').AsLargeInt);
        resultVar[length(resultVar) - 1].setIsbn(FieldByName('isbn').AsString);
        resultVar[length(resultVar) - 1].setCondition(
          FieldByName('condition').AsInteger);
        if returnOne then
          exit;
        Next;
      end;
    end;

  except
    on E: Exception do
    begin
      DBError := E;
      resultVar := nil;
    end;
  end;
end;

function TDBConnection.getBooks: ArrayOfBooks;
begin
  DBError := nil;
  try
    with SQLQuery do
    begin
      SQLQuery.Close;
      SQLQuery.SQL.Text := 'SELECT * FROM book';
      SQLQuery.Open;
    end;

  except
    on E: Exception do
    begin
      DBError := E;
      Result := nil;
      exit;
    end;
  end;

  Result := nil;

  setBookFields(Result, False);
end;

function TDBConnection.getBookById(id: longint): TBook;
var
  arr: ArrayOfBooks;
begin
  DBError := nil;
  try
    with SQLQuery do
    begin
      SQLQuery.Close;
      SQLQuery.SQL.Text := 'SELECT * FROM book WHERE id = (:id)';
      SQLQuery.ParamByName('id').AsInteger := id;
      SQLQuery.Open;
    end;

  except
    on E: Exception do
    begin
      DBError := E;
      Result := nil;
      exit;
    end;
  end;

  setBookFields(arr, True);

  Result := nil;
  if (arr[0] <> nil) then
    Result := arr[0];
end;

function TDBConnection.updateinsertBook(book: TBook): boolean;
begin
  DBError := nil;
  SQLQuery.Close;
  SQLQuery.SQL.Text := 'SELECT * FROM book WHERE id = (:id)';
  SQLQuery.ParamByName('id').AsInteger := book.getId;
  SQLQuery.Open;

  try
    with SQLQuery do
    begin
      First;
      //new row

      if EOF or (book.getId = -1) then
        Append //insert mode
      else
        Edit; //update mode

      //update object
      FieldByName('id').AsLargeInt := book.getId;
      FieldByName('isbn').AsString := book.getIsbn;
      FieldByName('condition').AsInteger := book.getCondition;
      Post; //add to change buffer
      ApplyUpdates; //commit change buffer to db
      SQLTransaction.commit;
    end;

    Result := True;
  except
    on E: Exception do
    begin
      DBError := E;
      Result := False;
    end;
  end;
end;

function TDBConnection.deleteBook(book: TBook): boolean;
begin
  DBError := nil;
  SQLQuery.Close;
  SQLQuery.SQL.Text := 'delete from book where id = (:id)';
  SQLQuery.ParamByName('id').AsLargeInt := book.getId;


  try
    with SQLQuery do
    begin
      ExecSQL;
      ApplyUpdates; //test
      SQLTransaction.commit;
    end;

  except
    on E: Exception do
    begin
      DBError := E;
      Result := False;
    end;
  end;
end;

////////////////////////////////////////////////////////

procedure TDBConnection.setBooktypeFields(var resultVar: ArrayOfBookTypes;
  returnOne: boolean);
begin
  try
    with SQLQuery do
    begin
      setLength(resultVar, 0);
      First;
      while not EOF do
      begin
        //new row
        setLength(resultVar, length(resultVar) + 1);

        resultVar[length(resultVar) - 1] := TBooktype.Create;
        //create new booktype object
        resultVar[length(resultVar) - 1].setIsbn(FieldByName('isbn').AsString);
        resultVar[length(resultVar) - 1].setTitle(FieldByName('title').AsString);
        resultVar[length(resultVar) - 1].setSubject(FieldByName('subject').AsString);
        resultVar[length(resultVar) - 1].setStorage(FieldByName('storage').AsInteger);
        if returnOne then
          exit;
        Next;
      end;
    end;

  except
    on E: Exception do
    begin
      DBError := E;
      resultVar := nil;
    end;
  end;
end;

function TDBConnection.getBooktypes: ArrayOfBooktypes;
begin
  DBError := nil;
  try
    with SQLQuery do
    begin
      SQLQuery.Close;
      SQLQuery.SQL.Text := 'SELECT * FROM booktype';
      SQLQuery.Open;
    end;

  except
    on E: Exception do
    begin
      DBError := E;
      Result := nil;
      exit;
    end;
  end;

  Result := nil;

  setBooktypeFields(Result, False);
end;

function TDBConnection.updateinsertBooktype(booktype: TBooktype): boolean;
begin
  DBError := nil;
  SQLQuery.Close;
  SQLQuery.SQL.Text := 'SELECT * FROM booktype WHERE isbn = (:isbn)';
  SQLQuery.ParamByName('isbn').AsString := booktype.getIsbn;
  SQLQuery.Open;

  try
    with SQLQuery do
    begin
      First;
      //new row

      if EOF or (length(booktype.getIsbn) = 0) then
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

    Result := True;
  except
    on E: Exception do
    begin
      DBError := E;
      Result := False;
    end;
  end;
end;

function TDBConnection.deleteBooktype(booktype: TBooktype): boolean;
begin
  DBError := nil;
  SQLQuery.Close;
  SQLQuery.SQL.Text := 'delete from booktype where isbn = (:isbn)';
  SQLQuery.ParamByName('isbn').AsString := booktype.getIsbn;


  try
    with SQLQuery do
    begin
      ExecSQL;
      SQLTransaction.commit;
    end;

  except
    on E: Exception do
    begin
      DBError := E;
      Result := False;
    end;
  end;
end;

function TDBConnection.getBooktypeByIsbn(isbn: string): TBooktype;
var
  arr: ArrayOfBooktypes;
begin
  DBError := nil;
  try
    with SQLQuery do
    begin
      SQLQuery.Close;
      SQLQuery.SQL.Text := 'SELECT * FROM booktype WHERE isbn = (:isbn)';
      SQLQuery.ParamByName('isbn').AsString := isbn;
      SQLQuery.Open;
    end;

  except
    on E: Exception do
    begin
      DBError := E;
      Result := nil;
      exit;
    end;
  end;

  setBooktypeFields(arr, True);

  Result := nil;
  if (arr[0] <> nil) then
    Result := arr[0];
end;

////////////////////////////////////////////////////////

function TDBConnection.getError: Exception;
begin
  Result := DBError;
end;

constructor TDBConnection.Create(databasePath: string);
begin
  DBError := nil;
  try
    self.SQLite3Connection := TSQLite3Connection.Create(nil);
    self.SQLTransaction := TSQLTransaction.Create(nil);
    self.SQLQuery := TSQLQuery.Create(nil);

    if not FileExists(databasePath) then
    begin
      DBError := Exception.Create('Unable to open file.');
      exit;
    end;

    self.SQLite3Connection.DatabaseName := databasePath;
    self.SQLite3Connection.Transaction := self.SQLTransaction;

    self.SQLTransaction.Database := self.SQLite3Connection;

    self.SQLQuery.Database := self.SQLite3Connection;
    self.SQLQuery.Transaction := self.SQLTransaction;

    self.SQLite3Connection.Open;
  except
    on E: Exception do
      DBError := E;
  end;
end;

destructor TDBConnection.Destroy;
begin
  DBError := nil;

  SQLQuery.Close;
  SQLQuery.Destroy;
  SQLTransaction.Destroy;
  SQLite3Connection.Destroy;
end;

function TDBConnection.isConnected: boolean;
begin
  try
    begin
      Result := self.SQLite3Connection.Connected;
    end;
  except
    on E: Exception do
    begin
      DBError := E;
      Result := False;
    end;
  end;
end;

end.
