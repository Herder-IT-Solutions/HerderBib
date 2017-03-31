unit DBConnection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqlite3conn, sqldb, DB, FileUtil, Forms, Controls,
  Graphics, Dialogs, DBGrids, book, booktype, rental, student, DBConstants;

type
  ArrayOfStudents = array of TStudent;
  ArrayOfRentals = array of TRental;
  ArrayOfBooks = array of TBook;
  ArrayOfBooktypes = array of TBooktype;

  ///////////////////////////////////////////////////////////
  //         Notes for use of TDBConnection                //
  //                                                       //
  //  You can call every function below, but you need to   //
  //  check if there were any errors by checking that the  //
  //  result of TDBConnection.getError function is NIL.    //
  //                                                       //
  ///////////////////////////////////////////////////////////

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
    function getStudentById(id: LargeInt): TStudent;

    // Returns an array of all students object with given birthdate
    // parameter: TDate object (birthdate)
    // result: array of student objects
    function getStudentsByBirthdate(birthdate: TDate): ArrayOfStudents;

    // Returns an array of all students with given ldap username
    // parameter: student's ldap username. "%" can be used as a placeholder
    // result: array of student objects
    function getStudentsByLDAPUserPattern(ldap_user: string): ArrayOfStudents;

    // Returns student who rented a book, nil if the book is not rented
    // parameter: book object
    // result: student object
    function getStudentWhoRentedBook(book: TBook): TStudent;

    // Returns an array of Students matching the given parameters
    // parameter: first name, last name, class name, birthdate
    // result: array of student objects
    function getStudentsByFistLastClassNameBirthdate(fname, lname, cname: string;
      birth: TDate): ArrayOfStudents;

    // updateInserts student object into database. Either updates an existing one or inserts a new one
    // parameter: student object
    // result: TRUE on success
    function updateInsertStudent(var student: TStudent): boolean;

    // Deletes a student and destroys the object
    // parameter: student object
    // result: TRUE on success
    function deleteStudent(var student: TStudent): boolean;


    /////////////////////////////////////////////////////////
    //             RENTAL                                  //
    /////////////////////////////////////////////////////////

    // Returns an array of all rentals
    // result: array of rental objects
    function getRentals: ArrayOfRentals;

    // Returns an array of all rentals with given student and book
    // parameter: student, book objects
    // result: array of rental objects
    function getAllRentalsByBookAndStudent(var student: TStudent;
      var book: TBook): ArrayOfRentals;

    // Checks if there are unreturned rentals for a specific book
    // parameter: book object
    // result: true when rentals exist that match this criteria
    function existsUnreturnedRentalByBook(var book: TBook): boolean;

    // updateInserts rental object into database. Either updates an existing one or inserts a new one
    // parameter: rental object
    // result: TRUE on success
    function updateInsertRental(var rental: TRental): boolean;

    // Deletes a rental and destroys the object
    // parameter: rental object
    // result: TRUE on success
    function deleteRental(var rental: TRental): boolean;

    // Deletes all returned rentals older than a certain date
    // parameter: date
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
    function getBookById(id: LargeInt): TBook;

    // updateInserts book object into database. Either updates an existing one or inserts a new one
    // parameter: book object
    // result: TRUE on success
    function updateInsertBook(var book: TBook): boolean;

    // Deletes a book and destroys the object
    // parameter: book object
    // result: TRUE on success
    function deleteBook(var book: TBook): boolean;

    /////////////////////////////////////////////////////////
    //             BOOKTYPE                                //
    /////////////////////////////////////////////////////////

    // Returns an array of all booktypes
    // result: array of booktype objects
    function getBooktypes: ArrayOfBooktypes;

    // Returns the Booktype of an ISBN Number
    // parameter: isbn
    // result: TBooktype on success, NIL on failure
    function getBooktypeByIsbn(isbn: string): TBooktype;

    // updateInserts booktype object into database. Either updates an existing one or inserts a new one
    // parameter: booktype object
    // result: TRUE on success
    function updateInsertBooktype(var booktype: TBooktype): boolean;

    // Deletes a booktype and destroys the object
    // parameter: booktype object
    // result: TRUE on success
    function deleteBooktype(var booktype: TBooktype): boolean;

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

        if not (FieldByName('birth').IsNull) then
          resultVar[length(resultVar) - 1].setBirth(
            StrToDate(FieldByName('birth').AsString));

        resultVar[length(resultVar) - 1].setLDAPUser(FieldByName('ldap_user').AsString);
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
      SQLQuery.SQL.Text :=
        'SELECT * FROM student WHERE LOWER(first_name) LIKE LOWER(:name)';
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
      SQLQuery.SQL.Text :=
        'SELECT * FROM student WHERE LOWER(last_name) LIKE LOWER(:name)';
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
      SQLQuery.SQL.Text :=
        'SELECT * FROM student WHERE LOWER(class_name) = LOWER(:name)';
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

function TDBConnection.getStudentById(id: LargeInt): TStudent;
var
  arr: ArrayOfStudents;
begin
  DBError := nil;
  try
    with SQLQuery do
    begin
      SQLQuery.Close;
      SQLQuery.SQL.Text := 'SELECT * FROM student WHERE id = (:id) LIMIT 1';
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
  if (length(arr) > 0) then
    Result := arr[0];
end;

function TDBConnection.getStudentsByBirthdate(birthdate: TDate): ArrayOfStudents;
begin
  DBError := nil;
  SQLQuery.Close;

  SQLQuery.SQL.Text := 'SELECT * FROM student WHERE birth = (:birthdate)';
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
      exit;
    end;
  end;
  Result := nil;

  setStudentFields(Result, False);
end;

function TDBConnection.getStudentsByLDAPUserPattern(ldap_user: string): ArrayOfStudents;
begin
  DBError := nil;
  SQLQuery.Close;
  SQLQuery.SQL.Text := 'SELECT FROM student WHERE LOWER(ldap_user) LIKE LOWER(:ldap_user)';
  SQLQuery.ParamByName('ldap_user').AsString := ldap_user;

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
      exit;
    end;
  end;
  Result := nil;

  setStudentFields(Result, False);
end;

function TDBConnection.getStudentWhoRentedBook(book: TBook): TStudent;
var
  arr: ArrayOfStudents;
begin
  DBError := nil;
  try
    with SQLQuery do
    begin
      SQLQuery.Close;
      SQLQuery.SQL.Text :=
        'SELECT r.student_id, r.return_date, s.* FROM rental r JOIN student s ON r.student_id = s.id WHERE r.book_id = (:book_id) AND r.return_date IS NULL LIMIT 1';
      SQLQuery.ParamByName('book_id').AsLargeInt := book.getId;
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
  if (length(arr) = 1) then
    Result := arr[0];
end;

function TDBConnection.getStudentsByFistLastClassNameBirthdate(
  fname, lname, cname: string; birth: TDate): ArrayOfStudents;
begin
  DBError := nil;
  SQLQuery.Close;

  try
    with SQLQuery do
    begin

      if birth = SQLNull then
      begin
        if cname = '' then
        begin
          SQLQuery.SQL.Text :=
            'SELECT * FROM student WHERE first_name = (:fname) AND last_name = (:lname)';
          SQLQuery.ParamByName('fname').AsString := fname;
          SQLQuery.ParamByName('lname').AsString := lname;
        end
        else
        begin
          SQLQuery.SQL.Text :=
            'SELECT * FROM student WHERE first_name = (:fname) AND last_name = (:lname) AND class_name = (:cname)';
          SQLQuery.ParamByName('fname').AsString := fname;
          SQLQuery.ParamByName('lname').AsString := lname;
          SQLQuery.ParamByName('cname').AsString := cname;
        end;
      end
      else
      begin
        if cname = '' then
        begin
          SQLQuery.SQL.Text :=
            'SELECT * FROM student WHERE first_name = (:fname) AND last_name = (:lname) AND birth = (:birthdate)';
          SQLQuery.ParamByName('fname').AsString := fname;
          SQLQuery.ParamByName('lname').AsString := lname;
          SQLQuery.ParamByName('birthdate').AsDate := birth;
        end
        else
        begin
          SQLQuery.SQL.Text :=
            'SELECT * FROM student WHERE first_name = (:fname) AND last_name = (:lname) AND birth = (:birthdate) AND cname = (:cname)';
          SQLQuery.ParamByName('fname').AsString := fname;
          SQLQuery.ParamByName('lname').AsString := lname;
          SQLQuery.ParamByName('cname').AsString := cname;
          SQLQuery.ParamByName('birthdate').AsDate := birth;
        end;
      end;
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


function TDBConnection.updateInsertStudent(var student: TStudent): boolean;
begin
  DBError := nil;
  SQLQuery.Close;
  SQLQuery.SQL.Text := 'SELECT * FROM student WHERE id = (:id) LIMIT 1';
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
      FieldByName('id').AsLargeInt := student.getId;
      FieldByName('last_name').AsString := student.getLastName;
      FieldByName('first_name').AsString := student.getFirstName;
      FieldByName('class_name').AsString := student.getClassName;

      if student.getBirth = SQLNull then //if set to null
        FieldByName('birth').Clear
      else
        FieldByName('birth').AsDateTime := student.getBirth;

      if student.getLDAPUser = '' then //if set to empty string meaning null
        FieldByName('ldap_user').Clear
      else
        FieldByName('ldap_user').AsString := student.getLDAPUser;

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

function TDBConnection.deleteStudent(var student: TStudent): boolean;
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
      student.Free;
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

        if not (FieldByName('return_date').IsNull) then
          resultVar[length(resultVar) - 1].setReturnDate(
            StrToDate(FieldByName('return_date').AsString));

        resultVar[length(resultVar) - 1].setRentalDate(
          StrToDate(FieldByName('rental_date').AsString));
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

function TDBConnection.getAllRentalsByBookAndStudent(var student: TStudent;
  var book: TBook): ArrayOfRentals;
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

function TDBConnection.existsUnreturnedRentalByBook(var book: TBook): boolean;
begin
  DBError := nil;
  SQLQuery.Close;
  try
    with SQLQuery do
    begin
      SQL.Text :=
        'SELECT * FROM rental where book_id = (:book) and return_date = NULL';
      ParamByName('book').AsLargeInt := book.getId;
      Open;

      First; //set pointer to first result row
      Result := not EOF; //does one exist?
    end;

  except
    on E: Exception do
    begin
      DBError := E;
      Result := False;
      exit;
    end;
  end;
end;

function TDBConnection.updateInsertRental(var rental: TRental): boolean;
begin
  DBError := nil;
  SQLQuery.Close;
  //get object from database if exists
  SQLQuery.SQL.Text := 'SELECT * FROM rental WHERE id = (:id) LIMIT 1';
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

      if rental.getReturnDate = SQLNull then //if set to null
        FieldByName('return_date').Clear
      else
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

function TDBConnection.deleteRental(var rental: TRental): boolean;
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
      rental.Free;
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
    'delete from rental where return_date not null and date(return_date) <= date(:date)';
  SQLQuery.ParamByName('date').AsDate := date;

  try
    with SQLQuery do
    begin
      SQLQuery.ExecSQL;
      SQLTransaction.commit;

      Close;
      SQLQuery.ExecSQL;
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

        if not (FieldByName('condition').IsNull) then
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

function TDBConnection.getBookById(id: LargeInt): TBook;
var
  arr: ArrayOfBooks;
begin
  DBError := nil;
  try
    with SQLQuery do
    begin
      SQLQuery.Close;
      SQLQuery.SQL.Text := 'SELECT * FROM book WHERE id = (:id) LIMIT 1';
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
  if (length(arr) > 0) then
    Result := arr[0];
end;

function TDBConnection.updateInsertBook(var book: TBook): boolean;
begin
  DBError := nil;
  SQLQuery.Close;
  SQLQuery.SQL.Text := 'SELECT * FROM book WHERE id = (:id) LIMIT 1';
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

function TDBConnection.deleteBook(var book: TBook): boolean;
begin
  DBError := nil;
  SQLQuery.Close;
  SQLQuery.SQL.Text := 'delete from book where id = (:id)';
  SQLQuery.ParamByName('id').AsLargeInt := book.getId;


  try
    with SQLQuery do
    begin
      ExecSQL;
      SQLTransaction.commit;
      book.Free;
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

        if not (FieldByName('storage').IsNull) then
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

function TDBConnection.updateInsertBooktype(var booktype: TBooktype): boolean;
begin
  DBError := nil;
  SQLQuery.Close;
  SQLQuery.SQL.Text := 'SELECT * FROM booktype WHERE isbn = (:isbn) LIMIT 1';
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

function TDBConnection.deleteBooktype(var booktype: TBooktype): boolean;
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
      booktype.Free;
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
      SQLQuery.SQL.Text := 'SELECT * FROM booktype WHERE isbn = (:isbn) LIMIT 1';
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

  if (length(arr) = 1) then
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
