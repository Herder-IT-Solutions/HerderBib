unit uVerwaltung;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqlite3conn, sqldb, student;

type
  ArrayOfStudents = array of TStudent;

  TVerwaltung = class
  //Methoden
  public
    //Erg: Initialisierung der Datenbankverwaltung
    constructor create(qy :TSQLQuery; ta : TSQLTransaction; cn : TSQLite3Connection);

    //destructor am Ende

    //Vor: Eine Buch Id
    //Eff: Überprüft, ob eine Buch Id bereits vergeben ist
    //Erg: Wahr, wenn vergeben
    function BIdCheck(BId :Cardinal):Boolean;

    //Vor: Eine Buch Id
    //Eff: Überprüft die Buchqualität
    //Erg: Die ehemalige Quaität
    function BQualiCheck(BId: Cardinal): Cardinal;

    //Vor: Die ISBN
    //Eff: Prüft den Buchtyp
    //Erg: Wahr, wenn isbn bereits vorhanden
    function BTypeCheck (isbn:String): Boolean;

    function getRentals: ArrayOfStudent;

    // Returns an array of all students                   !
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

    //Vor: Eine Schüler Id
    //Eff: Überprüft, ob eine Schüler Id bereits vergeben ist
    //Erg: Wahr, wenn vergeben
    function SIdCheck(SId :Cardinal):Boolean;

    //Vor: Buch Id und Schüler Id
    //Eff: Rückgabe eines Buches mit Schüler
    //Erg: Trägt aktuelles Datum (als Double (nicht lesbar)) als Rückgabedatum in die Datenbank ein
    procedure BookBack(BId, SId :Cardinal);

    //Vor: ISBN nur mit Zahlen
    //Eff: Hinzufügen eines neuen Buches
    //Erg: Buch in Tabelle book
    procedure BookAdd(isbn : String);

    //Vor: Die Buch Id
    //Eff: Löscht ein Buch aus dem Bestand
    procedure BookDel(BId:Cardinal);

    //Vor: Buch Id und seine Qualität
    //Eff: Ändert die Buchqualität
    //Erg: Trägt übergebene Qualität in die Datenbank ein
    procedure BookQualiNew(BId, quali :Cardinal);

    //Vor: isbn nur mit Zahlen, Titel und Fach des Buches, isbn darf nicht existieren
    //Eff: Neuer Buchtyp
    procedure BookTypeNew(isbn :String; title, subject :String);

    //Vor: Buch Id und Schüler Id
    //Eff: Neue Vergabe eines Buches
    //Erg: Trägt in procedure BuchTypHinzu(isbn, title, subject); der Datenbank in Tabelle rental das ausgeliehene Buch zu dem Schüler ein
    procedure BookRentStu(BId, SId :Cardinal);

    //Vor: Nachname, Vorname und Klassenname, Geburtsdatum als yyyymmtt (20101104)
    //Eff: Neuen Schüler erstellen
    //Erg: Neuer Schüler
    procedure NewStudent (lastN, firstN, classN : String; birth: Cardinal);

    //Vor: Eine Schüler Id
    //Eff: Löscht einen Schüler
    procedure DelStudent(SId : Cardinal);

    //Vor: Eine Datum, bis wohin der Verlauf des Verleihs gelöscht werden soll
    //Eff: Löscht jeden Verleih, welches Rückgabedatum kleiner gleich ist als das Datum
    procedure DelRental(datum: TDate);  //bzw TDateTime

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

function TVerwaltung.BIdCheck(BId :Cardinal):Boolean;
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

function TVerwaltung.BQualiCheck(BId: Cardinal): Cardinal;
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

function TVerwaltung.BTypeCheck (isbn:String): Boolean;
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

function TDBConnection.getStudents: ArrayOfStudents;
begin                                                                    //!
  query.Close;
  query.SQL.Text := 'SELECT * FROM student';
  query.Open;

  Result := nil;

  try
    with query do
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

function TVerwaltung.SIdCheck(SId :Cardinal):Boolean;
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

procedure TVerwaltung.BookBack(BId, SId :Cardinal);
begin
  query.Close;
  query.SQL.text:='Update rental Set return_date = :Datum where student_id = :SId and book_id = :BId';
  query.ParamByName('Datum').AsDate:= now;                 //Vorher: FormatDateTime(..)
  query.ParamByName('BId').AsInteger:=BId;
  query.ParamByName('Sid').AsInteger:=SId;
  query.ExecSQL;
  tran.Commit;

end;

procedure TVerwaltung.BookAdd(isbn : String);
Var id: Cardinal;
begin
  repeat
     id:= Random(5000000) + 23000001; //Bereich von 23Mio1 bis 28Mio1
  until BIdCheck(id)=false;            //Wiederholung bis id nicht vergeben

  query.Close;
  query.SQL.Text:='Insert into book Values (:Id, :isbn, :con)';
  query.ParamByName('Id').AsInteger:=id;
  query.ParamByName('isbn').AsString:=isbn;
  query.ParamByName('con').AsInteger:=1;
  query.ExecSQL;
  tran.Commit;
end;

procedure TVerwaltung.BookDel(BId:Cardinal);
begin
  query.Close;
  query.SQL.Text:='Delete from book where Id = (:BId)';
  query.ParamByName('BId').AsInteger:=BId;
  query.ExecSQL;
  tran.Commit;
end;

procedure TVerwaltung.BookQualiNew(BId, quali :Cardinal);
begin
  query.Close;
  query.SQL.text:='Update book Set condition = :Quali where id = :BId';
  query.ParamByName('Quali').AsInteger:=quali;
  query.ParamByName('BId').AsInteger:=BId;
  query.ExecSQL;
  tran.Commit;
end;

procedure TVerwaltung.BookTypeNew(isbn, title, subject :String);
begin
  query.Close;
  query.SQL.Text:='Insert into booktype Values (:isbn, :title, :sub, NULL)';
  query.ParamByName('isbn').AsString:=isbn;
  query.ParamByName('title').AsString:=title;
  query.ParamByName('sub').AsString:=subject;
  query.ExecSQL;
  tran.Commit;
end;

procedure TVerwaltung.BookRentStu(BId, SId :Cardinal);
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
  until SIdCheck(id)=false;            //Wiederholung bis id nicht vergeben

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

procedure TVerwaltung.DelStudent(SId : Cardinal);
begin
  query.Close;
  query.SQL.Text:='Delete from student where Id = (:SId)';
  query.ParamByName('SId').AsInteger:=SId;
  query.ExecSQL;
  tran.Commit;
end;

procedure TVerwaltung.DelRental(datum: TDate);
begin
  query.Close;
  query.SQL.Text:='Delete from rental where return_date <= (:date)';
  query.ParamByName('date').AsDate:=datum;
  query.ExecSQL;
  tran.Commit;
end;

end.
