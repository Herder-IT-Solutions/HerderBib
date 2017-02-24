unit uDBManagement;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqlite3conn, sqldb, student, DBConnection, book, booktype, rental;

type

  TDBManagement = class

  //Methoden
  public
    //Erg: Initialisierung der Datenbankverwaltung
    constructor create();

    //Erg: Säubert Überreste des Programms
    destructor Destroy;

    //Vor: Eine Buch Id
    //Eff: Überprüft, ob eine Buch Id bereits vergeben ist
    //Erg: Wahr, wenn Buch Id vergeben
    function BIdCheck(BId :Cardinal):Boolean;

    //Vor: Eine Buch Id
    //Eff: Überprüft die Buchqualität
    //Erg: Die ehemalige Quaität
    function BQualiCheck(BId: Cardinal): Cardinal;

    //Vor: Die ISBN
    //Eff: Prüft den Buchtyp
    //Erg: Wahr, wenn isbn bereits vorhanden
    function BTypeCheck (isbn:String): Boolean;

    //Erg: Gibt ein Element vom Typ ArrayOfStudents zurück,
    //     welches alle Schüler beinhaltet
    function getStudents: ArrayOfStudents;

    //Vor: Den Vornamen
    //Erg: Gibt ein Element vom Typ ArrayOfStudents zurück,
    //     welches alle Schüler mit dem übergebenen Vornamen beinhaltet
    function getStudentsByFirstNamePattern(firstName: string): ArrayOfStudents;

    //Vor: Den Nachnamen
    //Erg: Gibt ein Element vom Typ ArrayOfStudents zurück,
    //     welches alle Schüler mit dem übergebenen Nachnamen beinhaltet
    function getStudentsByLastNamePattern(lastName: string): ArrayOfStudents;

    //Vor: Den Klassennamen
    //Erg: Gibt ein Element vom Typ ArrayOfStudents zurück,
    //     welches alle Schüler in der Klasse beinhaltet
    function getStudentsByClassName(classN: string): ArrayOfStudents;

    //Vor: Die Schüler Id
    //Erg: Gibt ein Element vom Typ TStudent zurück,
    //     welches den Schüler mit der übergebnen Id beinhaltet
    //function getStudentById(id: Cardinal): TStudent;

    //Eff: Überschreibt die Daten des Schülers mit der übergebenen Id in der
    //     Datenbank mit dem Übergebenen Schüler
    //Erg: Wenn Schüler nicht vorhanden wahr -> False
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
    procedure BookTypeNew(isbn, title, subject :String);

    //Vor: Buch Id und Schüler Id
    //Eff: Neue Vergabe eines Buches
    //Erg: Trägt in der Datenbank in Tabelle rental das ausgeliehene Buch zu dem Schüler ein
    procedure BookRentStu(BId, SId :Cardinal);

    //Vor: Nachname, Vorname und Klassenname, Geburtsdatum als TDate
    //Eff: Neuen Schüler erstellen
    //Erg: Neuer Schüler
    procedure NewStudent (lastN, firstN, classN : String; birth: TDate);

    //Vor: Eine Schüler Id
    //Eff: Löscht einen Schüler
    procedure DelStudent(SId : Cardinal);

    //Vor: Eine Datum, bis wohin der Verlauf des Verleihs gelöscht werden soll
    //Eff: Löscht jeden Verleih, welches Rückgabedatum kleiner gleich ist als das Datum
    procedure DelRental(datum: TDate);  //bzw TDateTime

  //Atribute
  private
    uDBConn : TDBConnection;         //Element für Verbindung zur DBConnection

end;

implementation

constructor TDBManagement.create();
begin
  //Initialisierung
  uDBConn := TDBConnection.Create('buchverleih.sqlite');

  Randomize;
end;

destructor TDBManagement.Destroy;
begin
  uDBConn.Destroy;
end;

function TDBManagement.BIdCheck(BId :Cardinal):Boolean;
Var book : TBook;
begin
  book:=uDBConn.getBookById(BId);
  if book = nil then result:=False
  else result :=true;
end;

function TDBManagement.BQualiCheck(BId: Cardinal): Cardinal;
Var book : TBook;
begin
  book := uDBConn.getBookById(BId);
  result:=book.getCondition();
end;

function TDBManagement.BTypeCheck (isbn:String): Boolean;
Var bt : TBooktype;
begin
  bt:=uDBConn.getBooktypeByIsbn(isbn);
  if bt = nil then result:=false
  else result:=true;
end;

function TDBManagement.getStudents(): ArrayOfStudents;
begin
  Result := uDBConn.getStudents;
end;

function TDBManagement.getStudentsByFirstNamePattern(firstName: string): ArrayOfStudents;
begin
  Result := uDBConn.getStudentsByFirstNamePattern(firstName);
end;

function TDBManagement.getStudentsByLastNamePattern(lastName: string): ArrayOfStudents;
begin
  Result := uDBConn.getStudentsByLastNamePattern(lastName);
end;

function TDBManagement.getStudentsByClassName(classN: string): ArrayOfStudents;
begin
  Result := uDBConn.getStudentsByClassName(classN);
end;

function TDBManagement.persistStudent(student: TStudent): boolean;
begin
  Result:=uDBConn.persistStudent(student);
end;

function TDBManagement.SIdCheck(SId :Cardinal):Boolean;
begin
  if (uDBConn.getStudentById(SId) = nil) then Result:= false
  else Result:=true;
end;

procedure TDBManagement.BookBack(BId, SId :Cardinal);
Var aoR :ArrayOfRentals;
    rental : TRental;
begin
  aoR := uDBConn.getAllRentalsByBookAndStudent(uDBConn.getStudentById(SId), uDBConn.getBookById(BId));
  rental := aoR[1];
  rental.setReturnDate(now);
end;

procedure TDBManagement.BookAdd(isbn : String);
Var id, pz: Cardinal;
    hid :String;
    book :TBook;
begin
  repeat
     id:= Random(5000000) + 3000001; //Bereich von 3Mio1 bis 8Mio1

     hid:=inttostr(id);              //hid ist eine Hilfsvariable zur Prüfnummererstellung
     pz:= ((strtoint(hid[1])*3) + (strtoint(hid[3])*3) + (strtoint(hid[5])*3) + (strtoint(hid[7])*3) + strtoint(hid[2]) + strtoint(hid[4]) + strtoint(hid[6]))Mod 10; //Die Prüfziffer Teil 1
     if pz = 10 then pz := 0;
     id:=(id*10)+ (10-pz);
  until BIdCheck(id)=false;            //Wiederholung bis id nicht vergeben

  book := TBook.Create;

  book.setId(id);
  book.setIsbn(isbn);
  book.setCondition(1);

  uDBConn.persistBook(book);
end;

procedure TDBManagement.BookDel(BId:Cardinal);
begin
  uDBConn.deleteBook(uDBConn.getBookById(BId));
end;

procedure TDBManagement.BookQualiNew(BId, quali :Cardinal);
Var book:TBook;
begin
  book:=uDBConn.getBookById(BId);
  book.setCondition(quali);
end;

procedure TDBManagement.BookTypeNew(isbn, title, subject :String);
Var booktype : TBooktype;
begin
  booktype := TBooktype.Create;
  booktype.setIsbn(isbn);
  booktype.setTitle(title);
  booktype.setSubject(subject);

  uDBConn.persistBooktype(booktype);
end;

procedure TDBManagement.BookRentStu(BId, SId :Cardinal);                                //!
Var rental: TRental;
begin
  rental := TRental.Create;

  rental.setBookId(BId);
  rental.setStudentId(SId);
  rental.setRentalDate(now);
end;

procedure TDBManagement.DelStudent(SId : Cardinal);
begin
  uDBConn.deleteStudent(uDBConn.getStudentById(SID));
end;

procedure TDBManagement.DelRental(datum: TDate);                                  //!
begin
  {query.Close;
  query.SQL.Text:='Delete from rental where return_date <= (:date)';
  query.ParamByName('date').AsDate:=datum;
  query.ExecSQL;
  tran.Commit;   }
end;

procedure TDBManagement.NewStudent (lastN, firstN, classN : String; birth: TDate);
Var id, pz: Cardinal;
    hid: String;
    student: TStudent;
begin
  repeat
     id:= Random(2000000) + 1000000; //Bereich von 1Mio bis 3 Mio

     hid:=inttostr(id);              //hid ist eine Hilfsvariable zur Prüfnummererstellung
     pz:= ((strtoint(hid[1])*3) + (strtoint(hid[3])*3) + (strtoint(hid[5])*3) + (strtoint(hid[7])*3) + strtoint(hid[2]) + strtoint(hid[4]) + strtoint(hid[6]))Mod 10; //Die Prüfziffer Teil 1
     if pz = 10 then pz := 0;
     id:=(id*10)+ (10-pz);
  until SIdCheck(id)=false;            //Wiederholung bis id nicht vergeben

  student := TStudent.Create;
  student.setId(id);
  student.setFirstName(firstN);
  student.setLastName(lastN);
  student.setClassName(classN);
  student.setBirth(birth);

  self.persistStudent(student);
end;

end.
