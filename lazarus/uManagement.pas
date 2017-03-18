unit uManagement;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, sqlite3conn, sqldb, student, DBConnection,
  book, booktype, rental, dateutils;

type

  TManagement = class

    //Methoden
  public
    //Erg: Initialisierung der Datenbankverwaltung
    constructor Create();

    //Erg: Säubert Überreste des Programms
    destructor Destroy;

    // Überprüft die Verbindung zur Datenbank
    // Wahr wenn verbunden
    function isConnected: boolean;



    //BOOK                                                --------------------

    //Vor: Buch Id und Schüler Id
    //Eff: Rückgabe eines Buches mit Schüler
    //Erg: False, wenn gescheitert
    function BBack(BId, SId: int64): boolean;

    //Vor: Ein Buch mittels TBook-Objekt
    //Eff: Löscht ein Buch aus dem Bestand
    //Erg: Wahr wenn erfolgreich
    function BDel(var book: TBook): boolean;

    //Vor: Eine Buch Id
    //Eff: Überprüft, ob eine Buch Id bereits vergeben ist
    //Erg: Wahr, wenn Buch Id vergeben
    function BIdCheck(BId: int64): boolean;

    //Vor: ISBN nur mit Zahlen
    //Eff: Hinzufügen eines neuen Buches
    //Erg: Id des neu hinzugefügten Buches; -1 bei einem Fehler
    function BNew(isbn: string): int64;

    //Vor: Eine Buch Id
    //Eff: Überprüft die Buchqualität
    //Erg: Die Buchqualität
    function BQualiCheck(BId: int64): int64;

    //Vor: Buch Id und seine Qualität
    //Eff: Ändert die Buchqualität
    //Erg: Wahr wenn erfolgreich
    function BQualiNew(BId, quali: int64): boolean;

    //Eff: Überschreibt die Daten des Buches mit der übergebenen Id in der
    //     Datenbank mit den übergebenen Werten
    //Erg: Wenn Buch nicht vorhanden wahr -> False
    function BUpdate(var book: TBook): boolean;

    //Vor: Eine Buch Id
    //Erg: Gibt den Titel des Buches wieder
    function getBTitleById(BId: int64): string;

    //Vor: Buch Id
    //Erg: Das Buch-Object mit der Id
    function getBookByID(BID: int64): TBook;




    //BOOKTYPE                                            --------------------

    //Vor: Die ISBN
    //Eff: Prüft den Buchtyp
    //Erg: Wahr, wenn isbn bereits vorhanden
    function BTypeCheck(isbn: string): boolean;

    //Vor: isbn nur mit Zahlen, Titel und Fach des Buches, isbn darf nicht existieren
    //Eff: Neuer Buchtyp
    //Erg: Wahr wenn erfolgreich; Falsch wenn Fehler oder Buchtyp bereits vorhanden
    function BTypeNew(isbn, title, subject: string): boolean;

    //Eff: Überschreibt die Daten des Buchtyps mit der übergebenen Id in der
    //     Datenbank mit den übergebenen Werten
    //Erg: Wenn Buchtyp nicht vorhanden wahr -> False
    function BTypeUpdate(var Booktype: TBooktype): boolean;

    //Vor: ISBM als String ohne Bindestriche
    //Erg: Das Booktype-Objekt mit der übergebenen ISBN; Nil wenn nicht vorhanden
    function getBooktypeByISBN(isbn: string): TBooktype;




    //RENTAL                                              --------------------

    //Vor: Eine Datum, bis wohin der Verlauf des Verleihs gelöscht werden soll
    //Eff: Löscht jeden Verleih, welches Rückgabedatum kleiner gleich ist als das Datum
    //Erg: Anzahl der gelöschten Objekte; -1 bei einem Fehler
    function RDel(datum: TDate): integer;

    //Vor: Ein Rental-Objekt, dass gelöscht werden soll
    //Eff: Löscht das Rental Objekt
    //Erg: wahr wenn erfolgreich
    function RDelOne(var rental: TRental): boolean;

    //Vor: Buch Id und Schüler Id
    //Eff: Neue Vergabe eines Buches
    //Erg: Wahr wenn es geklappt hat, falsch wenn rental bereits vorhanden
    function RNew(BId, SId: int64): boolean;




    //STUDENT                                            --------------------

    //Vor: Eine Schüler Id
    //Erg: Den Namen des Schülers ('Vorname Nachname')
    function getSNameById(id: int64): string;

    //Erg: Gibt ein Element vom Typ ArrayOfStudents zurück,
    //     welches alle Schüler beinhaltet
    function getStudents: ArrayOfStudents;

    //Vor: Die Schüler Id
    //Erg: Gibt ein Element vom Typ TStudent zurück,
    //     welches den Schüler mit der übergebnen Id beinhaltet
    function getStudentById(id: int64): TStudent;

    //Vor: Den Klassennamen
    //Erg: Gibt ein Element vom Typ ArrayOfStudents zurück,
    //     welches alle Schüler in der Klasse beinhaltet
    function getStudentsByClassName(classN: string): ArrayOfStudents;

    //Vor: Den Vornamen
    //Erg: Gibt ein Element vom Typ ArrayOfStudents zurück,
    //     welches alle Schüler mit dem übergebenen Vornamen beinhaltet
    function getStudentsByFirstNamePattern(firstName: string): ArrayOfStudents;

    //Vor: LDAP Benutzernamen
    //Erg: Array von TStudent-Objekte mit dem Benutzernamen
    function getStudentByLDAPUserPattern(ldap_user: string): ArrayOfStudents;

    //Vor: Den Nachnamen
    //Erg: Gibt ein Element vom Typ ArrayOfStudents zurück,
    //     welches alle Schüler mit dem übergebenen Nachnamen beinhaltet
    function getStudentsByLastNamePattern(lastName: string): ArrayOfStudents;

    //Vor: den Vorname, Nachnamen, Klassennamen und Geburtsdatum
    //     Geburtsdatum darf ourNil sein; cname darf '' sein, um keine Werte zu übergeben
    //Erg: Ein Array von TStudent-Objekten mit den Daten
    function getStudentsByFirstLastClassNameBirthdate(fname, lname, cname: string;
      birth: TDate): ArrayOfStudents;

    // Importiert die Schüler Liste als CSV Datei
    // Klasse; Name; Vorname; Geburtsdatum
    // Datei mit dem Namen Dateiname muss im UNterverzeichnis liegen
    // Wahr wenn erfolgreich
    function importCSVSchueler(Dateiname: string): boolean;

    //Vor: Eine Schüler mittels TStudent-Objekt
    //Eff: Löscht einen Schüler
    //Erg: Wahr wenn erfolgreich
    function SDel(var student: TStudent): boolean;

    //Vor: Eine Schüler Id
    //Eff: Überprüft, ob eine Schüler Id bereits vergeben ist
    //Erg: Wahr, wenn vergeben
    function SIdCheck(SId: int64): boolean;

    //Vor: Nachname, Vorname und Klassenname, Geburtsdatum als TDate
    //Eff: Neuen Schüler erstellen
    //Erg: Die Schüler Id
    function SNew(lastN, firstN, classN: string; birth: TDate): int64;

    //Eff: Überschreibt die Daten des Schülers mit der übergebenen Id in der
    //     Datenbank mit dem Übergebenen Schüler
    //Erg: Wenn Schüler nicht vorhanden wahr -> False
    function SUpdate(var student: TStudent): boolean;




    //Atribute
  private
    uDBConn: TDBConnection;         //Element für Verbindung zur DBConnection

  end;

implementation

const
  ourNil = -693594;

constructor TManagement.Create();
begin
  //Initialisierung
  uDBConn := TDBConnection.Create('buchverleih.sqlite');


  Randomize;
end;

destructor TManagement.Destroy;
begin
  uDBConn.Destroy;
end;

function TManagement.isConnected(): boolean;
begin
  Result := uDBConn.isConnected;
end;




//---------------------------------------------------------------BOOK-------

function TManagement.BBack(BId, SId: int64): boolean;
var
  aoR: ArrayOfRentals;
  rental: TRental;
  book: TBook;
  student: TStudent;
begin
  book := self.getBookByID(BID);
  student := self.getStudentById(SId);
  if not ((book = nil) and (student = nil)) then
  begin
    aoR := uDBConn.getAllRentalsByBookAndStudent(student, book);
    rental := aoR[0];
    rental.setReturnDate(now);

    Result := uDBConn.updateInsertRental(rental);
  end
  else
    Result := False;
end;

function TManagement.BDel(var book: TBook): boolean;
begin
  if not (book = nil) then
    Result := uDBConn.deleteBook(book)
  else
    Result := False;
end;

function TManagement.BIdCheck(BId: int64): boolean;
var
  book: TBook;
begin
  book := uDBConn.getBookById(BId);
  if book = nil then
    Result := False
  else
    Result := True;
end;

function TManagement.BNew(isbn: string): int64;
var
  id, pz: int64;
  hid: string;
  book: TBook;
begin
  repeat
    id := Random(5000000) + 3000001; //Bereich von 3Mio1 bis 8Mio1

    hid := IntToStr(id);
    //hid ist eine Hilfsvariable zur Prüfnummererstellung
    pz := ((StrToInt(hid[1]) * 3) + (StrToInt(hid[3]) * 3) +
      (StrToInt(hid[5]) * 3) + (StrToInt(hid[7]) * 3) + StrToInt(hid[2]) +
      StrToInt(hid[4]) + StrToInt(hid[6])) mod 10;
    //Die Prüfziffer Teil 1
    if pz = 10 then
      pz := 0;
    id := (id * 10) + (10 - pz);
  until BIdCheck(id) = False;            //Wiederholung bis id nicht vergeben

  book := TBook.Create;

  book.setId(id);
  book.setIsbn(isbn);
  book.setCondition(1);

  if not (uDBConn.updateinsertBook(book)) then
    Result := -1
  else
    Result := id;
end;

function TManagement.BQualiCheck(BId: int64): int64;
var
  book: TBook;
begin
  book := uDBConn.getBookById(BId);
  Result := book.getCondition();
end;

function TManagement.BQualiNew(BId, quali: int64): boolean;
var
  book: TBook;
begin
  book := self.getBookById(BId);
  if not (book = nil) then
  begin
    book.setCondition(quali);
    Result := uDBConn.updateInsertBook(book);
  end
  else
    Result := False;
end;

function TManagement.BUpdate(var book: TBook): boolean;
begin
  if not (book = nil) and self.BIdCheck(book.getId()) then
    Result := uDBConn.updateInsertBook(book)
  else
    Result := False;
end;

function TManagement.getBTitleById(BId: int64): string;
var
  book: TBook;
  booktype: TBooktype;
begin
  book := self.getBookByID(BId);
  booktype := uDBConn.getBooktypeByIsbn(book.getIsbn());
  Result := booktype.getTitle();
end;

function TManagement.getBookByID(BID: int64): TBook;
begin
  Result := uDBConn.getBookById(bid);
end;

//---------------------------------------------------------------BOOKTYPE---

function TManagement.BTypeCheck(isbn: string): boolean;
var
  bt: TBooktype;
begin
  bt := uDBConn.getBooktypeByIsbn(isbn);
  if bt = nil then
    Result := False
  else
    Result := True;
end;

function TManagement.BTypeNew(isbn, title, subject: string): boolean;
var
  booktype: TBooktype;
begin
  if not (self.BTypeCheck(isbn)) then
  begin
    booktype := TBooktype.Create;
    booktype.setIsbn(isbn);
    booktype.setTitle(title);
    booktype.setSubject(subject);

    Result := uDBConn.updateinsertBooktype(booktype);
  end
  else
    Result := False;
end;

function TManagement.BTypeUpdate(var booktype: TBooktype): boolean;
begin
  if not (booktype = nil) and self.BTypeCheck(booktype.getIsbn()) then
    Result := uDBConn.updateInsertBooktype(booktype)
  else
    Result := False;
end;

function TManagement.getBooktypeByISBN(isbn: string): TBooktype;
begin
  Result := uDBConn.getBooktypeByISBN(isbn);
end;

//---------------------------------------------------------------RENTAL-----

function TManagement.RDel(datum: TDate): integer;
begin
  Result := uDBConn.deleteReturnedRentalOlderThan(datum);
end;

function TManagement.RDelOne(var rental: TRental): boolean;
begin
  if not (rental = nil) then
    Result := uDBConn.deleteRental(rental)
  else
    Result := False;
end;

function TManagement.RNew(BId, SId: int64): boolean;
var
  rentals: array of TRental;
  rental: TRental;
  book: TBook;
  student: TStudent;
begin
  book := self.getBookByID(BID);
  student := self.getStudentById(SId);
  if not ((book = nil) and (student = nil)) then
  begin
    rentals := uDBConn.getAllRentalsByBookAndStudent(student, book);
    if (length(rentals) = 0) then
    begin
      rental := TRental.Create;

      rental.setBookId(BId);
      rental.setStudentId(SId);
      rental.setRentalDate(now);
      rental.setReturnDate(ourNil);

      Result := uDBConn.updateinsertRental(rental);
    end
    else
      Result := False;
  end
  else
    Result := False;
end;

//---------------------------------------------------------------STUDENT----

function TManagement.getSNameById(id: int64): string;
var
  res: string;
  student: TStudent;
begin
  student := self.getStudentById(id);
  res := student.getFirstName() + ' ' + student.getLastName();
  Result := res;
end;

function TManagement.getStudents(): ArrayOfStudents;
begin
  Result := uDBConn.getStudents;
end;

function TManagement.getStudentById(id: int64): TStudent;
begin
  Result := uDBConn.getStudentById(id);
end;

function TManagement.getStudentsByClassName(classN: string): ArrayOfStudents;
begin
  Result := uDBConn.getStudentsByClassName(classN);
end;

function TManagement.getStudentsByFirstNamePattern(firstName: string): ArrayOfStudents;
begin
  Result := uDBConn.getStudentsByFirstNamePattern(firstName);
end;

function TManagement.getStudentByLDAPUserPattern(ldap_user: string): ArrayOfStudents;
begin
  Result := uDBConn.getStudentsByLDAPUserPattern(ldap_user);
end;

function TManagement.getStudentsByLastNamePattern(lastName: string): ArrayOfStudents;
begin
  Result := uDBConn.getStudentsByLastNamePattern(lastName);
end;

function TManagement.getStudentsByFirstLastClassNameBirthdate(
  fname, lname, cname: string; birth: TDate): ArrayOfStudents;
var
  students, students2, students3: array of TStudent;
  indexS3, j, k, id: integer;
begin
  indexS3 := 0;
  students := uDBConn.getStudentsByFirstNamePattern(fname);   // Nach Vorname

  students2 := uDBConn.getStudentsByLastNamePattern(lname);   //Name

  if not (length(students) = 0) and not (length(students2) = 0) then
  begin
    j := 0;
    while length(students) > j + 1 do
    begin
      id := students[j].getId;

      k := 0;
      while length(students2) > k + 1 do
      begin
        if id = students2[k].getId then
        begin
          students3[indexS3] := students2[k];
          indexS3 := indexS3 + 1;
        end;

        k := k + 1;
      end;
      j := j + 1;
    end;
  end
  else if (length(students) = 0) then
    students3 := students2
  else if (length(students2) = 0) then
    students3 := students;


  if not (birth = -693594) then
  begin
    students := uDBConn.getStudentsByBirthdate(birth);        //Geburtsdatum
    indexS3 := 0;

    if not (length(students) = 0) and not (length(students3) = 0) then
    begin
      j := 0;
      while length(students) > j + 1 do
      begin
        id := students[j].getId;
        k := 0;
        while length(students3) > k + 1 do
        begin
          if id = students3[k].getId then
          begin
            students2[indexS3] := students3[k];
            indexS3 := indexS3 + 1;
          end;
          k := k + 1;
        end;
        j := j + 1;
      end;
    end
    else if (length(students) = 0) then
      students2 := students3
    else if (length(students3) = 0) then
      students2 := students;
  end
  else
  begin
    students2 := students3;
  end;

  if not (cname = '') then
  begin
    students := uDBConn.getStudentsByClassName(cname);     //Klassenname
    indexS3 := 0;

    if not (length(students) = 0) and not (length(students2) = 0) then
    begin
      j := 0;
      while length(students) > j + 1 do
      begin
        id := students[j].getId;
        k := 0;
        while length(students2) > k + 1 do
        begin
          if id = students2[k].getId then
          begin
            students3[indexS3] := students2[k];
            indexS3 := indexS3 + 1;
          end;
          k := k + 1;
        end;
        j := j + 1;
      end;
      students2 := students3;
    end
    else if (length(students) = 0) then
      students2 := students3
    else if (length(students3) = 0) then
      students2 := students;
  end;

  Result := students2;
end;

function TManagement.importCSVSchueler(Dateiname: string): boolean;
var
  Text: TextFile;
  str, fname, lname, cname, birth: string;
  birthDate: TDate;
  i: integer;
  students: array of TStudent;
begin
  str := '';
  fname := '';
  lname := '';
  cname := '';
  birth := '';

  AssignFile(Text, Dateiname);

  try
    reset(Text);


    while not EOF(Text) do                //Schüler zeilenweise einlesen
    begin
      readln(Text, str);

      i := 1;
      while not (str[i] = ';') do        //Klassennamen einlesen
      begin
        cname := cname + str[i];
        i := i + 1;
      end;

      i := i + 1;
      while not (str[i] = ';') do      //Namen einlesen
      begin
        lname := lname + str[i];
        i := i + 1;
      end;

      i := i + 1;
      while not (str[i] = ';') do     //Vornamen einlesen
      begin
        fname := fname + str[i];
        i := i + 1;
      end;

      i := i + 1;
      while (not (str[i] = ';') and (length(str) >= i + 1)) do
      begin                          //Geburtsdatum einlesen
        birth := birth + str[i];
        i := i + 1;
      end;
      birthDate := StrToDate(birth, '-');
      // Vergleich ob bereits vorhanden
      students := self.getStudentsByFirstLastClassNameBirthdate(fname,
        lname, '', birthDate);

      //Verarbeitung
      if (length(students) = 0) then
      begin                 //Fall Einfügen eines neuen Schülers

        self.SNew(lname, fname, cname, birthDate);
        Result := True;

      end
      else if (length(students) = 1) then
      begin                 //Fall Klasse überschreiben

        students[0].setClassName(cname);
        Result := uDBConn.updateinsertStudent(students[0]);

      end
      else
      begin                  //Fall Ein Fehler liegt vor
        Result := False;
        break;
      end;

    end;
  except
    on E: Exception do
      Result := False;

  end;
  CloseFile(Text);

end;

function TManagement.SDel(var student: TStudent): boolean;
begin
  if not (student = nil) then
    Result := uDBConn.deleteStudent(student)
  else
    Result := False;
end;

function TManagement.SIdCheck(SId: int64): boolean;
begin
  if (uDBConn.getStudentById(SId) = nil) then
    Result := False
  else
    Result := True;
end;

function TManagement.SNew(lastN, firstN, classN: string; birth: TDate): int64;
var
  id, pz: int64;
  hid: string;
  student: TStudent;
begin
  pz := 0;

  //if (self.getStudentsByFirstLastClassNameBirthdate(firstN, lastN, birth)

  repeat
    id := Random(2000000) + 1000000; //Bereich von 1Mio bis 3 Mio

    hid := IntToStr(id); //hid ist eine Hilfsvariable zur Prüfnummererstellung

    pz := ((StrToInt(hid[1]) * 3) + (StrToInt(hid[3]) * 3) +
      (StrToInt(hid[5]) * 3) + (StrToInt(hid[7]) * 3) + StrToInt(hid[2]) +
      StrToInt(hid[4]) + StrToInt(hid[6])) mod 10;
    //Die Prüfziffer Teil 1
    if pz = 10 then
      pz := 0;
    id := (id * 10) + pz;                  //Die Prüfziffer wird hinten angehangen
  until SIdCheck(id) = False;            //Wiederholung bis id nicht vergeben

  student := TStudent.Create;
  student.setId(id);
  student.setFirstName(firstN);
  student.setLastName(lastN);
  student.setClassName(classN);
  student.setBirth(birth);

  self.SUpdate(student);
  Result := id;
end;

function TManagement.SUpdate(var student: TStudent): boolean;
begin
  if not (student = nil) and self.SIdCheck(student.getId()) then
    Result := uDBConn.updateinsertStudent(student)
  else
    Result := False;
end;

end.
