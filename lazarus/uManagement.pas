unit uManagement;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, sqlite3conn, sqldb, student, DBConnection,
  book, booktype, rental, dateutils, DBConstants;

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

    //Erg: Alle Buchtypen in der Datenbank
    function getBooktypes: ArrayOfBooktypes;




    //RENTAL                                              --------------------

    //Vor: Buch Objekt
    //Eff: Überprüft, ob ein Buch gerade Verliehen ist
    //Erg: Wahr, wenn Buch ausgeliehen oder Buch nicht existent
    function RCheckByBook(var book: TBook): boolean;

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
    //     Geburtsdatum darf SQLNull sein; cname darf '' sein, um keine Werte zu übergeben
    //Erg: Ein Array von TStudent-Objekten mit den Daten
    function getStudentsByFirstLastClassNameBirthdate(fname, lname, cname: string;
      birth: TDate): ArrayOfStudents;

    //Vor: Das TBook-Objekt
    //Erg: TStudent-Objekt des ausleihenden Schülers
    function getStudentWhoRentedBook(book: TBook): TStudent;

    //Vor: CSV-Format: Klasse; Name; Vorname; Geburtsdatum
    //     Datei mit dem Namen Dateiname muss im UNterverzeichnis liegen
    //     RnOld = True, wenn alle, die nicht in der Liste sind als Klasse 'Alt' bekommen sollen
    //Eff: Importiert die Schüler Liste als CSV Datei
    //Erg: Wahr wenn erfolgreich
    function importCSVSchueler(Dateiname: string; RnOld: boolean): boolean;

    //Vor: Eine Schüler mittels TStudent-Objekt
    //Eff: Löscht einen Schüler
    //Erg: Wahr wenn erfolgreich
    function SDel(var student: TStudent): boolean;

    //Vor: Eine Schüler Id
    //Eff: Überprüft, ob eine Schüler Id bereits vergeben ist
    //Erg: Wahr, wenn vergeben
    function SIdCheck(SId: int64): boolean;

    //Vor: Nachname, Vorname und Klassenname, Geburtsdatum als TDate
    //     Falls vorhanden ldap_user, ansonsten '' übergeben
    //Eff: Neuen Schüler erstellen
    //Erg: Die Schüler Id; -1 bei einem Fehler
    function SNew(lastN, firstN, classN, ldap_user: string; birth: TDate): int64;

    //Eff: Überschreibt die Daten des Schülers mit der übergebenen Id in der
    //     Datenbank mit dem Übergebenen Schüler
    //Erg: Wenn Schüler nicht vorhanden wahr -> False
    function SUpdate(var student: TStudent): boolean;




    //Atribute
  private
    uDBConn: TDBConnection;         //Element für Verbindung zur DBConnection

  end;

implementation

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
    if (length(aoR) > 0) then
    begin
      rental := aoR[0];
      rental.setReturnDate(now);

      Result := uDBConn.updateInsertRental(rental);
    end
    else
      Result := False;
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
    pz := (3 * ((StrToInt(hid[1])) + (StrToInt(hid[3])) + (StrToInt(hid[5])) +
      (StrToInt(hid[7]))) + StrToInt(hid[2]) + StrToInt(hid[4]) +
      StrToInt(hid[6])) mod 10;
    if pz <> 0 then
      pz := 10 - pz;
    //Die Prüfziffer Teil 1
    id := (id * 10) + pz;
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

function TManagement.getBooktypes: ArrayOfBooktypes;
begin
  Result := uDBConn.getBooktypes;
end;

//---------------------------------------------------------------RENTAL-----

function TManagement.RCheckByBook(var book: TBook): boolean;
begin
  if ((not (book = nil)) and (self.BIdCheck(book.getId()))) then
    Result := uDBConn.existsUnreturnedRentalByBook(book)
  else
    Result := True;
end;

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
  rental: TRental;
  book: TBook;
  student: TStudent;
begin
  book := self.getBookByID(BID);
  student := self.getStudentById(SId);
  if not ((book = nil) or (student = nil)) then
  begin
    rental := TRental.Create;

    rental.setBookId(BId);
    rental.setStudentId(SId);
    rental.setRentalDate(now);
    rental.setReturnDate(SQLNull);

    Result := uDBConn.updateinsertRental(rental);

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
begin
  Result := uDBConn.getStudentsByFistLastClassNameBirthdate(fname, lname, cname, birth);
end;

function TManagement.getStudentWhoRentedBook(book: TBook): TStudent;
begin
  Result := uDBConn.getStudentWhoRentedBook(book);
end;

function TManagement.importCSVSchueler(Dateiname: string; RnOld: boolean): boolean;

  //Vor: Array von TStudent-Objekten
  //Eff: Benennt den Klassennamen von allen Schülern, die nicht in der Liste,
  //     aber in der Datenbank sind, um in 'old'
  //Erg: Wahr, wenn erfolgreich
  function renameOld(allStu: ArrayOfStudents): boolean;
  var
    allDBStu, oldDBStu: ArrayOfStudents;
    i, j, ioldDB, DBid, lAll, lAllDB: integer;
    inside: boolean;

  begin
    ioldDB := 0;
    Result := True;

    //Schüler ermitteln, die nur in der Datenbank sind, nicht aber in dem übergebenen Array
    allDBStu := uDBConn.getStudents;
    lAll := length(allStu);
    lAllDB := length(allDBStu);

    for i := 0 to (lAllDB - 1) do
    begin
      DBid := allDBStu[i].getId();
      inside := False;
      for j := 0 to (lAll - 1) do
      begin
        if (DBid = allStu[j].getId) then
        begin
          inside := True;
          break;
        end;
      end;
      if not (inside) then
      begin
        ioldDB := ioldDB + 1;
        SetLength(oldDBStu, ioldDB);
        oldDBStu[ioldDB - 1] := allDBStu[i];
      end;
    end;

    //Klassennamen der Schüler zu 'old' ändern
    for i := 0 to (ioldDB - 1) do
    begin
      oldDBStu[i].setClassName('Alt');
      if not (uDBConn.updateInsertStudent(oldDBStu[i])) then
      begin
        Result := False;
        Break;
      end;
    end;
  end;

var
  Text: TextFile;
  str, fname, lname, cname, birth: string;
  birthDate: TDate;
  i, id, iAllStu: integer;
  students, allStu: ArrayOfStudents;
begin
  str := '';
  iAllStu := 0;
  Result := True;

  AssignFile(Text, Dateiname);

  try
    reset(Text);


    while not EOF(Text) do                //Schüler zeilenweise einlesen
    begin
      readln(Text, str);

      fname := '';
      lname := '';
      cname := '';
      birth := '';

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

        id := self.SNew(lname, fname, cname, '', birthDate);
        if not (id = -1) then
        begin
          if (RnOld) then
          begin
            iAllStu := iAllStu + 1;
            SetLength(allStu, iAllStu);
            allStu[iAllStu - 1] := uDBConn.getStudentById(id);
          end;
        end
        else
          Result := False;
      end
      else if (length(students) = 1) then
      begin                 //Fall Klasse überschreiben

        students[0].setClassName(cname);
        if (uDBConn.updateinsertStudent(students[0])) then
        begin
          if (RnOld) then
          begin
            iAllStu := iAllStu + 1;
            SetLength(allStu, iAllStu);
            allStu[iAllStu - 1] := students[0];
          end;
        end
        else
        begin
          Result := False;
          Break;
        end;

      end
      else
      begin                  //Fall Ein Fehler liegt vor
        Result := False;
        break;
      end;

    end;
    if (RnOld and Result) then
      Result := renameOld(allStu);
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

function TManagement.SNew(lastN, firstN, classN, ldap_user: string; birth: TDate): int64;
var
  id, pz: int64;
  hid: string;
  student: TStudent;
  students: ArrayOfStudents;
begin
  students := self.getStudentsByFirstLastClassNameBirthdate(firstN, lastN, classN, birth);
  if (length(students) = 0) then
  begin
    pz := 0;

    repeat
      id := Random(2000000) + 1000000; //Bereich von 1Mio bis 3 Mio

      hid := IntToStr(id);
      //hid ist eine Hilfsvariable zur Prüfnummererstellung
      pz := (3 * ((StrToInt(hid[1])) + (StrToInt(hid[3])) + (StrToInt(hid[5])) +
        (StrToInt(hid[7]))) + StrToInt(hid[2]) + StrToInt(hid[4]) +
        StrToInt(hid[6])) mod 10;
      if pz <> 0 then
        pz := 10 - pz;
      //Die Prüfziffer Teil 1
      id := (id * 10) + pz;                  //Die Prüfziffer wird hinten angehangen
    until SIdCheck(id) = False;            //Wiederholung bis id nicht vergeben

    student := TStudent.Create;
    student.setId(id);
    if (student.setFirstName(firstN) and student.setLastName(lastN) and
      student.setClassName(classN) and student.setBirth(birth) and
      student.setLDAPUser(ldap_user)) then
    begin
      self.SUpdate(student);
      Result := id;
    end
    else
    begin
      student.Destroy;
      Result := -1;
    end;

  end
  else
    Result := -1;
end;

function TManagement.SUpdate(var student: TStudent): boolean;
begin
  if not (student = nil) then
    Result := uDBConn.updateinsertStudent(student)
  else
    Result := False;
end;

end.
