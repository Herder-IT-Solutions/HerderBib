unit uDBManagement;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, sqlite3conn, sqldb, student, DBConnection, book, booktype, rental, dateutils;

type

  TDBManagement = class

  //Methoden
  public
    //Erg: Initialisierung der Datenbankverwaltung
    constructor create();

    //Erg: Säubert Überreste des Programms
    destructor Destroy;

    // Überprüft die Verbindung zur Datenbank
    // Wahr wenn verbunden
    function isConnected: boolean;



    //BOOK                                                --------------------

    //Vor: Buch Id und Schüler Id
    //Eff: Rückgabe eines Buches mit Schüler
    //Erg: False, wenn gescheitert
    function BBack(BId, SId :Int64): Boolean;

    //Vor: Ein Buch mittels TBook-Objekt
    //Eff: Löscht ein Buch aus dem Bestand
    //Erg: Wahr wenn erfolgreich
    function BDel(var book:TBook):Boolean;

    //Vor: Eine Buch Id
    //Eff: Überprüft, ob eine Buch Id bereits vergeben ist
    //Erg: Wahr, wenn Buch Id vergeben
    function BIdCheck(BId :Int64):Boolean;

    //Vor: ISBN nur mit Zahlen
    //Eff: Hinzufügen eines neuen Buches
    //Erg: Id des neu hinzugefügten Buches; -1 bei einem Fehler
    function BNew(isbn : String) : Int64;

    //Vor: Eine Buch Id
    //Eff: Überprüft die Buchqualität
    //Erg: Die Buchqualität
    function BQualiCheck(BId: Int64): Int64;

    //Vor: Buch Id und seine Qualität
    //Eff: Ändert die Buchqualität
    //Erg: Wahr wenn erfolgreich
    function BQualiNew(BId, quali :Int64):Boolean;

    //Vor: Eine Buch Id
    //Erg: Gibt den Titel des Buches wieder
    function BTitleById(BId: int64):String;

    //Vor: Buch Id
    //Erg: Das Buch-Object mit der Id
    function getBookByID(BID: Int64): TBook;




    //BOOKTYPE                                            --------------------

    //Vor: Die ISBN
    //Eff: Prüft den Buchtyp
    //Erg: Wahr, wenn isbn bereits vorhanden
    function BTypeCheck (isbn:String): Boolean;

    //Vor: isbn nur mit Zahlen, Titel und Fach des Buches, isbn darf nicht existieren
    //Eff: Neuer Buchtyp
    //Erg: Wahr wenn erfolgreich; Falsch wenn Fehler oder Buchtyp bereits vorhanden
    function BTypeNew(isbn, title, subject :String):Boolean;




    //RENTAL                                              --------------------

    //Vor: Eine Datum, bis wohin der Verlauf des Verleihs gelöscht werden soll
    //Eff: Löscht jeden Verleih, welches Rückgabedatum kleiner gleich ist als das Datum
    //Erg: Anzahl der gelöschten Objekte; -1 bei einem Fehler
    function RDel(datum: TDate):Integer;

    //Vor: Ein Rental-Objekt, dass gelöscht werden soll
    //Eff: Löscht das Rental Objekt
    //Erg: wahr wenn erfolgreich
    function RDelOne(var rental:TRental):Boolean;

    //Vor: Buch Id und Schüler Id
    //Eff: Neue Vergabe eines Buches
    //Erg: Wahr wenn es geklappt hat, falsch wenn rental bereits vorhanden
    function RNew(BId, SId :Int64):Boolean;




    //STUDENTS                                            --------------------

    //Vor: Eine Schüler Id
    //Erg: Den Namen des Schülers ('Vorname Nachname')
    function getSNameById(id: int64): String;

    //Erg: Gibt ein Element vom Typ ArrayOfStudents zurück,
    //     welches alle Schüler beinhaltet
    function getStudents: ArrayOfStudents;

    //Vor: Die Schüler Id
    //Erg: Gibt ein Element vom Typ TStudent zurück,
    //     welches den Schüler mit der übergebnen Id beinhaltet
    function getStudentById(id: Int64): TStudent;

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
    function getStudentsByFirstLastClassNameBirthdate(fname, lname, cname: STring; birth:TDate) : ArrayOfStudents;

    // Importiert die Schüler Liste als CSV Datei
    // Klasse; Name; Vorname; Geburtsdatum
    // Datei mit dem Namen Dateiname muss im UNterverzeichnis liegen
    // Wahr wenn erfolgreich
    function importCSVSchueler(Dateiname:String):Boolean;

    //Vor: Eine Schüler mittels TStudent-Objekt
    //Eff: Löscht einen Schüler
    //Erg: Wahr wenn erfolgreich
    function SDel(var student:TStudent):Boolean;

    //Vor: Eine Schüler Id
    //Eff: Überprüft, ob eine Schüler Id bereits vergeben ist
    //Erg: Wahr, wenn vergeben
    function SIdCheck(SId :Int64):Boolean;

    //Vor: Nachname, Vorname und Klassenname, Geburtsdatum als TDate
    //Eff: Neuen Schüler erstellen
    //Erg: Die Schüler Id
    function SNew (lastN, firstN, classN : String; birth:TDate):Int64;

    //Eff: Überschreibt die Daten des Schülers mit der übergebenen Id in der
    //     Datenbank mit dem Übergebenen Schüler
    //Erg: Wenn Schüler nicht vorhanden wahr -> False
    function SUpdate (var student: TStudent): boolean;




  //Atribute
  private
    uDBConn : TDBConnection;         //Element für Verbindung zur DBConnection

end;

implementation

const
  ourNil = -693594;

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

function TDBManagement.isConnected():Boolean;
Begin
  Result:=uDBConn.isConnected;
end;




//-----------------------------------------------------------------------------

function TDBManagement.BIdCheck(BId :Int64):Boolean;
Var book : TBook;
begin
  book:=uDBConn.getBookById(BId);
  if book = nil then result:=False
  else result :=true;
end;

function TDBManagement.BQualiCheck(BId: Int64): Int64;
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

function TDBManagement.getStudentByLDAPUserPattern(ldap_user: string): ArrayOfStudents;
begin
  Result:=uDBConn.getStudentsByLDAPUserPattern(ldap_user);
end;

function TDBManagement.getStudentById(id: Int64): TStudent;
begin
  Result:=uDBConn.getStudentById(id);
end;

function TDBManagement.SUpdate(var student: TStudent): boolean;
begin
  Result:=uDBConn.updateinsertStudent(student);
end;

function TDBManagement.SIdCheck(SId :Int64):Boolean;
begin
  if (uDBConn.getStudentById(SId) = nil) then Result:= false
  else Result:=true;
end;

function TDBManagement.BBack(BId, SId :Int64):Boolean;
Var aoR :ArrayOfRentals;
    rental : TRental;
    book: TBook;
    student: TStudent;
begin
  book:= self.getBookByID(BID);
  student:=self.getStudentById(SId);
  if not ((book=nil) and (student=nil)) then
  begin
    aoR := uDBConn.getAllRentalsByBookAndStudent(student, book);
    rental := aoR[0];
    rental.setReturnDate(now);

    Result:=uDBConn.updateInsertRental(rental);
  end else Result:=False;
end;

function TDBManagement.BNew(isbn : String):Int64;
Var id, pz: Int64;
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

  if not (uDBConn.updateinsertBook(book)) then Result:=-1
  else Result:=id;
end;

function TDBManagement.BDel(var book:TBook):Boolean;
begin
  if not (book=nil) then Result:=uDBConn.deleteBook(book)
  else Result:=False;
end;

function TDBManagement.BQualiNew(BId, quali :Int64):Boolean;
Var book:TBook;
begin
  book:=self.getBookById(BId);
  if not (book = nil) then
  begin
    book.setCondition(quali);
    Result:=uDBConn.updateInsertBook(book);
  end else Result:=False;
end;

function TDBManagement.BTypeNew(isbn, title, subject :String):Boolean;
Var booktype : TBooktype;
begin
  if not (self.BTypeCheck(isbn)) then
  begin
    booktype := TBooktype.Create;
    booktype.setIsbn(isbn);
    booktype.setTitle(title);
    booktype.setSubject(subject);

    Result:=uDBConn.updateinsertBooktype(booktype);
  end else Result:=False;
end;

function TDBManagement.RNew(BId, SId :Int64): Boolean;
Var rentals: Array Of TRental;
    rental : TRental;
    book: TBook;
    student: TStudent;
begin
  book:= self.getBookByID(BID);
  student:=self.getStudentById(SId);
  if not ((book=nil) and (student=nil)) then
  begin
    rentals := uDBConn.getAllRentalsByBookAndStudent(student, book);
    if (length(rentals) = 0) then
    begin
      rental := TRental.Create;

      rental.setBookId(BId);
      rental.setStudentId(SId);
      rental.setRentalDate(now);
      rental.setReturnDate(ourNil);

      Result:=uDBConn.updateinsertRental(rental);
    end else Result := False;
  end else Result:=false;
end;

function TDBManagement.SDel(var student:TStudent):Boolean;
begin
  if not (student=nil) then Result:=uDBConn.deleteStudent(student)
  else Result:=False;
end;

function TDBManagement.RDel(datum: TDate):Integer;
begin
  Result:=uDBConn.deleteReturnedRentalOlderThan(datum);
end;

function TDBManagement.SNew (lastN, firstN, classN : String; birth:TDate) : Int64;
Var id, pz: Int64;
    hid: String;
    student: TStudent;
begin
  pz:=0;

  //if (self.getStudentsByFirstLastClassNameBirthdate(firstN, lastN, birth)

  repeat
     id:= Random(2000000) + 1000000; //Bereich von 1Mio bis 3 Mio

     hid:=inttostr(id); //hid ist eine Hilfsvariable zur Prüfnummererstellung

     pz:= ((strtoint(hid[1])*3) + (strtoint(hid[3])*3) + (strtoint(hid[5])*3) + (strtoint(hid[7])*3) + strtoint(hid[2]) + strtoint(hid[4]) + strtoint(hid[6]))Mod 10; //Die Prüfziffer Teil 1
     if pz = 10 then pz := 0;
     id:=(id*10)+ pz;                  //Die Prüfziffer wird hinten angehangen
  until SIdCheck(id)=false;            //Wiederholung bis id nicht vergeben

  student := TStudent.Create;
  student.setId(id);
  student.setFirstName(firstN);
  student.setLastName(lastN);
  student.setClassName(classN);
  student.setBirth(birth);

  self.SUpdate(student);
  Result:=id;
end;

function TDBManagement.getBookByID(BID: Int64): TBook;
begin
  result:= uDBConn.getBookById(bid);
end;

function TDBManagement.importCSVSchueler(Dateiname:String): Boolean;
Var text: TextFile;
    str, fname, lname, cname, birth : String;
    birthDate : TDate;
    i : Integer;
    students: Array of TStudent;
begin
  str:='';
  fname:='';
  lname:='';
  cname:='';
  birth:='';

  AssignFile(text, Dateiname);

  try
    reset(text);


    while not EoF (text) do                //Schüler zeilenweise einlesen
    begin
      readln(text, str);

      i:=1;
      while not (str[i] = ';') do        //Klassennamen einlesen
      begin
        cname:=cname+str[i];
        i:=i+1;
      end;

      i:=i+1;
      while not (str[i] = ';') do      //Namen einlesen
      begin
        lname:=lname+str[i];
        i:=i+1;
      end;

      i:=i+1;
      while not (str[i] = ';') do     //Vornamen einlesen
      begin
        fname:=fname+str[i];
        i:=i+1;
      end;

      i:=i+1;
      while ( not (str[i] = ';') and (length(str) >= i+1)) do     //Geburtsdatum einlesen
      begin
        birth:=birth+str[i];
        i:=i+1;
      end;
      birthDate:=StrToDate(birth, '-');
                                                                // Vergleich ob bereits vorhanden
      students:=self.getStudentsByFirstLastClassNameBirthdate(fname, lname, '', birthDate);

                                                                //Verarbeitung
      if (length(students) = 0) then
      begin                 //Fall Einfügen eines neuen Schülers

        self.SNew(lname, fname, cname, birthDate);
        Result:=true;

      end else if (length(students) = 1) then
      begin                 //Fall Klasse überschreiben

        students[0].setClassName(cname);
        Result:=uDBConn.updateinsertStudent(students[0]);;

      end else begin        //Fall Ein Fehler liegt vor
        Result:=False;
        break;
      end;

    end;
  except
    on E: Exception do
      Result:=False;

  end;
  CloseFile(text);


end;

function TDBManagement.getStudentsByFirstLastClassNameBirthdate(fname, lname, cname: STring; birth:TDate) : ArrayOfStudents;
Var students, students2, students3: Array of TStudent;
    indexS3, j, k, id: Integer;
begin
  indexS3:=0;
  students:=uDBConn.getStudentsByFirstNamePattern(fname);   // Nach Vorname

  students2:=uDBConn.getStudentsByLastNamePattern(lname);   //Name

  if not (length(students) = 0) and not (length(students2) = 0) then
  begin
    j:=0;
    while length(students) > j+1 do
    begin
      id := students[j].getId;

      k:=0;
      while length(students2) > k+1 do
      begin
        if id = students2[k].getId then
        begin
          students3[indexS3] := students2[k];
          indexS3:=indexS3+1;
        end;

        k:=k+1;
      end;
      j:=j+1;
    end;
  end else if (length(students) = 0) then students3 := students2
  else if (length(students2) = 0) then students3 := students;


  if not (birth = -693594) then
  begin
    students:=uDBConn.getStudentsByBirthdate(birth);        //Geburtsdatum
    indexS3:=0;

    if not (length(students) = 0) and not (length(students3) = 0) then
    begin
      j:=0;
      while length(students) > j+1 do
      begin
        id := students[j].getId;
        k:=0;
        while length(students3) > k+1 do
        begin
          if id = students3[k].getId then
          begin
            students2[indexS3] := students3[k];
            indexS3:=indexS3+1;
          end;
          k:=k+1;
        end;
        j:=j+1;
      end;
    end else if (length(students) = 0) then students2 := students3
    else if (length(students3) = 0) then students2 := students;
  end else begin
    students2:=students3;
  end;

  if not (cname = '') then
  begin
    students:=uDBConn.getStudentsByClassName(cname);     //Klassenname
    indexS3:=0;

    if not (length(students) = 0) and not (length(students2) = 0) then
    begin
      j:=0;
      while length(students) > j+1 do
      begin
        id := students[j].getId;
        k:=0;
        while length(students2) > k+1 do
        begin
          if id = students2[k].getId then
          begin
            students3[indexS3] := students2[k];
            indexS3:=indexS3+1;
          end;
          k:=k+1;
        end;
        j:=j+1;
      end;
      students2:=students3;
    end else if (length(students) = 0) then students2 := students3
    else if (length(students3) = 0) then students2 := students;
  end;

  Result:=students2;
end;

function TDBManagement.RDelOne(var rental:TRental):Boolean;
begin
  if not (rental=nil) then Result:=uDBConn.deleteRental(rental)
  else Result:=False;
end;

function TDBManagement.getSNameById(id: int64): String;
Var res: String;
    student:TStudent;
begin
  student:= self.getStudentById(id);
  res:= student.getFirstName() + ' ' + student.getLastName();
  Result:= res;
end;

function TDBManagement.BTitleById(BId: int64):String;
Var book:TBook;
    booktype:TBooktype;
begin
  book:=self.getBookByID(BId);
  booktype:=uDBConn.getBooktypeByIsbn(book.getIsbn());
  Result:=booktype.getTitle();
end;

end.
