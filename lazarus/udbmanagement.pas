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



    //BOOK

    //Vor: ISBN nur mit Zahlen
    //Eff: Hinzufügen eines neuen Buches
    //Erg: Id des neu hinzugefügten Buches
    function BNew(isbn : String) : LongInt;

    //Vor: Buch Id und Schüler Id
    //Eff: Rückgabe eines Buches mit Schüler
    //Erg: Trägt aktuelles Datum (als Double (nicht lesbar)) als Rückgabedatum in die Datenbank ein
    procedure BBack(BId, SId :LongInt);

    //Vor: Eine Buch Id
    //Eff: Überprüft, ob eine Buch Id bereits vergeben ist
    //Erg: Wahr, wenn Buch Id vergeben
    function BIdCheck(BId :LongInt):Boolean;

    //Vor: Die Buch Id
    //Eff: Löscht ein Buch aus dem Bestand
    procedure BDel(BId:LongInt);

    //Vor: Buch Id und seine Qualität
    //Eff: Ändert die Buchqualität
    //Erg: Trägt übergebene Qualität in die Datenbank ein
    procedure BQualiNew(BId, quali :LongInt);

    //Vor: Eine Buch Id
    //Eff: Überprüft die Buchqualität
    //Erg: Die ehemalige Quaität
    function BQualiCheck(BId: LongInt): LongInt;

    //Vor: Buch Id
    //Erg: Das Buch-Object mit der Id
    function getBookByID(BID: LongInt): TBook;




    //BOOKTYPE

    //Vor: isbn nur mit Zahlen, Titel und Fach des Buches, isbn darf nicht existieren
    //Eff: Neuer Buchtyp
    procedure BTypeNew(isbn, title, subject :String);

    //Vor: Die ISBN
    //Eff: Prüft den Buchtyp
    //Erg: Wahr, wenn isbn bereits vorhanden
    function BTypeCheck (isbn:String): Boolean;



    //STUDENTS

    // Importiert die Schüler Liste als CSV Datei
    // Klasse; Name; Vorname; Geburtsdatum
    // Datei mit dem Namen Dateiname muss im UNterverzeichnis liegen
    // False, wenn ein Fehler vorliegt
    function importCSVSchueler(Dateiname:String):Boolean;

    //Erg: Gibt ein Element vom Typ ArrayOfStudents zurück,
    //     welches alle Schüler beinhaltet
    function getStudents: ArrayOfStudents;

    //Vor: Den Klassennamen
    //Erg: Gibt ein Element vom Typ ArrayOfStudents zurück,
    //     welches alle Schüler in der Klasse beinhaltet
    function getStudentsByClassName(classN: string): ArrayOfStudents;

    //Vor: Den Vornamen
    //Erg: Gibt ein Element vom Typ ArrayOfStudents zurück,
    //     welches alle Schüler mit dem übergebenen Vornamen beinhaltet
    function getStudentsByFirstNamePattern(firstName: string): ArrayOfStudents;

    //Vor: Den Nachnamen
    //Erg: Gibt ein Element vom Typ ArrayOfStudents zurück,
    //     welches alle Schüler mit dem übergebenen Nachnamen beinhaltet
    function getStudentsByLastNamePattern(lastName: string): ArrayOfStudents;

    //Vor: den Vorname, Nachnamen und Geburtsdatum
    //     Geburtsdatum darf nil sein
    //Erg: Ein Array von TStudent-Objekten mit den Daten
    function getStudentsByFirstLastNameBirthdate(fname, lname: STring; birth:TDate) : ArrayOfStudents;

    //Vor: Die Schüler Id
    //Erg: Gibt ein Element vom Typ TStudent zurück,
    //     welches den Schüler mit der übergebnen Id beinhaltet
    function getStudentById(id: LongInt): TStudent;

    //Eff: Überschreibt die Daten des Schülers mit der übergebenen Id in der
    //     Datenbank mit dem Übergebenen Schüler
    //Erg: Wenn Schüler nicht vorhanden wahr -> False
    function SUpdate (student: TStudent): boolean;

    //Vor: Eine Schüler Id
    //Eff: Überprüft, ob eine Schüler Id bereits vergeben ist
    //Erg: Wahr, wenn vergeben
    function SIdCheck(SId :LongInt):Boolean;

    //Vor: Nachname, Vorname und Klassenname, Geburtsdatum als TDate
    //Eff: Neuen Schüler erstellen
    //Erg: Die Schüler Id
    function SNew (lastN, firstN, classN : String; birth:TDate):LongInt;

    //Vor: Eine Schüler Id
    //Eff: Löscht einen Schüler
    procedure SDel(SId : LongInt);



    //RENTAL

    //Vor: Buch Id und Schüler Id
    //Eff: Neue Vergabe eines Buches
    //Erg: Trägt in der Datenbank in Tabelle rental das ausgeliehene Buch zu dem Schüler ein
    procedure RNew(BId, SId :LongInt);

    //Vor: Eine Datum, bis wohin der Verlauf des Verleihs gelöscht werden soll
    //Eff: Löscht jeden Verleih, welches Rückgabedatum kleiner gleich ist als das Datum
    procedure RDel(datum: TDate);  //bzw TDateTime



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

function TDBManagement.BIdCheck(BId :LongInt):Boolean;
Var book : TBook;
begin
  book:=uDBConn.getBookById(BId);
  if book = nil then result:=False
  else result :=true;
end;

function TDBManagement.BQualiCheck(BId: LongInt): LongInt;
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

function TDBManagement.getStudentById(id: LongInt): TStudent;
begin
  Result:=uDBConn.getStudentById(id);
end;

function TDBManagement.SUpdate(student: TStudent): boolean;
begin
  Result:=uDBConn.updateinsertStudent(student);
end;

function TDBManagement.SIdCheck(SId :LongInt):Boolean;
begin
  if (uDBConn.getStudentById(SId) = nil) then Result:= false
  else Result:=true;
end;

procedure TDBManagement.BBack(BId, SId :LongInt);
Var aoR :ArrayOfRentals;
    rental : TRental;
begin
  aoR := uDBConn.getAllRentalsByBookAndStudent(uDBConn.getStudentById(SId), uDBConn.getBookById(BId));
  rental := aoR[1];
  rental.setReturnDate(now);
end;

function TDBManagement.BNew(isbn : String):LongInt;
Var id, pz: LongInt;
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

  uDBConn.updateinsertBook(book);

  Result:=id;
end;

procedure TDBManagement.BDel(BId:LongInt);
begin
  uDBConn.deleteBook(uDBConn.getBookById(BId));
end;

procedure TDBManagement.BQualiNew(BId, quali :LongInt);
Var book:TBook;
begin
  book:=uDBConn.getBookById(BId);
  book.setCondition(quali);
end;

procedure TDBManagement.BTypeNew(isbn, title, subject :String);
Var booktype : TBooktype;
begin
  booktype := TBooktype.Create;
  booktype.setIsbn(isbn);
  booktype.setTitle(title);
  booktype.setSubject(subject);

  uDBConn.updateinsertBooktype(booktype);
end;

procedure TDBManagement.RNew(BId, SId :LongInt);        //functioniert nicht !                        //!
Var rental: TRental;
begin
  rental := TRental.Create;

  rental.setBookId(BId);
  rental.setStudentId(SId);
  rental.setRentalDate(now);

  uDBConn.updateinsertRental(rental);
end;

procedure TDBManagement.SDel(SId : LongInt);
begin
  uDBConn.deleteStudent(uDBConn.getStudentById(SID));
end;

procedure TDBManagement.RDel(datum: TDate);
begin
  uDBConn.deleteReturnedRentalOlderThan(datum);
end;

function TDBManagement.SNew (lastN, firstN, classN : String; birth:TDate) : LongInt;
Var id, pz: LongInt;
    hid: String;
    student: TStudent;
begin
  pz:=0;

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

function TDBManagement.getBookByID(BID: LongInt): TBook;
begin
  result:= uDBConn.getBookById(bid);
end;

function TDBManagement.importCSVSchueler(Dateiname:String): Boolean;
Var text: TextFile;
    str, fname, lname, cname, birth : String;
    birthDate : TDate;
    i, j, k, id : Integer;
    students: Array of TStudent;
begin
  str:='';
  fname:='';
  lname:='';
  cname:='';
  birth:='';
  indexS3:=0;

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
      students:=self.getStudentsByFirstLastNameBirthdate(fname, lname, birthDate);

                                                                //Verarbeitung
      if (length(students2) = 0) then
      begin                 //Fall Einfügen eines neuen Schülers

        self.SNew(lname, fname, cname, birthDate);
        Result:=true;

      end else if (length(students2) = 1) then
      begin                 //Fall Klasse überschreiben

        students2[0].setClassName(cname);
        Result:=uDBConn.updateinsertStudent(students2[0]);;

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

function TDBManagement.isConnected():Boolean;
Begin
  Result:=uDBConn.isConnected;
end;

function TDBManagement.getStudentsByFirstLastNameBirthdate(fname, lname: STring; birth:TDate) : ArrayOfStudents;
Var students, students2, students3: Array of TStudent;
    indexS3: Integer;
begin
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



  students:=uDBConn.getStudentsByBirthdate(birthDate);        //Geburtsdatum
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

  Result:=students2;
end;

end.
