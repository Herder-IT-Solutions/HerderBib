unit gui;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, Spin, ExtCtrls, Grids, Menus, types, sqldb, sqlite3conn, lclintf,
  Buttons, CheckLst, DB, uManagement, Student, Book, Rental,
  Booktype, LConvEncoding, uBarcodePrint;

type

  { TForm1 }

  TForm1 = class(TForm)
    BtInfoBooktypeEdit: TButton;
    BtInfoBookEdit: TButton;
    BtInfoBooktypeShow: TButton;
    BtInfoBookShow1: TButton;
    BtInfoSuportError: TButton;
    BtInfoSuportLicense: TButton;
    BtRent: TButton;
    BtRet: TButton;
    BtAddBook: TButton;
    BtInfoStudShow: TButton;
    BtInfoStudEdit: TButton;
    BtInfoBookDel: TButton;
    BtInfoStudExportRel: TButton;
    BtInfoRelFilter: TButton;
    BtInfoAdminLogin: TButton;
    BtInfoAdminLogout: TButton;
    BtInfoSuportWiki: TButton;
    BtInfoAdminSendDBName: TButton;
    BtInfoStudPrintQ: TButton;
    BtInfoBookPrintQ: TButton;
    BtPrint: TButton;
    CBAddBookSubject: TComboBox;
    CBInfoBooktypeSubject: TComboBox;
    CBInfoRelGrade: TComboBox;
    CBInfoRelSubject: TComboBox;
    EdRentBook: TEdit;
    EdRetBook: TEdit;
    EdAddBookName: TEdit;
    EdAddBookISBN: TEdit;
    EdInfoBooktypeISBN: TEdit;
    EdInfoBookID: TEdit;
    EdInfoBooktypeName: TEdit;
    EdInfoStudFirstName: TEdit;
    EdInfoStudLastName: TEdit;
    EdInfoStudGrade: TEdit;
    EdInfoStudID: TEdit;
    EdInfoBookRent: TEdit;
    EdInfoAdminPw: TEdit;
    EdRentStud: TEdit;
    EdRetStud: TEdit;
    ImRentHerder: TImage;
    ImRetHerder: TImage;
    ImAddHerder: TImage;
    ImPrintHerder: TImage;
    LbInfoSupportLicense: TLabel;
    LbInfoSupportError: TLabel;
    LbInfoBooktypeSubject: TLabel;
    LbInfoAdminConnection: TLabel;
    LbPrintQueue: TLabel;
    LbPrintInfo: TLabel;
    LbInfoStudBD: TLabel;
    LbInfoStudError: TLabel;
    LbInfoBookError: TLabel;
    LbInfoBooktypeError: TLabel;
    LbRetError: TLabel;
    LbRentError: TLabel;
    LEInfoAdminDBName: TLabeledEdit;
    LbInfoAdminCheck: TLabel;
    LbInfoAdmin: TLabel;
    LbInfoSupport: TLabel;
    LbInfoRelSubj: TLabel;
    LbInfoRelGrade: TLabel;
    LbInfoRelFilter: TLabel;
    LbAddBookHint: TLabel;
    LbAddBookError: TLabel;
    LbRetBookState: TLabel;
    LbInfoBookRent: TLabel;
    LbInfoBookTrack1: TLabel;
    LbRetBookTrack1: TLabel;
    LbInfoBookTrack3: TLabel;
    LbRetBookTrack3: TLabel;
    LbInfoBookTrack5: TLabel;
    LbRetBookTrack5: TLabel;
    LbInfoBooktypeISBN: TLabel;
    LbInfoBookID: TLabel;
    LbInfoBooktypeName: TLabel;
    LbInfoBookState: TLabel;
    LbInfoStudID: TLabel;
    LbInfoStudFirstName: TLabel;
    LbInfoStudLastName: TLabel;
    LbInfoStudGrade: TLabel;
    LbAddBookSubject: TLabel;
    LbAddBookQuantity: TLabel;
    LbAddBookName: TLabel;
    LbAddBookISBN: TLabel;
    LbRetBookName: TLabel;
    LbInfoSupport2: TLabel;
    LbRetStudInstruct: TLabel;
    LbRetBookInstruct: TLabel;
    LbRentStudName: TLabel;
    LbRentStudInstruct: TLabel;
    LbRentBookInstruct: TLabel;
    LbRentBookName: TLabel;
    LbRetStudName: TLabel;
    LiPrintQueue: TListBox;
    MeCredits: TMemo;
    MeInfoStudRel: TMemo;
    MeInfoRel: TMemo;
    PageControl1: TPageControl;
    PCInfos: TPageControl;
    SEAddBookQuantity: TSpinEdit;
    SEInfoStudDay: TSpinEdit;
    SEInfoStudMonth: TSpinEdit;
    SEInfoStudYear: TSpinEdit;
    TabRent: TTabSheet;
    TabRet: TTabSheet;
    TabAdd: TTabSheet;
    TabInfo: TTabSheet;
    TabBook: TTabSheet;
    TabRel: TTabSheet;
    TabBooktype: TTabSheet;
    TabCreditsHelp: TTabSheet;
    Administration: TTabSheet;
    TabPrint: TTabSheet;
    TabStud: TTabSheet;
    TBInfoBookState: TTrackBar;
    TBRetBookState: TTrackBar;
    procedure BtAddBookClick(Sender: TObject);
    procedure BtInfoAdminLoginClick(Sender: TObject);
    procedure BtInfoAdminLogoutClick(Sender: TObject);
    procedure BtInfoBookDelClick(Sender: TObject);
    procedure BtInfoBookEditClick(Sender: TObject);
    procedure BtInfoBookPrintQClick(Sender: TObject);
    procedure BtInfoBookShow1Click(Sender: TObject);
    procedure BtInfoBooktypeEditClick(Sender: TObject);
    procedure BtInfoBooktypeShowClick(Sender: TObject);
    procedure BtInfoRelFilterClick(Sender: TObject);
    procedure BtInfoStudEditClick(Sender: TObject);
    procedure BtInfoStudPrintQClick(Sender: TObject);
    procedure BtInfoStudShowClick(Sender: TObject);
    procedure BtInfoSuportLicenseClick(Sender: TObject);
    procedure BtInfoSuportWikiClick(Sender: TObject);
    procedure BtPrintClick(Sender: TObject);
    procedure BtRentClick(Sender: TObject);
    procedure BtRetClick(Sender: TObject);
    procedure BtInfoSuportErrorClick(Sender: TObject);
    procedure confirmNumbers(Sender: TObject; var Key: char);
    procedure EdRetBookChange(Sender: TObject);
    procedure EdRentBookChange(Sender: TObject);
    procedure EdInfoStudFirstNameChange(Sender: TObject);
    procedure EdRetStudChange(Sender: TObject);
    procedure EdRentStudChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LbRetStudNameClick(Sender: TObject);
    procedure PCInfosChange(Sender: TObject);
    procedure SEInfoStudMonthChange(Sender: TObject);
    procedure TabRetContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: boolean);
  private
    { private declarations }
    procedure addToPrintingQueueListBox(code: string; title: string);
  public
    { public declarations }
  end;

var
  Form1: TForm1;
  management: TManagement;
  PermissionLevel: cardinal;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.TabRetContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: boolean);
begin

end;



procedure TForm1.PCInfosChange(Sender: TObject);
begin
  if management.isConnected then
    LbInfoAdminConnection.Caption := 'Datenbankverbindung hergestellt'
  else if not (management.isConnected) then
    LbInfoAdminConnection.Caption := 'Datenbankverbindung nicht hergestellt';
end;


procedure TForm1.SEInfoStudMonthChange(Sender: TObject);
begin
  case SeInfoStudMonth.Value of
    //This procedure limits the number of days according to the month
    1: SeInfoStudDay.MaxValue := 31;
    2: SeInfoStudDay.MaxValue := 29;
    3: SeInfoStudDay.MaxValue := 31;
    4: SeInfoStudDay.MaxValue := 30;
    5: SeInfoStudDay.MaxValue := 31;
    6: SeInfoStudDay.MaxValue := 30;
    7: SeInfoStudDay.MaxValue := 31;
    8: SeInfoStudDay.MaxValue := 31;
    9: SeInfoStudDay.MaxValue := 30;
    10:SeInfoStudDay.MaxValue := 31;
    11:SeInfoStudDay.MaxValue := 30;
    12:SeInfoStudDay.MaxValue := 31;
  end;
end;



procedure TForm1.FormCreate(Sender: TObject);
begin
  // LbRentStudInstruct := 'Hello' + #13#10 + 'world';
  PermissionLevel := 1;
  management := tmanagement.Create();
  //management := TVerwaltung.create(SQLQuery,SQLTransaction,SQLite3Connection)
end;



procedure TForm1.LbRetStudNameClick(Sender: TObject);
begin

end;



procedure TForm1.confirmNumbers(Sender: TObject; var Key: char);
begin
  if not (Key in ['0'..'9', #8, #9]) then
    Key := #0;
end;

procedure TForm1.EdRetBookChange(Sender: TObject);
var
  book: TBook;
begin
  LbRetError.Visible := False;
  try
    if not (EdRetBook.Text = '') then
    begin
      book := management.getBookByID(StrToInt(EdRetBook.Text));
      if not (book = nil) then
        LbRetBookName.Caption := book.getISBN
      else
        LbRetBookname.Caption := '';
    end
    else
      LbRetBookname.Caption := '';
  except
    On EConvertError do
    begin
      LbRetError.Visible := True;
      LbRetError.Caption :=
        'Fehler 3: Eines der erforderlichen Felder enthaelt kein gültiges Datum';
    end;
  end;
end;

procedure TForm1.EdRentBookChange(Sender: TObject);
var
  book: TBook;
begin
  try
    if not (EdRentBook.Text = '') then
    begin
      book := management.getBookByID(StrToInt(EdRentBook.Text));
      if not (book = nil) then
        LbRentBookName.Caption := book.getISBN
      else
        LbRentBookname.Caption := '';
    end
    else
      LbRentBookname.Caption := '';

  except
    On EConvertError do
    begin
      LbRentError.Visible := True;
      LbRentError.Caption :=
        'Fehler 3: Eines der erforderlichen Felder enthaelt kein gültiges Datum';
    end;
  end;
end;


procedure TForm1.EdInfoStudFirstNameChange(Sender: TObject);
begin

end;

procedure TForm1.EdRetStudChange(Sender: TObject);
var
  stu: TSTUDENT;
begin
  LbRetError.Visible := False;
  try
    if not (EdRetStud.Text = '') then
    begin
      stu := management.getStudentByID(StrToInt(EdRetStud.Text));
      if not (stu = nil) then
        LbRetStudName.Caption := stu.getFirstName() + ' ' + stu.getLastName()
      else
        LbRetStudname.Caption := '';
    end
    else
      LbRetStudname.Caption := '';
  except
    On EConvertError do
    begin
      LbRetError.Visible := True;
      LbRetError.Caption :=
        'Fehler 3: Eines der erforderlichen Felder enthaelt kein gültiges Datum';
    end;
  end;
end;

procedure TForm1.EdRentStudChange(Sender: TObject);
var
  stu: TSTUDENT;
begin
  LbRentError.Visible := False;
  try
    if not (EdRentStud.Text = '') then
    begin
      stu := management.getStudentByID(StrToInt(EdRentStud.Text));
      if not (stu = nil) then
        LbRentStudName.Caption := stu.getFirstName() + ' ' + stu.getLastName()
      else
        LbRentStudname.Caption := '';
    end
    else
      LbRentStudname.Caption := '';
  except
    On EConvertError do
    begin
      LbRentError.Visible := True;
      LbRentError.Caption :=
        'Fehler 3: Eines der erforderlichen Felder enthaelt kein gültiges Datum';
    end;
  end;
end;

procedure TForm1.BtAddBookClick(Sender: TObject);
var
  s: string;
  a, b: boolean;
  k: cardinal;
  tempCode: LARGEINT;


  function CheckSumISBN13(isbn: string): boolean;
  var
    i, sum, check: cardinal;
  begin
    check := 0;
    sum := 0;
    i := 1;
    while i < 13 do
    begin
      if (i mod 2 = 0) then
        sum := sum + 3 * StrToInt(isbn[i])
      else if (i mod 2 = 1) then
        sum := sum + StrToInt(isbn[i]);
      Inc(i);
    end;
    check := (10 - (sum mod 10));
    Result := (check = StrToInt(isbn[13]));
  end;

begin
  a := False;
  s := EdAddBookISBN.Text;    //Beispiel: funktiuoniert bei 9780306406157
  if not (s = '') then
  begin

    if (length(s) = 13) then
      b := CheckSumISBN13(s)
    else
      a := True;
    LbAddBookError.Visible := False;
    if not (b) then
    begin
      LbAddBookError.Visible := True;
      LbAddBookError.Caption :=
        'Fehler 1: Die ISBN ist ungültig (falsche Prüfziffer)';
      exit;
    end;
    if a then
    begin
      LbAddBookError.Visible := True;
      LbAddBookError.Caption := 'Fehler 2: Die ISBN ist nicht 13 Ziffern lang';
      exit;
    end;

    k := 1;

    if not (management.BTypeCheck(s)) then
      management.BTypeNew(s, EdAddBookName.Text, CBAddBookSubject.Text);
    //Wenn ISBN noch nicht bekannt dann füge buch hinzu
    while (k <= SeAddBookQuantity.Value) do
    begin     //füge bücher hinzu
      tempcode := management.BNew(s);
      Inc(k);
      TBarcodePrinter.instance.add_barcode(IntToStr(tempcode), EdAddBookName.Text);
      addToPrintingQueueListBox(IntToStr(tempcode), EdAddBookName.Text);
    end;
    EdAddBookName.Text := '';
    EdAddBookISBN.Text := '';
    CBAddBookSubject.Text := '';
    SEAddBookQuantity.Value := 0;
  end
  else
  begin
    LbAddBookError.Visible := True;
    LbAddBookError.Caption :=
      'Fehler 3: Eines der erforderlichen Felder enthaelt kein gültiges Datum';
    exit;
  end;
end;

procedure TForm1.BtInfoAdminLoginClick(Sender: TObject);
begin
  if EdInfoAdminPw.Text = 'h3rd3r' then
  begin
    PermissionLevel := 0;
    //EdInfoAdminPw.Text=NONE;
    LbInfoAdminCheck.Caption := 'Sie sind Administrator';
    BtInfoStudEdit.Enabled := True;
    BtInfoAdminLogin.Enabled := False;
    BtInfoAdminLogout.Enabled := True;

  end;
  EdInfoAdminPw.Text := '';
end;

procedure TForm1.BtInfoAdminLogoutClick(Sender: TObject);
begin
  PermissionLevel := 1;
  LbInfoAdminCheck.Caption := 'Sie sind nicht Administrator';
  BtInfoStudEdit.Enabled := False;
  BtInfoAdminLogin.Enabled := True;
  BtInfoAdminLogout.Enabled := False;
end;

procedure TForm1.BtInfoBookDelClick(Sender: TObject);
var
  b: TBook;
begin
  b := management.getBookByID(StrToInt(EdInfoBookID.Text));
  management.BDel(b);
  EdInfoBookId.Text := '';
  EdInfoBookRent.Text := '';
  TBInfoBookState.Position := 1;
end;

procedure TForm1.BtInfoBookEditClick(Sender: TObject);
begin
  LbInfoBookError.Visible := False;
  try

  except
    On EConvertError do
    begin
      LbInfoBookError.Visible := True;
      LbInfoBookError.Caption :=
        'Fehler 3: Eines der erforderlichen Felder enthaelt kein gültiges Datum';
    end;
  end;

end;

procedure TForm1.BtInfoBookPrintQClick(Sender: TObject);
var
  book: TBook;
  booktitle: string;
begin
  LbInfoBookError.Visible := False;
  try
    if not (EdInfoBookId.Text = '') then
    begin
      book := management.getBookByID(StrToInt(EdInfoBookID.Text));
      booktitle := management.BTitleByID(StrToInt(EdInfoBookID.Text));
      TBarcodePrinter.instance.add_barcode(IntToStr(book.getid), booktitle);
      addToPrintingQueueListBox(IntToStr(book.getid), booktitle);
    end
    else
    begin
      LbInfoBookError.Visible := True;
      LbInfoBookError.Caption :=
        'Fehler 5: Die Buch-Identifikationsnummer ist keinem Buch zugeordnet';
    end;
  except
    On EConvertError do
    begin
      LbInfoBookError.Visible := True;
      LbInfoBookError.Caption :=
        'Fehler 3: Eines der erforderlichen Felder enthaelt kein gültiges Datum';
    end;
  end;

end;

procedure TForm1.BtInfoBookShow1Click(Sender: TObject);
var
  book: TBOOK;
begin
  LbInfoBookError.Visible := False;
  try
    if management.BIdCheck(StrToInt(EdInfoBookID.Text)) and not
      (EdInfoBookID.Text = '') then
    begin
      book := management.getBookByID(StrToInt(EdInfoBookID.Text));
      TBInfoBookState.Position := book.getcondition;
      //SHOW RENTAL RELATION
    end;

  except
    On EConvertError do
    begin
      LbInfoBookError.Visible := True;
      LbInfoBookError.Caption :=
        'Fehler 3: Eines der erforderlichen Felder enthaelt kein gültiges Datum';
    end;
  end;

end;

procedure TForm1.BtInfoBooktypeEditClick(Sender: TObject);
var
  booktype: TBooktype;
begin
  LbInfoBooktypeError.Visible := False;
  try
    if not (EdInfoBooktypeISBN.Text = '') then
    begin
      booktype := management.getBooktypeByISBN(EdInfoBooktypeISBN.Text);
      if not (EdInfoBooktypeName.Text = '') then
      begin
        booktype.setTitle(EdInfoBooktypeName.Text);
      end;
      if not (CBInfoBooktypeSubject.Text = '') then
      begin
        booktype.setSubject(CBInfoBooktypeSubject.Text);
      end;
    end;
  except
    On EConvertError do
    begin
      LbInfoBooktypeError.Visible := True;
      LbInfoBooktypeError.Caption :=
        'Fehler 3: Eines der erforderlichen Felder enthaelt kein gültiges Datum';
    end;
  end;

end;

procedure TForm1.BtInfoBooktypeShowClick(Sender: TObject);
var
  booktype: TBooktype;
begin
  LbInfoBooktypeError.Visible := False;
  try
    if not (management.BTypeCheck(EdInfoBooktypeISBN.Text)) then
    begin
      LbInfoBooktypeError.Visible := True;
      LbInfoBooktypeError.Caption := 'Fehler 4: Die ISBN ist keinem Buchtyp zugeordnet';
    end
    else if management.BTypeCheck(EdInfoBooktypeISBN.Text) then
    begin
      booktype := management.getBooktypeByISBN(EdInfoBooktypeISBN.Text);
      EdInfoBooktypeName.Text := booktype.gettitle;
      CBInfoBooktypeSubject.Text := booktype.getsubject;
    end;
  except
    On EConvertError do
    begin
      LbInfoBooktypeError.Visible := True;
      LbInfoBooktypeError.Caption :=
        'Fehler 3: Eines der erforderlichen Felder enthaelt kein gültiges Datum';
    end;
  end;

end;

procedure TForm1.BtInfoRelFilterClick(Sender: TObject);

  procedure printLine(s: string; li: cardinal);
  var
    len, i: cardinal;
  begin
    len := length(s);
    i := 1;
    while i < (len + 1) do
    begin
      if s[i] = ';' then
        s[i] := ' ';
      Inc(i);
    end;
    MeInfoRel.Lines[li] := s;
  end;

  procedure readCSV(dataname: string);
  var
    f: TextFile;
    str: string;
    cur: cardinal;
  begin
    AssignFile(f, dataname);
    reset(f);
    cur := 0;
    while not EOF(f) do
    begin
      cur := cur + 1;
      readln(f, str);
      //SomeRTLRoutine(UTF8ToAnsi(str));
      printLine(AnsiToUTF8(str), cur);
    end;
    CloseFile(f);
  end;

begin
  ;
  MeInfoRel.Clear;
  readCSV('rental_relations.csv');
  //MeInfoRel.Lines.Text := ConvertEncoding(MeInfoRel.Lines.Text, GuessEncoding(MeInfoRel.Lines.Text), EncodingUTF8);
end;

procedure TForm1.BtInfoStudEditClick(Sender: TObject);
var
  stud: TStudent;
begin
  LbInfoStudError.Visible := False;
  try
    stud := management.getStudentbyID(StrToInt(EdInfoStudID.Text));
    stud.setlastname(EdInfoStudLastName.Text);
    stud.setfirstname(EdInfoStudFirstName.Text);
    stud.setclassname(EdInfoStudGrade.Text);
    management.supdate(stud);
  except
    On EConvertError do
    begin
      LbInfoStudError.Visible := True;
      LbInfoStudError.Caption :=
        'Fehler 3: Eines der erforderlichen Felder enthaelt kein gültiges Datum';
    end;
  end;
end;

procedure TForm1.BtInfoStudPrintQClick(Sender: TObject);
var
  stud: Tstudent;
  studname: string;
begin
  try
    if not (EdInfoStudID.Text = '') then
    begin
      //stud := management.getStudentById(StrToInt(EdInfoStudID.Text));
      studname := management.getSNameById(StrToInt(EdInfoStudID.Text));
      TBarcodePrinter.instance.add_barcode(EdInfoStudID.Text, studname);
      addToPrintingQueueListBox(EdInfoStudID.Text, studname);
    end;

  except
    On EConvertError do
    begin
      LbInfoStudError.Visible := True;
      LbInfoStudError.Caption :=
        'Fehler 3: Eines der erforderlichen Felder enthaelt kein gültiges Datum';
    end;
  end;
end;

procedure TForm1.BtInfoStudShowClick(Sender: TObject);

  function checkBD: string;
  var
    mo: cardinal;
    s: string;
  begin
    mo := SeInfoStudDay.Value;
    if mo < 10 then
      s := '0';
    s := s + IntToStr(mo);
    mo := SeInfoStudMonth.Value;
    if mo < 10 then
      s := '0';
    s := s + IntToStr(mo);
    mo := SeInfoStudYear.Value;
    s := s + IntToStr(mo);
    Result := s;
  end;

var
  birthdate: string;
  stud: TStudent;
begin
  LbInfoStudError.Visible := False;
  try
    birthdate := checkBD;
    stud := management.getStudentbyID(StrToInt(EdInfoStudID.Text));
    EdInfoStudFirstName.Text := stud.getfirstname;
    EdInfoStudLastName.Text := stud.getlastname;
    EdInfoStudGrade.Text := stud.getclassname;
  except
    On EConvertError do
    begin
      LbInfoStudError.Visible := True;
      LbInfoStudError.Caption :=
        'Fehler 3: Eines der erforderlichen Felder enthaelt kein gültiges Datum';
    end;
  end;
end;

procedure TForm1.BtInfoSuportLicenseClick(Sender: TObject);
begin
  OpenURL('https://creativecommons.org/licenses/by-sa/4.0/');
end;

procedure TForm1.BtInfoSuportWikiClick(Sender: TObject);
begin

  OpenURL('https://github.com/Herder-IT-Solutions/HerderBib/wiki');

end;

procedure TForm1.BtPrintClick(Sender: TObject);
begin
  TBarcodePrinter.instance.print;
  LiPrintQueue.Items.Clear;
end;

procedure TForm1.BtRentClick(Sender: TObject);
begin
  LbRentError.Visible := False;
  try
    if (management.BIdCheck(StrToInt(EdRentBook.Text)) and
      management.SIdCheck(StrToInt(EdRentStud.Text))) then
    begin
      //Check if book is already rent
      management.RNew(StrToInt(EdRentBook.Text), StrToInt(EdRentStud.Text));
      EdRentStud.Text := '';
      EdRentBook.Text := '';
    end;

  except
    On EConvertError do
    begin
      LbRentError.Visible := True;
      LbRentError.Caption :=
        'Fehler 3: Eines der erforderlichen Felder enthaelt kein gültiges Datum';
    end;
  end;
end;

procedure TForm1.BtRetClick(Sender: TObject);
begin
  LbRetError.Visible := False;
  try
    if (management.BIdCheck(StrToInt(EdRetBook.Text)) and
      management.SIdCheck(StrToInt(EdRetStud.Text))) then
    begin
      management.BQualiNew(StrToInt(EdRetBook.Text), TBRetBookState.Position);
      management.BBack(StrToInt(EdRetBook.Text), StrToInt(EdRetStud.Text));
      EdRetStud.Text := '';
      EdRetBook.Text := '';
      TBRetBookState.Position := 1;
    end;
  except
    On EConvertError do
    begin
      LbRetError.Visible := True;
      LbRetError.Caption :=
        'Error 3: Eines der erforderlichen Felder enthaelt kein gültiges Datum';
    end;
  end;
  //returnBook(StrToINT(EdRetStud.text),StrToINT(EdRetBook.text),TBRetBookState.Position)
end;

procedure TForm1.BtInfoSuportErrorClick(Sender: TObject);
begin
  OpenURL('https://github.com/Herder-IT-Solutions/HerderBib/wiki/Fehler');
end;


procedure Tform1.addToPrintingQueueListBox(code: string; title: string);
begin
  LiPrintQueue.Items.add(code + ', ' + title);
end;

end.
