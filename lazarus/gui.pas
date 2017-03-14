unit gui;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, Spin, ExtCtrls, Grids, Menus, types, sqldb, sqlite3conn, lclintf,
  Buttons, CheckLst, uDBManagement, Student, Book, Rental, Booktype,LConvEncoding,uBarcodePrint ;

type

  { TForm1 }

  TForm1 = class(TForm)
    BtInfoBooktypeEdit: TButton;
    BtInfoBookEdit: TButton;
    BtInfoBooktypeShow: TButton;
    BtInfoBookShow1: TButton;
    BtInfoSuportError: TButton;
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
    Admnistration: TTabSheet;
    TabPrint: TTabSheet;
    TabStud: TTabSheet;
    TBInfoBookState: TTrackBar;
    TBRetBookState: TTrackBar;
    procedure BtAddBookClick(Sender: TObject);
    procedure BtInfoAdminLoginClick(Sender: TObject);
    procedure BtInfoAdminLogoutClick(Sender: TObject);
    procedure BtInfoBookDelClick(Sender: TObject);
    procedure BtInfoBookShow1Click(Sender: TObject);
    procedure BtInfoBooktypeShowClick(Sender: TObject);
    procedure BtInfoRelFilterClick(Sender: TObject);
    procedure BtInfoStudEditClick(Sender: TObject);
    procedure BtInfoStudPrintQClick(Sender: TObject);
    procedure BtInfoStudShowClick(Sender: TObject);
    procedure BtPrintClick(Sender: TObject);
    procedure BtRentClick(Sender: TObject);
    procedure BtRetClick(Sender: TObject);
    procedure BtInfoSuportWikiClick(Sender: TObject);
    procedure confirmNumbers(Sender: TObject; var Key: char);
    procedure EdRetBookChange(Sender: TObject);
    procedure EdRentBookChange(Sender: TObject);
    procedure EdInfoStudFirstNameChange(Sender: TObject);
    procedure EdRetStudChange(Sender: TObject);
    procedure EdRentStudChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LbRetStudNameClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
    procedure PCInfosChange(Sender: TObject);
    procedure TabRetContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;
  management: TDBManagement;
  PermissionLevel:CARDINAL;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.TabRetContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin

end;

procedure TForm1.PageControl1Change(Sender: TObject);
begin

end;

procedure TForm1.Panel1Click(Sender: TObject);
begin

end;

procedure TForm1.PCInfosChange(Sender: TObject);
begin
   if management.isConnected then LbInfoAdminConnection.Caption := 'Datenbankverbindung hergestellt'
   else if not (management.isConnected) then LbInfoAdminConnection.Caption := 'Datenbankverbindung nicht hergestellt'
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
 // LbRentStudInstruct := 'Hello' + #13#10 + 'world';
    PermissionLevel :=1;
    management := tdbmanagement.create();
    //management := TVerwaltung.create(SQLQuery,SQLTransaction,SQLite3Connection)
end;

procedure TForm1.LbRetStudNameClick(Sender: TObject);
begin

end;


procedure TForm1.confirmNumbers(Sender: TObject; var Key: char);
begin
if not (Key in ['0'..'9', #8, #9]) then Key := #0;
end;

procedure TForm1.EdRetBookChange(Sender: TObject);
begin
   //
   //LbRetBookName.Caption:= getBookName(EdRentBook.Text)
end;

procedure TForm1.EdRentBookChange(Sender: TObject);
Var book :TBook;
begin
  if not (EdRentBook.text='') then
  begin
       book :=  management.getBookByID(STRTOINT(EdRentBook.Text));
            if not (book=nil) then
                LbRentBookName.Caption:= book.getISBN
            else
                LbRentBookname.caption := '';
  end
  else LbRentBookname.caption := '';
end;

procedure TForm1.EdInfoStudFirstNameChange(Sender: TObject);
begin

end;

procedure TForm1.EdRetStudChange(Sender: TObject);
Var stu :TSTUDENT;
begin
  if not (EdRetStud.text='') then
  begin
       stu :=  management.getStudentByID(STRTOINT(EdRetStud.Text));
            if not (stu=nil) then
                LbRetStudName.Caption:= stu.getFirstName() + ' ' + stu.getLastName()
            else
                LbRetStudname.caption := '';
  end
  else LbRetStudname.caption := '';
end;

procedure TForm1.EdRentStudChange(Sender: TObject);
Var stu :TSTUDENT;
begin
  if not (EdRentStud.text='') then
  begin
       stu :=  management.getStudentByID(STRTOINT(EdRentStud.Text));
            if not (stu=nil) then
                LbRentStudName.Caption:= stu.getFirstName() + ' ' + stu.getLastName()
            else
                LbRentStudname.caption := '';
  end
  else LbRentStudname.caption := '';
end;

procedure TForm1.BtAddBookClick(Sender: TObject);
var s:STRING;
  a,b: BOOLEAN;
  k: CARDINAL;


  function CheckSumISBN13(isbn:STRING): BOOLEAN;
  var
    i,sum,check: CARDINAL;
  begin
       check:=0;
      sum:=0;
      i:=1;
      while i<13 do begin
          if (i mod 2 = 0) then sum := sum + 3*StrToInt(isbn[i])
          else if (i mod 2 = 1) then sum := sum + StrToInt(isbn[i]);
          INC(i);
      end;
      check := (10-(sum mod 10));
      result := (check = StrToInt(isbn[13]));
  end;
begin
     a := FALSE;
     s := EdAddBookISBN.text;    //Beispiel: funktiuoniert bei 9780306406157
     if not (s = '') then begin

        if (length(s) = 13) then b := CheckSumISBN13(s)
        else a:=TRUE;
        LbAddBookError.Visible := False;
        if not (b) then begin
           LbAddBookError.Visible := True;
           LbAddBookError.Caption := 'Fehler 1: Die ISBN ist ungültig (falsche Prüfziffer)';
           exit;
        end;
        if a then begin
           LbAddBookError.Visible := True;
           LbAddBookError.Caption := 'Fehler 2: Die ISBN ist nicht 13 Ziffern lang';
           exit;
        end;

      k:=1;

     if not( management.BTypeCheck(s)) then management.BTypeNew(s,EdAddBookName.Text, CBAddBookSubject.Text);  //Wenn ISBN noch nicht bekannt dann füge buch hinzu
     while (k <= SeAddBookQuantity.Value) do begin     //füge bücher hinzu
         management.BNew(s);
         INC(k);
         //TBarcodePrinter.instance.add_barcode(9342, 'jfdisfjo')
     end;
     EdAddBookName.text := '';
     EdAddBookISBN.text := '';
         CBAddBookSubject.text := '';
         SEAddBookQuantity.Value:= 0;
     end
     else begin
         LbAddBookError.Visible := True;
         LbAddBookError.Caption := 'Fehler 3: Eines der erforderlichen Felder enthaelt kein gültiges Datum';
         exit;
     end;
end;

procedure TForm1.BtInfoAdminLoginClick(Sender: TObject);
begin
  if EdInfoAdminPw.Text='h3rd3r' then begin
     PermissionLevel:=0;
     //EdInfoAdminPw.Text=NONE;
     LbInfoAdminCheck.Caption := 'Sie sind Administrator';
     BtInfoStudEdit.Enabled := True;
     BtInfoAdminLogin.Enabled:= False;
     BtInfoAdminLogout.Enabled:= True;

  end;
  EdInfoAdminPw.Text := '';
end;

procedure TForm1.BtInfoAdminLogoutClick(Sender: TObject);
begin
     PermissionLevel:=1;
     LbInfoAdminCheck.Caption := 'Sie sind nicht Administrator';
     BtInfoStudEdit.Enabled := False;
     BtInfoAdminLogin.Enabled:= True;
     BtInfoAdminLogout.Enabled:= False;
end;

procedure TForm1.BtInfoBookDelClick(Sender: TObject);
var b:TBook;
begin
  b := management.getBookByID(STRTOINT(EdInfoBookID.text));
  management.BDel(b);
  EdInfoBookId.text:='';
  EdInfoBookRent.text:='';
  TBInfoBookState.Position:=1;
end;

procedure TForm1.BtInfoBookShow1Click(Sender: TObject);
begin
     LbInfoBookError.Visible := FALSE;
  try

  except
    On EConvertError do begin
        LbInfoBookError.Visible := TRUE;
        LbInfoBookError.Caption := 'Fehler 3: Eines der erforderlichen Felder enthaelt kein gültiges Datum';
    end;
  end;

end;

procedure TForm1.BtInfoBooktypeShowClick(Sender: TObject);
begin
    LbInfoBooktypeError.Visible := FALSE;
  try
     if not(management.BTypeCheck(EdInfoBooktypeISBN.text)) then
     begin
     LbInfoBooktypeError.Visible := TRUE;
     LbInfoBooktypeError.Caption := 'Fehler 4: Die ISBN ist keinem Buchtyp zugeordnet';
     end
     else if  management.BTypeCheck(EdInfoBooktypeISBN.text) then
     begin
     LbInfoBooktypeError.Visible := FALSE;
     //Anfrage an uDBManagement stellen
     end;
  except
    On EConvertError do begin
        LbInfoBooktypeError.Visible := TRUE;
        LbInfoBooktypeError.Caption := 'Fehler 3: Eines der erforderlichen Felder enthaelt kein gültiges Datum';
    end;
  end;

end;

procedure TForm1.BtInfoRelFilterClick(Sender: TObject);
procedure printLine (s: STRING ; li : CARDINAL);
var len,i : CARDINAL;
begin
     len := length(s);
     i   := 1;
     while i < (len+1) do
     begin
          if s[i]=';' then s[i]:=' ';
          INC(i);
     end;
     MeInfoRel.Lines[li]:=s
end;

procedure readCSV(dataname: STRING);
   var f : TextFile;
    str : String;
    cur: CARDINAL;
   begin
     AssignFile (f,dataname);
     reset (f);
     cur:=0;
     while not EOF (f) do
     begin
       cur:=cur+1;
       readln (f,str);
       //SomeRTLRoutine(UTF8ToAnsi(str));
       printLine(AnsiToUTF8(str),cur)
     end;
     CloseFile (f);
end;
begin;
MeInfoRel.Clear;
readCSV('rental_relations.csv');
//MeInfoRel.Lines.Text := ConvertEncoding(MeInfoRel.Lines.Text, GuessEncoding(MeInfoRel.Lines.Text), EncodingUTF8);
end;

procedure TForm1.BtInfoStudEditClick(Sender: TObject);
  var
    stud : TStudent;
begin
       LbInfoStudError.Visible := FALSE;
  try
       stud := management.getStudentbyID(STRTOINT(EdInfoStudID.text));
       stud.setlastname(EdInfoStudLastName.text);
       stud.setfirstname(EdInfoStudFirstName.text);
       stud.setclassname(EdInfoStudGrade.text);
       management.supdate(stud);
  except
    On EConvertError do begin
        LbInfoStudError.Visible := TRUE;
        LbInfoStudError.Caption := 'Fehler 3: Eines der erforderlichen Felder enthaelt kein gültiges Datum';
    end;
  end;
end;

procedure TForm1.BtInfoStudPrintQClick(Sender: TObject);
begin
  try
     TBarcodePrinter.instance.add_barcode(9342, 'jfdisfjo');
  except
    On EConvertError do begin
        LbInfoStudError.Visible := TRUE;
        LbInfoStudError.Caption := 'Fehler 3: Eines der erforderlichen Felder enthaelt kein gültiges Datum';
    end;
  end;
end;

procedure TForm1.BtInfoStudShowClick(Sender: TObject);
function checkBD:STRING;
var mo :CARDINAL;
    s :STRING;
begin
   mo :=  SeInfoStudDay.Value;
   if mo < 10 then s := '0';
   s := s + IntToStr(mo);
   mo :=  SeInfoStudMonth.Value;
   if mo < 10 then s := '0';
   s := s + IntToStr(mo);
   mo :=  SeInfoStudYear.Value;
   s := s + IntToStr(mo);
   result := s;
end;
var
    birthdate: String;
    stud : TStudent;
begin
       LbInfoStudError.Visible := FALSE;
  try
       birthdate:=checkBD;
       stud := management.getStudentbyID(STRTOINT(EdInfoStudID.text));
       EdInfoStudFirstName.text:=stud.getfirstname;
       EdInfoStudLastName.text:=stud.getlastname;
       EdInfoStudGrade.text:=stud.getclassname;
  except
    On EConvertError do begin
        LbInfoStudError.Visible := TRUE;
        LbInfoStudError.Caption := 'Fehler 3: Eines der erforderlichen Felder enthaelt kein gültiges Datum';
    end;
  end;
end;

procedure TForm1.BtPrintClick(Sender: TObject);
begin
  TBarcodePrinter.instance.print;
end;

procedure TForm1.BtRentClick(Sender: TObject);
begin
  LbRentError.Visible := FALSE;
  try
  if (management.BIdCheck(STRTOINT(EdRentBook.text)) and management.SIdCheck(STRTOINT(EdRentStud.text))) then begin
     //Check if book is already rent
     management.RNew(STRTOINT(EdRentBook.text), STRTOINT(EdRentStud.text));
     EdRentStud.text:='';
     EdRentBook.text:='';
  end;

  except
    On EConvertError do begin
        LbRentError.Visible := TRUE;
        LbRentError.Caption := 'Fehler 3: Eines der erforderlichen Felder enthaelt kein gültiges Datum';
    end;
  end;
end;

procedure TForm1.BtRetClick(Sender: TObject);
begin
     LbRetError.Visible := FALSE;
  try
    if (management.BIdCheck(STRTOINT(EdRetBook.text)) and management.SIdCheck(STRTOINT(EdRetStud.text))) then begin
     management.BQualiNew(STRTOINT(EdRetBook.text), TBRetBookState.Position);
     management.BBack(STRTOINT(EdRetBook.text), STRTOINT(EdRetStud.text));
     EdRetStud.text:='';
     EdRetBook.text:='';
     TBRetBookState.Position :=1;
  end;
      except
    On EConvertError do begin
        LbRetError.Visible := TRUE;
        LbRetError.Caption := 'Error 3: Eines der erforderlichen Felder enthaelt kein gültiges Datum';
    end;
  end;
  //returnBook(StrToINT(EdRetStud.text),StrToINT(EdRetBook.text),TBRetBookState.Position)
end;

procedure TForm1.BtInfoSuportWikiClick(Sender: TObject);
begin
     OpenURL('https://github.com/Herder-IT-Solutions/HerderBib/wiki/Fehler');
end;



end.

