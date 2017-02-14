unit gui;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, Spin, ExtCtrls, Grids, Menus, types, sqldb, sqlite3conn, lclintf, uVerwaltung;

type

  { TForm1 }

  TForm1 = class(TForm)
    BtInfoBooktypeEdit: TButton;
    BtInfoBookEdit: TButton;
    BtInfoBooktypeShow: TButton;
    BtInfoBookShow1: TButton;
    BtRent: TButton;
    BtRet: TButton;
    BtAddBook: TButton;
    BtInfoStudShow: TButton;
    BtInfoStudEdit: TButton;
    BtInfoBookDel: TButton;
    BtInfoStudExportRel: TButton;
    BtInfoRelFilter: TButton;
    BtInfoAdminLogin: TButton;
    BtInfoAminLogout: TButton;
    BtInfoSuportWiki: TButton;
    BtInfoAdminSendDBName: TButton;
    CBAddBookSubject: TComboBox;
    CBInfoRelGrade: TComboBox;
    CBInfoRelSubject: TComboBox;
    EdBook: TEdit;
    EdBook1: TEdit;
    EdAddBookName: TEdit;
    EdAddBookISBN: TEdit;
    EdInfoBooktypeISBN: TEdit;
    EdInfoBookID: TEdit;
    EdInfoBooktypeName: TEdit;
    EdInfoStudSur: TEdit;
    EdInfoStudName: TEdit;
    EdInfoStudGrade: TEdit;
    EdInfoStudUser: TEdit;
    EdInfoStudID: TEdit;
    EdInfoBookRent: TEdit;
    EdInfoAdminPw: TEdit;
    EdStud: TEdit;
    EdStud1: TEdit;
    Image1: TImage;
    LEInfoAdminDBName: TLabeledEdit;
    LbInfoAdminCheck: TLabel;
    LbInfoAdmin: TLabel;
    LbInfoSupport: TLabel;
    LbInfoRelSubj: TLabel;
    LbInfoRelGrade: TLabel;
    LbInfoRelFilter: TLabel;
    LbAddBookHint: TLabel;
    LbAddBookError: TLabel;
    LbBookState: TLabel;
    LbInfoBookRent: TLabel;
    LbInfoBookTrack1: TLabel;
    LbBookTrack1: TLabel;
    LbInfoBookTrack3: TLabel;
    LbBookTrack3: TLabel;
    LbInfoBookTrack5: TLabel;
    LbBookTrack5: TLabel;
    LbInfoBooktypeISBN: TLabel;
    LbInfoBookID: TLabel;
    LbInfoBooktypeName: TLabel;
    LbInfoBookState: TLabel;
    LbInfoStudID: TLabel;
    LbInfoStudSur: TLabel;
    LbInfoStudName: TLabel;
    LbInfoStudGrade: TLabel;
    LbInfoStudUser: TLabel;
    LbAddBookISBN1: TLabel;
    LbAddBookQuantity: TLabel;
    LbAddBookName: TLabel;
    LbAddBookISBN: TLabel;
    LbBookName1: TLabel;
    LbInfoSupport2: TLabel;
    LbStudInstruct2: TLabel;
    LbStudInstruct3: TLabel;
    LbStudName: TLabel;
    LbStudInstruct: TLabel;
    LbStudInstruct1: TLabel;
    LbBookName: TLabel;
    LbStudName1: TLabel;
    MeCredits: TMemo;
    MeInfoStudRel: TMemo;
    PageControl1: TPageControl;
    PCInfos: TPageControl;
    SEAddBookQuantity: TSpinEdit;
    SQLite3Connection: TSQLite3Connection;
    SQLQuery: TSQLQuery;
    SQLTransaction: TSQLTransaction;
    StringGrid1: TStringGrid;
    TabRent: TTabSheet;
    TabRet: TTabSheet;
    TabAdd: TTabSheet;
    TabInfo: TTabSheet;
    TabBook: TTabSheet;
    TabRel: TTabSheet;
    TabBooktype: TTabSheet;
    TabCreditsHelp: TTabSheet;
    Admnistration: TTabSheet;
    TabStud: TTabSheet;
    TBInfoBookState: TTrackBar;
    TBBookState: TTrackBar;
    procedure BtAddBookClick(Sender: TObject);
    procedure BtInfoAdminLoginClick(Sender: TObject);
    procedure BtInfoAminLogoutClick(Sender: TObject);
    procedure BtRentClick(Sender: TObject);
    procedure BtRetClick(Sender: TObject);
    procedure BtInfoSuportWikiClick(Sender: TObject);
    procedure confirmNumbers(Sender: TObject; var Key: char);
    procedure EdBook1Change(Sender: TObject);
    procedure EdBookChange(Sender: TObject);
    procedure EdStud1Change(Sender: TObject);
    procedure EdStudChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure TabRetContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;
  management: TVerwaltung;
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

procedure TForm1.FormCreate(Sender: TObject);
begin
 // LbStudInstruct := 'Hello' + #13#10 + 'world';
    PermissionLevel :=1;
    management := TVerwaltung.create(SQLQuery,SQLTransaction,SQLite3Connection)
end;

procedure TForm1.confirmNumbers(Sender: TObject; var Key: char);
begin
if not (Key in ['0'..'9', #8, #9]) then Key := #0;
end;

procedure TForm1.EdBook1Change(Sender: TObject);
begin
   //
   //LbBookName1.Caption:= getBookName(EdBook.Text)
end;

procedure TForm1.EdBookChange(Sender: TObject);
begin
  //LbBookName.Caption:= getBookName(EdBook.Text)
end;

procedure TForm1.EdStud1Change(Sender: TObject);
begin
     //LbStudName1.Caption:= getStudName(EdStud1.Text)
end;

procedure TForm1.EdStudChange(Sender: TObject);
begin
  //LbStudName.Caption:= getStudName(EdStud.Text)
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
     s := EdAddBookISBN.text;    //Beispiel: funktiuoniert bei 9780306406157
     if (length(s) = 13) then b := CheckSumISBN13(s)
     else a:=TRUE;
     LbAddBookError.Visible := False;
     if not (b) then begin
        LbAddBookError.Visible := True;
        LbAddBookError.Caption := 'Fehler 1: Die ISBN ist ungültig (falsche Prüfziffer)';
     end;
     if a then begin
        LbAddBookError.Visible := True;
        LbAddBookError.Caption := 'Fehler 2: Die ISBN ist nicht 13 Ziffern lang';
     end;

      k:=1;

     if not( management.BTypeCheck(s)) then management.BookTypeNew(s,EdAddBookName.Text, CBAddBookSubject.Text);  //Wenn ISBN noch nicht bekannt dann füge buch hinzu
     while (k <= SeAddBookQuantity.Value) do begin     //füge bücher hinzu
         management.BookAdd(s);
         INC(k)
     end;
     //exexute print.exe -s -LbAddBookName.Text -CBAddBookSubject.Text -SeAddBookQuantity.Value

     end;

procedure TForm1.BtInfoAdminLoginClick(Sender: TObject);
begin
  if EdInfoAdminPw.Text='h3rd3r' then begin
     PermissionLevel:=0;
     //EdInfoAdminPw.Text=NONE;
     LbInfoAdminCheck.Caption := 'Sie sind Administrator';
     BtInfoStudEdit.Enabled := True;
  end;
end;

procedure TForm1.BtInfoAminLogoutClick(Sender: TObject);
begin
     PermissionLevel:=1;
     LbInfoAdminCheck.Caption := 'Sie sind nicht Administrator';
     BtInfoStudEdit.Enabled := False;
end;

procedure TForm1.BtRentClick(Sender: TObject);
begin
  if (management.BIdCheck(STRTOINT(EdBook.text)) and management.SIdCheck(STRTOINT(EdStud.text))) then begin
     //Check if book is already rent
     management.BookRentStu(STRTOINT(EdBook.text), STRTOINT(EdStud.text));
  end;
end;

procedure TForm1.BtRetClick(Sender: TObject);
begin
    if (management.BIdCheck(STRTOINT(EdBook1.text)) and management.SIdCheck(STRTOINT(EdStud1.text))) then begin
     management.BookQualiNew(STRTOINT(EdBook1.text), TBBookState.Position);
     management.BookBack(STRTOINT(EdBook1.text), STRTOINT(EdStud1.text));
  end;
  //returnBook(StrToINT(EdStud1.text),StrToINT(EdBook1.text),TBBookState.Position)
end;

procedure TForm1.BtInfoSuportWikiClick(Sender: TObject);
begin
     OpenURL('https://github.com/Herder-IT-Solutions/HerderBib/wiki');
end;



end.

