unit gui;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, Spin, ExtCtrls, Grids, types, sqldb, sqlite3conn;

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
    CBSubject: TComboBox;
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
    EdStud: TEdit;
    EdStud1: TEdit;
    Image1: TImage;
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
    TabCredits: TTabSheet;
    TabStud: TTabSheet;
    TBInfoBookState: TTrackBar;
    TBBookState: TTrackBar;
    procedure BtAddBookClick(Sender: TObject);
    procedure confirmNumbers(Sender: TObject; var Key: char);
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
end;

procedure TForm1.confirmNumbers(Sender: TObject; var Key: char);
begin
if not (Key in ['0'..'9', #8, #9]) then Key := #0;
end;

procedure TForm1.BtAddBookClick(Sender: TObject);
var s:STRING;
  i,sum: CARDINAL;
begin
     i := 1;
     sum := 0;
     s := '9780306406157';//EdAddBookISBN.text;
     while i<14 do begin
       if (i mod 2=0) then sum := sum + StrToInt(s[i])
       else sum := sum + StrToInt(s[i])*3;
       INC(i);
     end;
     if not (sum mod 10=0) then
     begin
     LbAddBookError.Visible := True;
     LbAddBookError.Caption := 'Error 1: Die ISBN entspricht nicht der ISBN-13 Berechnung';
     end;
     end;

end.

