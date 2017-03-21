unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, DBConnection, sqlite3conn, rental, student;

type

  { TForm1 }

  TForm1 = class(TForm)
    bnConnect: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    procedure bnConnectClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { private declarations }
    dbcon: TDBConnection;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.bnConnectClick(Sender: TObject);
begin
  dbcon := TDBConnection.Create('buchverleih.sqlite');
  //ShowMessage(BoolToStr(dbcon.getError = nil, 'true', 'false'));
  //ShowMessage(BoolToStr(dbcon.isConnected, 'true', 'false'));

end;

procedure TForm1.Button2Click(Sender: TObject);
var
  rental: TRental;
begin
  rental := TRental.Create;
  rental.setId(2);
  rental.setBookId(1);
  rental.setStudentId(1);
  rental.setRentalDate(0);
  ShowMessage(BoolToStr(dbcon.updateInsertRental(rental)));
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  arr: ArrayOfRentals;
begin
  arr := dbcon.getRentals;
  ShowMessage(DateToStr(arr[0].getReturnDate));
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  //  dbcon.deleteStudent(1);
end;


end.
