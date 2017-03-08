unit booktype;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TBooktype = class
  private
    isbn: string;
    title: string;
    subject: string;
    storage: integer;
  public
    // Returns the booktype ISBN
    // result: isbn[13]
    function getIsbn(): string;

    // Sets a new booktype ISBN
    // parameter: newIsbn
    // result: TRUE on success, so if newIsbn has 13 characters and is not NULL
    function setIsbn(newIsbn: string): boolean;

    // Returns the booktype title
    // result: title[0..255]
    function getTitle(): string;

    // Sets a new booktype title
    // parameter: newTitle
    // result: TRUE on success, so if newTitle is not NULL
    function setTitle(newTitle: string): boolean;

    // Returns the booktype subject
    // result: subject
    function getSubject(): string;

    // Sets a new booktype subject
    // parameter: newSubject
    // result: TRUE on success, so if newSubject is not NULL
    function setSubject(newSubject: string): boolean;

    // Returns the booktype storage
    // result: storage
    function getStorage(): integer;

    // Sets a new book storage
    // parameter: newStorage
    // result: TRUE on success, so if newStorage <= (2^63)-1)
    function setStorage(newStorage: integer): boolean;

    constructor Create;
  end;

implementation

function TBooktype.getIsbn(): string;
begin
  Result := self.isbn;
end;

function TBooktype.setIsbn(newIsbn: string): boolean;
var
  isbn1, isbn2: cardinal;
begin
  Result := False;
  if (newIsbn <> NULL) and (length(newIsbn) = 13) then
  begin
    try // is numeric?
      isbn1 := StrToInt(Copy(newIsbn, 0, 8));
      isbn2 := StrToInt(Copy(newIsbn, 8, 5));
    except
      On E: EConvertError do
        exit;
    end;

    self.isbn := newIsbn;
    Result := True;
  end;
end;

function TBooktype.getTitle(): string;
begin
  Result := self.title;
end;

function TBooktype.setTitle(newTitle: string): boolean;
begin
  Result := False;
  if (newTitle <> NULL) then
  begin
    self.title := newTitle;
    Result := True;
  end;
end;

function TBooktype.getSubject(): string;
begin
  Result := self.subject;
end;

function TBooktype.setSubject(newSubject: string): boolean;
begin
  Result := False;
  if (newSubject <> NULL) then
  begin
    self.subject := newSubject;
    Result := True;
  end;
end;

function TBooktype.getStorage(): integer;
begin
  Result := self.storage;
end;

function TBooktype.setStorage(newStorage: integer): boolean;
begin
  Result := False;
  if (newStorage <> NULL) then
  begin
    self.storage := newStorage;
    Result := True;
  end;
end;

constructor TBooktype.Create;
begin
  self.isbn := '';
  self.title := '';
  self.subject := '';
  self.storage := 0;
end;

end.
