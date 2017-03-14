unit book;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DBConstants;

type
  TBook = class
  private
    id: int64;
    isbn: string;
    condition: cardinal;
  public
    // Returns the book id
    // result: id
    function getId(): int64;

    // Sets a new book id
    // parameter: newId
    // result: TRUE on success, so if newId not NULL
    function setId(newId: int64): boolean;

    // Returns the book ISBN
    // result: isbn[13]
    function getIsbn(): string;

    // Sets a new book ISBN
    // parameter: newIsbn
    // result: TRUE on success, so if newIsbn has 13 characters and is not NULL
    function setIsbn(newIsbn: string): boolean;

    // Returns the book condition
    // result: condition[1]
    function getCondition(): cardinal;

    // Sets a new book condition
    // parameter: newCondition
    // result: TRUE on success, so if newIsbn has 1 character, is <=5 or is =SQLNull
    function setCondition(newCondition: cardinal): boolean;

    constructor Create;
  end;

implementation

function TBook.getId(): int64;
begin
  Result := self.id;
end;

function TBook.setId(newId: int64): boolean;
begin
  Result := False;
  if (newId <> NULL) then
  begin
    self.id := newId;
    Result := True;
  end;
end;

function TBook.getIsbn(): string;
begin
  Result := self.isbn;
end;

function TBook.setIsbn(newIsbn: string): boolean;
var
  isbn1, isbn2: cardinal;
begin
  Result := False;
  if length(newIsbn) = 13 then
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

function TBook.getCondition(): cardinal;
begin
  Result := self.condition;
end;

function TBook.setCondition(newCondition: cardinal): boolean;
begin
  Result := False;
  if (newCondition = SQLNull) or (newCondition <= 6) then
  begin
    self.condition := newCondition;
    Result := True;
  end;
end;

constructor TBook.Create;
begin
  self.id := SQLNull;
  self.isbn := '';
  self.condition := SQLNull;
end;

end.
