unit book;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TBook = class
    private
      id:CARDINAL;
      isbn:CARDINAL;
      condition:STRING;
    public
      // Returns the book id
      // result: id
      function getId():CARDINAL;

      // Sets a new book id
      // parameter: newId
      // result: TRUE on success, so if newId <= (2^63)-1 and not NULL
      function setId(newId:CARDINAL):BOOLEAN;

      // Returns the book ISBN
      // result: isbn[13]
      function getIsbn():CARDINAL;

      // Sets a new book ISBN
      // parameter: newIsbn
      // result: TRUE on success, so if newIsbn has 13 characters and is not NULL
      function setIsbn(newIsbn:CARDINAL):BOOLEAN;

      // Returns the book condition
      // result: condition
      function getCondition():STRING;

      // Sets a new book condition
      // parameter: newCondition
      // result: TRUE on success
      function setCondition(newCondition:STRING):BOOLEAN;
  end;

implementation

function TBook.getId():CARDINAL;
begin
  result:=self.id;
end;

function TBook.setId(newId:CARDINAL):BOOLEAN;
begin
  result:=false;
  if (newId<=(2^63)-1) and (!newId.isNull) then
  begin
    self.id:=newId;
    result:=true;
  end;
end;

function TBook.getIsbn():CARDINAL;
begin
  result:=self.isbn;
end;

function TBook.setIsbn(newIsbn:CARDINAL):BOOLEAN;
begin
  result:=false;
  if (Length(newIsbn) = 13) and (!newIsbn.isNull) then
  begin
    self.isbn:=newIsbn;
    result:=true;
  end;
end;

function TBook.getCondition():STRING;
begin
  result:=self.condition;
end;

function TBook.setCondition(newCondition:STRING):BOOLEAN;
begin
    self.condition:=newCondition;
    result:=true;
  end;

end.

