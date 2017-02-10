unit book;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TBook = class
    private
      id:int64;
      isbn:STRING;
      condition:CARDINAL;
    public
      // Returns the book id
      // result: id
      function getId():int64;

      // Sets a new book id
      // parameter: newId
      // result: TRUE on success, so if newId not NULL
      function setId(newId:int64):BOOLEAN;

      // Returns the book ISBN
      // result: isbn[13]
      function getIsbn():STRING;

      // Sets a new book ISBN
      // parameter: newIsbn
      // result: TRUE on success, so if newIsbn has 13 characters and is not NULL
      function setIsbn(newIsbn:STRING):BOOLEAN;

      // Returns the book condition
      // result: condition[1]
      function getCondition():CARDINAL;

      // Sets a new book condition
      // parameter: newCondition
      // result: TRUE on success, so if newIsbn has 1 character, is <=5 and not NULL
      function setCondition(newCondition:CARDINAL):BOOLEAN;
  end;

implementation

function TBook.getId():int64;
begin
  result:=self.id;
end;

function TBook.setId(newId:int64):BOOLEAN;
begin
  result:=false;
  if (newId <> NULL) then
  begin
    self.id:=newId;
    result:=true;
  end;
end;

function TBook.getIsbn():STRING;
begin
  result:=self.isbn;
end;

function TBook.setIsbn(newIsbn:STRING):BOOLEAN;
begin
  result:=false;
  if (newIsbn <> NULL) and (length(newIsbn) = 13) then
  begin
    self.isbn:=newIsbn;
    result:=true;
  end;
end;

function TBook.getCondition():CARDINAL;
begin
  result:=self.condition;
end;

function TBook.setCondition(newCondition:CARDINAL):BOOLEAN;
begin
  result:=false;
  if  (newCondition <> NULL) and (newCondition <= 6) then
  begin
    self.condition:=newCondition;
    result:=true
  end
end;
end.

