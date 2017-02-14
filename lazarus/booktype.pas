unit booktype;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TBooktype = class
    private
      isbn:STRING;
      title:STRING;
      subject:STRING;
      storage:INTEGER;
    public
      // Returns the booktype ISBN
      // result: isbn[13]
      function getIsbn():STRING;

      // Sets a new booktype ISBN
      // parameter: newIsbn
      // result: TRUE on success, so if newIsbn has 13 characters and is not NULL
      function setIsbn(newIsbn:STRING):BOOLEAN;

      // Returns the booktype title
      // result: title[0..255]
      function getTitle():STRING;

      // Sets a new booktype title
      // parameter: newTitle
      // result: TRUE on success, so if newTitle is not NULL
      function setTitle(newTitle:STRING):BOOLEAN;

      // Returns the booktype subject
      // result: subject
      function getSubject():STRING;

      // Sets a new booktype subject
      // parameter: newSubject
      // result: TRUE on success, so if newSubject is not NULL
      function setSubject(newSubject:STRING):BOOLEAN;

      // Returns the booktype storage
      // result: storage
      function getStorage():INTEGER;

      // Sets a new book storage
      // parameter: newStorage
      // result: TRUE on success, so if newStorage <= (2^63)-1)
      function setStorage(newStorage:INTEGER):BOOLEAN;
  end;

implementation

function TBooktype.getIsbn():STRING;
begin
  result:=self.isbn;
end;

function TBooktype.setIsbn(newIsbn:STRING):BOOLEAN;
begin
  result:=false;
  if (newIsbn <> NULL) and (length(newIsbn) = 13) then
  begin
    self.isbn:=newIsbn;
    result:=true;
  end;
end;

function TBooktype.getTitle():STRING;
begin
  result:=self.title;
end;

function TBooktype.setTitle(newTitle:STRING):BOOLEAN;
begin
  result:=false;
  if(newTitle <> NULL) then
  begin
    self.title:=newTitle;
    result:=true;
  end;
end;

function TBooktype.getSubject():STRING;
begin
  result:=self.subject;
end;

function TBooktype.setSubject(newSubject:STRING):BOOLEAN;
begin
  result:=false;
  if(newSubject <> NULL) then
  begin
    self.subject:=newSubject;
    result:=true;
  end;
end;

function TBooktype.getStorage():INTEGER;
begin
  result:=self.storage;
end;

function TBooktype.setStorage(newStorage:INTEGER):BOOLEAN;
begin
  result:=false;
  if (newStorage <> NULL) then
  begin
    self.storage:=newStorage;
    result:=true;
  end;
end;

end.

