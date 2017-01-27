unit student;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TStudent = class
    private
      id:int64;
      last_name:STRING;
      first_name:STRING;
      class_name:STRING;
      birth:TDate;
    public
      // Returns the student id
      // result: id
      function getId():int64;

      // Sets a new student id
      // parameter: newId
      // result: TRUE on success, so if newId is not NULL
      function setId(newId:int64):BOOLEAN;

      // Returns the student's last name
      // result: last_name
      function getLastName():STRING;

      // Sets a new student's last name
      // parameter: newLastName
      // result: TRUE on success, so if newLastName has 0..255 characters and is not NULL
      function setLastName(newLastName:STRING):BOOLEAN;

      // Returns the student's first name
      // result: first_name
      function getFirstName():STRING;

      // Sets a new student's first name
      // parameter: newFirstName
      // result: TRUE on success, so if newFirstName has 0..255 characters and is not NULL
      function setFirstName(newFirstName:STRING):BOOLEAN;

      // Returns the student's class name
      // result: class_name
      function getClassName():STRING;

      // Sets a new student's class name
      // parameter: newClassName
      // result: TRUE on success, so if newClassName has 0..10 characters and is not NULL
      function setClassName(newClassName:STRING):BOOLEAN;

      // Returns the student's date of birth
      // result: date of birth
      function getBirth():TDate;

      // Sets a new student's date of birth
      // parameter: birth
      // result: TRUE on success, so if newBirth >= 0 and not NULL
      function setBirth(newBirth:TDate):BOOLEAN;
  end;

implementation

function TStudent.getId():int64;
begin
  result:=self.id;
end;

function TStudent.setId(newId:int64):BOOLEAN;
begin
  result:=false;
  if (newId <> NULL) then
  begin
    self.id:=newId;
    result:=true;
  end;
end;

function TStudent.getLastName():STRING;
begin
  result:=self.last_name;
end;

function TStudent.setLastName(newLastName:STRING):BOOLEAN;
begin
  result:=false;
  if(Length(newLastName) <= 255) and (newLastName <> NULL) then
  begin
    self.last_name:=newLastName;
    result:=true;
  end;
end;

function TStudent.getFirstName():STRING;
begin
  result:=self.first_name;
end;

function TStudent.setFirstName(newFirstName:STRING):BOOLEAN;
begin
  result:=false;
  if (newFirstName <> NULL) and (Length(newFirstName) <= 255) then
  begin
    self.first_name:=newFirstName;
    result:=true;
  end;
end;

function TStudent.getClassName():STRING;
begin
  result:=self.class_name;
end;

function TStudent.setClassName(newClassName:STRING):BOOLEAN;
begin
  result:=false;
  if (newClassName <> NULL) and (Length(newClassName) <= 10) then
  begin
    self.class_name:=newClassName;
    result:=true;
  end;
end;

function TStudent.getBirth():TDate;
begin
  result:=self.birth;
end;

function TStudent.setBirth(newBirth:TDate):BOOLEAN;
begin
  result:=false;
  if (newBirth <> NuLL) and (newBirth >= 0) then
  begin
    self.birth:=newBirth;
    result:=true;
  end;
end;

end.

