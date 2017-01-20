unit student;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TStudent = class
    private
      id:CARDINAL;
      last_name:STRING;
      first_name:STRING;
      class_name:STRING;
    public
      // Returns the student id
      // result: id
      function getId():CARDINAL;

      // Sets a new student id
      // parameter: newId
      // result: TRUE on success, so if newId <= (2^63)-1) and not NULL
      function setId(newId:CARDINAL):BOOLEAN;

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
  end;

implementation

function TStudent.getId():CARDINAL;
begin
  result:=self.id;
end;

function TStudent.setId(newId:CARDINAL):BOOLEAN;
begin
  result:=false;
  if (newId<=(2^63)-1) and (!newId.isNull) then
  begin
    self.id:=newId;
    result:=true;
  end;
end;

function TStudent.getLastName():STRING;
begin
  result:=self.last_name;
end;

function TStudent.setLastName(newKastName:STRING):BOOLEAN;
begin
  result:=false;
  if(Length(newLastName) <= 255) and (!newLastName.isNull) then
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
  if(Length(newFirstName) <= 255) and (!newFirstName.isNull) then
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
  if(Length(newClassName) <= 10) and (!newClassName.isNull) then
  begin
    self.class_name:=newClassName;
    result:=true;
  end;
end;

end.

