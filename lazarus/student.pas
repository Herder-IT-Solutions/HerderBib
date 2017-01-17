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
      function getId():CARDINAL;
      function setId(newId:CARDINAL):BOOLEAN; //true on success
      function getLastName():STRING;
      function setLastName(newLastName:STRING):BOOLEAN;
      function getFirstName():STRING;
      function setFirstName(newFirstName:STRING):BOOLEAN;
      function getClassName():STRING;
      function setClassName(newClassName:STRING):BOOLEAN;
  end;

implementation

end.

