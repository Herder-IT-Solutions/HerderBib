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
      function getId():CARDINAL;
      function setId(newId:CARDINAL):BOOLEAN; //true on success
      function getIsbn():CARDINAL;
      function setIsbn(newIsbn:CARDINAL):BOOLEAN;
      function getCondition():STRING;
      function setCondition(newCondition:STRING):BOOLEAN;
  end;

implementation

end.

