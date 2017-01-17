unit booktype;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TBooktype = class
    private
      isbn:CARDINAL;
      title:STRING;
      subject:STRING;
      storage:INTEGER;
    public
      function getIsbn():CARDINAL;
      function setIsbn(newIsbn:CARDINAL):BOOLEAN; //true on success
      function getTitle():STRING;
      function setTitle(newTitle:STRING):BOOLEAN;
      function getSubject():STRING;
      function setSubject(newSubject:STRING):BOOLEAN;
      function getStorage():INTEGER;
      function setStorage(newStorage:INTEGER):BOOLEAN;
  end;

implementation

end.

