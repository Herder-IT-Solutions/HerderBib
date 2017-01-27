unit rental;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TRental = class
    private
      book_id:int64;
      student_id:int64;
      return_date:TDateTime;
      rental_date:TDateTime;
    public
      // Returns the book id
      // result: book_id
      function getBookId():int64;

      // Sets a new book id
      // parameter: newBookId
      // result: TRUE on success, so if newBookID is not NULL
      function setBookId(newBookId:int64):BOOLEAN;

      // Returns the student id
      // result: student_id
      function getStudentId():int64;

      // Sets a new sudent id
      // parameter: newStudentId
      // result: TRUE on success, so if new StudentId is not NULL
      function setStudentId(newStudentId:int64):BOOLEAN;

      // Returns the book return date
      // result: return_date
      function getReturnDate():TDateTime;

      // Sets a new book return datetime in seconds
      // parameter: newReturnDate
      // result: TRUE on success, so if newReturnDate >= 0 and not NULL
      function setReturnDate(newReturnDate:TDateTime):BOOLEAN;

      // Returns the book rental datetime in seconds
      // result: rental_date
      function getRentalDate():TDateTime;

      // Sets a new book rental datetime in seconds
      // parameter: newRentalDate
      // result: TRUE on success, so if newReturnDate >= 0 and not NULL
      function setRentalDate(newRentalDate:TDateTime):BOOLEAN;
  end;

implementation

function TRental.getBookId():int64;
begin
  result:=self.book_id;
end;

function TRental.setBookId(newBookId:int64):BOOLEAN;
begin
  result:=false;
  if (newBookId <> NULL) then
  begin
    self.book_id:=newBookId;
    result:=true;
  end;
end;

function TRental.getStudentId():int64;
begin
  result:=self.student_id;
end;

function TRental.setStudentId(newStudentId:int64):BOOLEAN;
begin
  result:=false;
  if (newStudentId <> NULL) then
  begin
    self.student_id:=newStudentId;
    result:=true;
  end;
end;

function TRental.getReturnDate():TDateTime;
begin
  result:=self.return_date;
end;

function TRental.setReturnDate(newReturnDate:TDateTime):BOOLEAN;
begin
  result:=false;
  if (newReturnDate <> NuLL) and (newReturnDate >= 0) then
  begin
    self.return_date:=newReturnDate;
    result:=true;
  end;
end;

function TRental.getRentalDate():TDateTime;
begin
  result:=self.rental_date;
end;

function TRental.setRentalDate(newRentalDate:TDateTime):BOOLEAN;
begin
  result:=false;
  if  (newRentalDate <> NULL) and (newRentalDate>=0) then
  begin
    self.rental_date:=newRentalDate;
    result:=true;
  end;
end;

end.

