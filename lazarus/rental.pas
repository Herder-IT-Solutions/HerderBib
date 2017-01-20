unit rental;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TRental = class
    private
      book_id:CARDINAL;
      student_id:CARDINAL;
      return_date:TDateTime;
      rental_date:TDateTime;
    public
      // Returns the book id
      // result: book_id
      function getBookId():CARDINAL;

      // Sets a new book id
      // parameter: newBookId
      // result: TRUE on success, so if newBookID <= (2^63)-1) and is not NULL
      function setBookId(newBookId:CARDINAL):BOOLEAN;

      // Returns the student id
      // result: student_id
      function getStudentId():CARDINAL;

      // Sets a new sudent id
      // parameter: newStudentId
      // result: TRUE on success, so if new StudentId <= (2^63)-1) and not NULL
      function setStudentId(newStudentId:CARDINAL):BOOLEAN;

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

function TRental.getBookId():CARDINAL;
begin
  result:=self.book_id;
end;

function TRental.setBookId(newBookId:CARDINAL):BOOLEAN;
begin
  result:=false;
  if (newBookId<=(2^63)-1) and (!newBookId.isNull) then
  begin
    self.book_id:=newBookId;
    result:=true;
  end;
end;

function TRental.getStudentId():CARDINAL;
begin
  result:=self.student_id;
end;

function TRental.setStudentId(newStudentId:CARDINAL):BOOLEAN;
begin
  result:=false;
  if (newStudentId<=(2^63)-1) and (!newStudentId.isNull) then
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
  if (newReturnDate>=0) and (!newReturnDate.isNull) then
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
  if (newRentalDate>=0) and (!newRentalDate.isNull) then
  begin
    self.rental_date:=newRentalDate;
    result:=true;
  end;
end;

end.

