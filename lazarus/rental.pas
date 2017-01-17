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
      function getBookId():CARDINAL;
      function setBookId(newBookId:CARDINAL):BOOLEAN; //true on success
      function getStudentId():CARDINAL;
      function setStudentId(newStudentId:CARDINAL):BOOLEAN;
      function getReturnDate():TDateTime;
      function setReturnDate(newReturnDate:TDateTime):BOOLEAN;
      function getRentalDate():TDateTime;
      function setRentalDate(newRentalDate:TDateTime):BOOLEAN;
  end;

implementation

end.

