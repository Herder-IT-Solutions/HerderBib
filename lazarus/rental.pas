unit rental;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TRental = class
  private
    id: LargeInt;
    book_id: LargeInt;
    student_id: LargeInt;
    return_date: TDate;
    rental_date: TDate;
  public
    // Returns the id
    // result: id
    function getId(): LargeInt;

    // Sets a new id
    // parameter: newId
    // result: TRUE on success, so if newID is not NULL
    function setId(newId: LargeInt): boolean;

    // Returns the book id
    // result: book_id
    function getBookId(): LargeInt;

    // Sets a new book id
    // parameter: newBookId
    // result: TRUE on success, so if newBookID is not NULL
    function setBookId(newBookId: LargeInt): boolean;

    // Returns the student id
    // result: student_id
    function getStudentId(): LargeInt;

    // Sets a new sudent id
    // parameter: newStudentId
    // result: TRUE on success, so if new StudentId is not NULL
    function setStudentId(newStudentId: LargeInt): boolean;

    // Returns the book return date
    // result: return_date
    function getReturnDate(): TDate;

    // Sets a new book return datetime in seconds
    // parameter: newReturnDate
    // result: TRUE on success, so if newReturnDate >= 0 and not NULL
    function setReturnDate(newReturnDate: TDate): boolean;

    // Returns the book rental datetime in seconds
    // result: rental_date
    function getRentalDate(): TDate;

    // Sets a new book rental datetime in seconds
    // parameter: newRentalDate
    // result: TRUE on success, so if newReturnDate >= 0 and not NULL
    function setRentalDate(newRentalDate: TDate): boolean;

    constructor Create;
  end;

implementation

function TRental.getId(): LargeInt;
begin
  Result := self.id;
end;

function TRental.setId(newId: LargeInt): boolean;
begin
  Result := False;
  if (newId <> NULL) then
  begin
    self.id := newId;
    Result := True;
  end;
end;

function TRental.getBookId(): LargeInt;
begin
  Result := self.book_id;
end;

function TRental.setBookId(newBookId: LargeInt): boolean;
begin
  Result := False;
  if (newBookId <> NULL) then
  begin
    self.book_id := newBookId;
    Result := True;
  end;
end;

function TRental.getStudentId(): LargeInt;
begin
  Result := self.student_id;
end;

function TRental.setStudentId(newStudentId: LargeInt): boolean;
begin
  Result := False;
  if (newStudentId <> NULL) then
  begin
    self.student_id := newStudentId;
    Result := True;
  end;
end;

function TRental.getReturnDate(): TDate;
begin
  Result := self.return_date;
end;

function TRental.setReturnDate(newReturnDate: TDate): boolean;
begin
  Result := False;
  if (newReturnDate <> NuLL) and (newReturnDate >= 0) then
  begin
    self.return_date := newReturnDate;
    Result := True;
  end;
end;

function TRental.getRentalDate(): TDate;
begin
  Result := self.rental_date;
end;

function TRental.setRentalDate(newRentalDate: TDate): boolean;
begin
  Result := False;
  if (newRentalDate <> NULL) and (newRentalDate >= 0) then
  begin
    self.rental_date := newRentalDate;
    Result := True;
  end;
end;

constructor TRental.Create;
begin
  self.id := -1;
  self.book_id := -1;
  self.student_id := -1;
  self.return_date := -1;
  self.rental_date := -1;
end;

end.

