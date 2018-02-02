(*******************************************************************************
NumberPack.pas
--------------------------------------------------------------------------------
Universal compression routine for numbers

The core routines of NumberPack are "PackIntegers" and "UnpackIntegers".
They work best with data that has some natural source like measuring samples.

The surounding object "TNumberPack" handles integer and floating point numbers
by simply defininig a decimal place that goes with the integer number.

--------------------------------------------------------------------------------

2918-02-02 - 1.30
  -Change of string types to work with newer Delphi versions

2017-12-17 - 1.20
  -Preperation for an escape code. Nothing will happen yet

2017-08-17 - 1.10
  -Cleaned up code
  -No more dependencies but 'math'
  -32 and 64 bit support

2012-01-01 - 1.00
  -Initial version. For 32 bit integers only

*******************************************************************************)

unit NumberPack;

interface

const
  MAX_DIGIT = 9;

type
  //RawByteString = AnsiString; //Combatibilty for older Delphi versions

{$IFDEF LOW_RESOLUTION}
  SINT = Longint; //32 Bit Signed Integer
  UINT = Cardinal; //32 Bit Unsinged Integer
  FLOAT = double; //64 Bit precision Float
{$ELSE}
  SINT = Int64; //64 Bit Signed Integer
  UINT = UInt64; //64 Bit Unsinged Integer
  FLOAT = extended; //80 Bit precision Float
{$ENDIF}

  Integers = array of SINT;
  Numbers = array of FLOAT;
  ShortStr = string[31];
  DataStr = RawByteString;


  TNumberPack = class(TObject)
  private
    fAutomatic: boolean;
    fDigits: integer;
    fFactor: Double;
    fValues: Integers;
    fFloats: Numbers;
    procedure FindDigits(S: ShortStr);
    function Append(Num: FLOAT): integer;
    function GetCount: integer;
    procedure SetDigits(Digits: integer);
    function GetData: DataStr;
    procedure SetData(S: DataStr);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;

    procedure Add(Value: FLOAT); overload;
    procedure Add(Value: ShortStr); overload;
    procedure AddArray(Values: Integers); overload;
    procedure AddArray(Values: Numbers); overload;

    function GetInt(Index: integer): SINT;
    function GetNum(Index: integer): FLOAT;
    function GetIntStr(Index: integer): ShortStr;
    function GetNumStr(Index: integer): ShortStr;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: SINT read GetInt; default;
    property Digits: integer read fDigits write SetDigits;
    property Factor: Double read fFactor;

    property Data: DataStr read GetData write SetData;
  end;


procedure PackIntegers(Values: Integers; var Output: DataStr);
procedure UnpackIntegers(Input: DataStr; var Values: Integers);


implementation

uses
  { Delphi }
  Math;


procedure PackIntegers(Values: Integers; var Output: DataStr);
var
  Index: integer; //Actual index into the value array
  Delta: SINT; //Difference between actual value and its predecessor
  Previous: SINT; //Previous value
  Block: UINT; //Absolute value of the actual delta value
  Count: word; //Number of repeats (up to 63)
  Code: byte; //actual coded octet
begin
  Output := '';
  Index := 0;
  Previous := 0;

  repeat
    Count := 0;
    Delta := Values[Index] - Previous; // Process the difference of this and previous value
    Block := abs(Delta); // absolute Value of Delta
    Previous := Values[Index];

    while (Count < 63) and (Index < High(Values)) and ((Values[Index + 1] - Previous) = Delta) do
    begin // Count up to 63 repeating deltas
      inc(Index);
      inc(Count);
      Previous := Values[Index];
    end;

    if (Count = 0) and (Block < 64) then Code := Block //The EASY way: Just one byte
    else Code := Count or $80; //Set Bit 7 because one more Bytes is following

    if (Delta < 0) then Code := Code or $40; // Set Bit 6 for minus sign
    Output := Output + AnsiChar(Code);

    while (Code > $7F) do // At least one more Byte to go
    begin
      Code := Block mod 128; // devide the remaining block into 7-Bit chunks
      Block := Block div 128;
      if (Block > 0) then Code := Code or $80; //Set Bit 7 because one more Byte is following
      Output := Output + AnsiChar(Code);
    end;

    inc(Index);
  until (Index > High(Values));
end;


procedure UnpackIntegers(Input: DataStr; var Values: Integers);
var
  i, j: integer; //Loop variables
  Code: byte; //Actual octet
  Sign: boolean; //Sign of the actual value
  Count: word; //Number of repeats (up to 63)
  Value: SINT; //Actual value (including sign)
  Block: UINT; //Actual value (absolute)
  Stack: ShortStr; //Helper stack for collecting the 7-bit chunks of the block value
  Previous: SINT; //Previous value
begin
  SetLength(Values, 0);
  i := 0;
  Previous := 0;
  while (i < length(Input)) do
  begin
    inc(i);
    Code := ord(Input[i]);

    if (Code = $40) then
    begin
      // ESCAPE Procedure
      Stack := '';
      repeat
        if (i < length(Input)) then
        begin
          inc(i);
          Code := ord(Input[i]);
          Stack := Stack + AnsiChar(Code and $7F);
        end;
      until (Code < $80);
    end
    else begin
      // Regular Procedure
      Sign := (Code and $40) = $40;

      if (Code > $7F) and (i < length(Input)) then // More than one Byte is needed for coding
      begin
        Count := Code and $3F;
        Stack := '';
        repeat
          inc(i);
          Code := ord(Input[i]);
          Stack := AnsiChar(Code and $7F) + Stack;
        until (Code < $80) or (i >= length(Input)); // Repeat until no more bytes left for coding

        Block := 0;
        for j := 1 to length(Stack) do Block := (Block * $80) + ord(Stack[j]);
      end
      else begin // EASY! Only one octet with 6 Bit value (+/- 63)
        Count := 0;
        Block := Code and $3F;
      end;

      Value := Block;
      if Sign then Value := -Value;

      for j := 0 to Count do //Output repeating values
      begin
        Previous := Previous + Value;
        SetLength(Values, length(Values) + 1);
        Values[High(Values)] := Previous;
      end;
    end;
  end;
end;


{==============================================================================}


constructor TNumberPack.Create;
begin
  inherited Create;
  Clear;
end;


destructor TNumberPack.Destroy;
begin
  Clear;
  inherited Destroy;
end;


procedure TNumberPack.FindDigits(S: ShortStr);
var
  D, P: integer;
begin
  D := 0;
  while (length(S) > 0) and (S[length(S)] = '0') do
  begin
    delete(S, length(S), 1);
    dec(D);
  end;

  P := pos(AnsiChar('.'), S);
  if (P <> 0) then
  begin
    delete(S, 1, P);
    D := length(S);
  end;

  if (D > fDigits) then fDigits := D;
end;


function TNumberPack.Append(Num: FLOAT): integer;
begin
  SetLength(fFloats, length(fFloats) + 1);
  SetLength(fValues, length(fFloats));
  fFloats[High(fFloats)] := Num;
  Result := High(fFloats);
end;


function TNumberPack.GetCount: Integer;
begin
  Result := length(fValues);
end;


function TNumberPack.GetData: DataStr;
var
  i: integer;
  D: Shortint; //Signed Byte
begin
  fFactor := IntPower(10, fDigits);
  for i := Low(fFloats) to High(fFloats) do
  begin
    try
      fValues[i] := round(fFloats[i] * fFactor);
    except
      // Something went terribly wrong here!
      // Happens only if Digits is out of range.
      // RaiseLastOsError;
      fValues[i] := 0;
    end;
  end;
  PackIntegers(fValues, Result);
  D := fDigits;
  Result := AnsiString('X1') + AnsiChar(D) + Result;
end;


procedure TNumberPack.SetData(S: DataStr);
var D: Shortint; //Signed Byte
begin
  SetLength(fValues, 0);
  if (length(S) > 3) then
  begin
    if (S[1] = 'X') and (S[2] = '1') then
    begin
      move(S[3], D, 1);
      SetDigits(D);
      fFactor := IntPower(10, fDigits);
      delete(S, 1, 3);
      UnpackIntegers(S, fValues);
    end;
  end;
end;


procedure TNumberPack.SetDigits(Digits: integer);
begin
  fDigits := Digits;
  fAutomatic := false;
end;


procedure TNumberPack.Clear;
begin
  fAutomatic := true;
  fDigits := -MAX_DIGIT;
  fFactor := 0;
  SetLength(fFloats, 0);
  SetLength(fValues, 0);
end;


procedure TNumberPack.Add(Value: FLOAT);
var S: ShortString;
begin
  if fAutomatic and (fDigits < MAX_DIGIT) then
  begin
    str(Value: 1: MAX_DIGIT, S);
    FindDigits(S);
  end;

  Append(Value);
end;


procedure TNumberPack.Add(Value: ShortStr);
var
  Num: FLOAT;
  Code: integer;
begin
  if (Value = '') then Value := '0';
  val(String(Value), Num, Code);
  if (Code = 0) then
  begin
    if fAutomatic and (fDigits < MAX_DIGIT) then FindDigits(Value);
    Append(Num);
  end;
end;


procedure TNumberPack.AddArray(Values: Integers);
var i: integer;
begin
  for i := low(Values) to high(Values) do Add(Values[i]);
end;


procedure TNumberPack.AddArray(Values: Numbers);
var i: integer;
begin
  for i := low(Values) to high(Values) do Add(Values[i]);
end;


function TNumberPack.GetInt(Index: Integer): SINT;
begin
  if (Index < 0) or (Index > High(fValues)) then Result := 0
  else Result := fValues[Index];
end;


function TNumberPack.GetNum(Index: integer): FLOAT;
begin
  try
    Result := GetInt(Index) / fFactor;
  except
    // Something went terribly wrong here!
    // Happens only if Digits is out of range/not set.
    // RaiseLastOsError;
    Result := 0.0;
  end;
end;


function TNumberPack.GetIntStr(Index: integer): ShortStr;
begin
  str(GetInt(Index): 1, Result);
end;


function TNumberPack.GetNumStr(Index: integer): ShortStr;
begin
  str(GetNum(Index): 1: fDigits, Result);
end;


end.

