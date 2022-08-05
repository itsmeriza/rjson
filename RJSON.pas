{ ***************************************************************************

   JSON to object and object to JSON converter. JSON library for Delphi
   (Delphi 6 or above) and Lazarus.

   Author: Riza Anshari
   Github: github.com/itsmeriza
   Twitter: @RizaAnshari
   Email: rizast@gmail.com
   License: MIT License

   Copyright (c) 2022 Riza Anshari

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE.

 *************************************************************************** }

unit RJSON;

interface

uses Classes, SysUtils, Dialogs, TypInfo, StrUtils, Variants, DateUtils;

const
  BRACKET_OPEN = '[';
  BRACKET_CLOSE = ']';
  BRACKET_CURLY_OPEN = '{';
  BRACKET_CURLY_CLOSE = '}';
  LIST_CLASS_NAME = 'TList';
  ESCAPE_CHARS = [#39, '"', '\', #10, #13, #9, #8, #12, #255];

type
  TRJSONOption = (joAssociate, joIndexed);
  TValueTypes = (vtPrimitive, vtArray, vtObject, vtNull);
  
  TValue = record
    name: string;
    value: string;
    vtype: TValueTypes;
  end;
  PValue = ^TValue;

  TIllegalChars = set of Char;
  TDelimiters = set of Char;
  TPersistentClasses = array of TPersistentClass;
  TRJSONHelper = class
  public
    class procedure Split(list: TStringList; s: string; delimiter: Char = ',');
    class procedure JSONSplit(list: TStringList; s: string; delimiter: Char = ',');
    class procedure ClearList(List: TList);
    class function Trim(s: string; illegalChars: TIllegalChars = [#32]): string;
    class function TrimLeft(s: string; illegalChars: TIllegalChars = [#32]): string;
    class function TrimRight(s: string; illegalChars: TIllegalChars = [#32]): string;
    class function Underscored(ACammelCaseString: string): string;
    class function Escape(s: string): string;
    class function Unescape(s: string): string;
    class function StrToBool(s: string): Boolean;
    class function ChangeDecimalSeparator(StringFloat: string; DecimalSeparator: Char): string;
    class function FloatToStr(Float: Extended; DecimalSeparator: Char = '.'): string;
    class function StrToFloat(StringFloat: string; DecimalSeparator: Char = '.'): Extended;
  end;

  TRJSON = class(TPersistent)
  private
    class function DoToJSON(AObject: TObject; AJSONOption: TRJSONOption): string;
    class function GetValue(Value: string): PValue;
    class procedure DoProcessObject(Instance: TObject; JSON: string);
    class procedure DoProcessArray(Instance: TObject; V: PValue);
  public
    class procedure ToObject(Instance: TObject; const JSON: string); 
    class function ToJSON(Instance: TObject; Option: TRJSONOption = joAssociate): string; overload; virtual;
    function ToJSON(AJSONOption: TRJSONOption = joAssociate): string; overload;
    class procedure Clone(Dest, Source: TObject); overload;
    procedure Clone(Dest: TObject); overload;
  end;

  TRJSONListHelper = class(TRJSON)
  private
    fList: TList;
  public
    constructor Create; 
    destructor Destroy; override;
  published
    property List: TList read fList write fList;
  end;

  TRJSONBooleanHelper = class(TRJSON)
  private
    fValue: Boolean;
  published
    property Value: Boolean read fValue write fValue;
  end;

implementation

{ TRJSON }

class function TRJSON.GetValue(Value: string): PValue;
var
  c: Char;
  p: Integer;
begin
  New(Result);

  p := Pos(':', Value);
  Result^.name := Copy(Value, 1, p-1);
  Result^.value := Copy(Value, p+1, Length(Value));
  c := PChar(Result^.value)[0];

  if LowerCase(Result^.value) = 'null' then
    Result^.vtype := vtNull
  else if c = BRACKET_OPEN then
    Result^.vtype := vtArray
  else if c = BRACKET_CURLY_OPEN then
    Result^.vtype := vtObject
  else
    Result^.vtype := vtPrimitive;
end;

class procedure TRJSON.DoProcessObject(Instance: TObject; JSON: string);
var
  tokens: TStringList;
  i: Integer;
  token, p: string;
  v: PValue;
  propInfo: PPropInfo;
  value: Variant;
  o: TObject;
begin
  tokens := TStringList.Create;
  try
    JSON := TRJSONHelper.Trim(JSON, [BRACKET_CURLY_OPEN, BRACKET_CURLY_CLOSE]);
    TRJSONHelper.JSONSplit(tokens, JSON);
    for i := 0 to tokens.Count - 1 do
    begin
      token := tokens[i];
      v := GetValue(token);
      try
        if v^.vtype = vtPrimitive then
        begin
          p := Copy(v^.name, 2, Length(v^.name)-2);
          try
            propInfo := GetPropInfo(Instance, p);
            if propInfo <> nil then
            begin
              value := TRJSONHelper.Unescape(TRJSONHelper.Trim(v^.value, ['"']));
              if propInfo^.PropType^.Name = 'Boolean' then
                value := TRJSONHelper.StrToBool(v^.value)
              else if propInfo^.PropType^.Name = 'TDateTime' then
                value := UnixToDateTime(StrToIntDef(value, 0))
              else if ((propInfo^.PropType^.Name = 'String') or (propInfo^.PropType^.Name = 'AnsiString')) and (value = 'null') then
                  value := '';

              SetPropValue(Instance, p, value);
            end;
          except
          end;
        end
        else if v^.vtype = vtObject then
        begin
          v^.name := TRJSONHelper.Trim(v^.name, ['"']);
          o := GetObjectProp(Instance, v^.name);
          if o <> nil then
            DoProcessObject(o, v^.value);
        end
        else if v^.vtype = vtArray then
          DoProcessArray(Instance, v);
      finally
        Dispose(v);
      end;
    end;
  finally
    tokens.Free;
  end;
end;

class procedure TRJSON.ToObject(Instance: TObject; const JSON: string);
begin
  DoProcessObject(Instance, JSON);
end;

class procedure TRJSON.DoProcessArray(Instance: TObject; V: PValue);
var
  i: Integer;
  item: TCollectionItem;
  objList: TCollection;
  tokens: TStringList;
  token, s, name: string;
begin
  name := TRJSONHelper.Trim(V^.name, ['"']);
  objList := TCollection(GetObjectProp(Instance, name));
  tokens := TStringList.Create;
  try
    s := Copy(V^.value, 2, Length(V^.value)-2);
    TRJSONHelper.JSONSplit(tokens, s);

    for i := 0 to tokens.Count - 1 do
    begin
      token := tokens[i];
      item := objList.Add;

      DoProcessObject(item, token);
    end;
  finally
    tokens.Free;
  end;
end;

function TRJSON.ToJSON(AJSONOption: TRJSONOption): string;
begin
  Result := DoToJSON(Self, AJSONOption);
  Result := Copy(Result, 1, Length(Result) - 1);
end;

class function TRJSON.DoToJSON(AObject: TObject;
  AJSONOption: TRJSONOption): string;
var
  count: Integer;
  list: PPropList;
  propInfo: PPropInfo;
  objList: TList;
  objCollection: TCollection;
  o: TObject;
  i: Integer;
  ii: Integer;
  el, value: string;
begin
  if AJSONOption = joIndexed then
    Result := BRACKET_OPEN
  else
    Result := BRACKET_CURLY_OPEN;

  if AObject = nil then
    Exit;

  count := GetPropList(AObject, list);
  try
    for i := 0 to count - 1 do
    begin
      propInfo := list^[i];
      if (propInfo^.PropType^.Name = 'TCollection') then
      begin
        objCollection:= TCollection(GetObjectProp(AObject, propInfo^.Name));
        if objCollection.Count > 0 then
        begin
          el := BRACKET_OPEN;
          if AJSONOption = joAssociate then
            el := Format('"%s":%s', [propInfo^.Name, BRACKET_OPEN]);
        end
        else begin
          if AJSONOption = joAssociate then
            el := Format('"%s":%s', [propInfo^.Name, '[]']);
        end;

        Result := Result + el;

        for ii := 0 to objCollection.Count - 1 do
        begin
          o := TObject(objCollection.Items[ii]);
          Result := Result + DoToJSON(o, AJSONOption);
        end;

        if objCollection.Count > 0 then
          Result := Copy(Result, 0, Length(Result) - 1) + BRACKET_CLOSE + ','
        else
          Result := Copy(Result, 0, Length(Result)) + ','; 
      end
      else if propInfo^.PropType^.Name = LIST_CLASS_NAME then
      begin
        objList := GetObjectProp(AObject, propInfo^.Name) as TList;
        if objList.Count > 0 then
        begin
          el := BRACKET_OPEN;
          if AJSONOption = joAssociate then
            el := Format('"%s":%s', [propInfo^.Name, BRACKET_OPEN]);

          Result := Result + el;
        end;

        for ii := 0 to objList.Count - 1 do
        begin
          o := TObject(objList[ii]);
          Result := Result + DoToJSON(o, AJSONOption);
        end;

        if objList.Count > 0 then
          Result := Copy(Result, 0, Length(Result) - 1) + BRACKET_CLOSE + ',';
      end
      else if (propInfo^.PropType^.Kind = tkClass) then
      begin
        o := GetObjectProp(AObject, propInfo^.Name);
        el := '';
        if AJSONOption = joAssociate then
          el := Format('"%s":', [propInfo^.Name]);
        if o <> nil then
          Result := Result + el + DoToJSON(o, AJSONOption)
        else
          Result := Result + el + 'null,';
      end
      else begin
        value := VarToStr(GetPropValue(AObject, propInfo^.Name));
        if value = '' then
          value := 'null'
        else if propInfo^.PropType^.Name = 'Double' then
          value := TRJSONHelper.ChangeDecimalSeparator(value, '.')
        else if propInfo^.PropType^.Name = 'TDateTime' then
          value := IntToStr(DateTimeToUnix(VarToDateTime(GetPropValue(AObject, propInfo^.Name))))
        else if propInfo^.PropType^.Name = 'Boolean' then
          value := LowerCase(value)
        else if ((propInfo^.PropType^.Kind = tkString) or (propInfo^.PropType^.Kind = tkChar) or
          (propInfo^.PropType^.Kind = tkWChar) or (propInfo^.PropType^.Kind = tkLString) or
          (propInfo^.PropType^.Kind = tkWString)) and (value <> 'null') then

            value := '"' + TRJSONHelper.Escape(value) + '"';

        if AJSONOption = joAssociate then
        begin
          el := '"%s":%s';
          el := Format(el, [propInfo^.Name, value]);
        end
        else
          el := value;

        Result := Result + el + ',';
      end
    end;
  finally
    Dispose(list);
  end;

  if AJSONOption = joIndexed then
    Result := Copy(Result, 0, Length(Result) - 1) + BRACKET_CLOSE + ','
  else
    Result := Copy(Result, 0, Length(Result) - 1) + BRACKET_CURLY_CLOSE + ',';
end;

class function TRJSON.ToJSON(Instance: TObject;
  Option: TRJSONOption): string;
begin
  Result := DoToJSON(Instance, Option);
  Result := Copy(Result, 1, Length(Result) - 1);
end;

class procedure TRJSON.Clone(Dest, Source: TObject);
var
  count: Integer;
  list: PPropList;
  i: Integer;
  pi: PPropInfo;
  v: Variant;
begin
  count := GetPropList(Source, list);
  for i := 0 to count - 1 do
  begin
    pi := list^[i];
    v := GetPropValue(Source, pi^.Name);
    SetPropValue(Dest, pi^.Name, v);
  end;
end;

procedure TRJSON.Clone(Dest: TObject);
begin
  TRJSON.Clone(Dest, Self);
end;

{ TRJSONHelper }

class procedure TRJSONHelper.Split(list: TStringList; s: string; delimiter: Char = ',');
var
  i, len, p, pClosed: Integer;
  closed, found: Boolean;
  c: Char;
  left, right: string;
begin
  right := s;
  len := Length(right);
  if len = 0 then
    Exit;

  if PChar(right)[len-1] <> delimiter then
    right := right + delimiter;
    
  closed := True;
  found := False;
  pClosed := 0;
  len := Length(right);
  for i := 0 to len - 1 do
  begin
    c := PChar(right)[i];
    if c = '"' then
    begin
      closed := not closed;
      if closed then
        pClosed := i+1
    end;

    if (c = delimiter) and closed then
    begin
      if not found then
        p := Pos(delimiter, right)
      else begin
        p := pClosed+1;
      end;

      left := Trim(Copy(right, 1, p-1));
      list.Add(left);

      right := Copy(right, p+1, len);
      Split(list, right, delimiter);

      Break;
    end
    else if (c = delimiter) and not closed then
      found := True;
  end;
end;

class procedure TRJSONHelper.JSONSplit(list: TStringList; s: string;
  delimiter: Char = ',');
var
  i, len, p, count: Integer;
  closedQuote: Boolean;
  c, cc: Char;
  left, right: string;
begin
  right := s;
  len := Length(right);
  if len = 0 then
    Exit;
    
  if PChar(right)[len-1] <> delimiter then
    right := right + delimiter;

  closedQuote := True;
  len := Length(right);
  count := 0;
  cc := #0;
  for i := 0 to len - 1 do
  begin
    c := PChar(right)[i];
    if i - 1 >= 0 then
      cc:= PChar(right)[i-1];

    if (c = '"') and (cc <> '\') then
      closedQuote := not closedQuote
    else if (c in [BRACKET_CURLY_OPEN, BRACKET_OPEN]) and closedQuote then
      Inc(count)
    else if (c in [BRACKET_CURLY_CLOSE, BRACKET_CLOSE]) and closedQuote then
      Dec(count);

    if (c = delimiter) and (count = 0) and closedQuote then
    begin
      p := i + 1;

      left := TRJSONHelper.Trim(Copy(right, 1, p-1));
      list.Add(left);

      right := Copy(right, p+1, len);
      JSONSplit(list, right, delimiter);

      Break;
    end;
  end;
end;

class function TRJSONHelper.Trim(s: string;
  illegalChars: TIllegalChars): string;
var
  left: string;
begin
  left := TRJSONHelper.TrimLeft(s, illegalChars);
  Result := TRJSONHelper.TrimRight(left, illegalChars);
end;

class function TRJSONHelper.TrimLeft(s: string;
  illegalChars: TIllegalChars): string;
var
  i, l: Integer;
  c, cc: Char;
  isIllegal: Boolean;
begin
  l := Length(s);
  cc := #0;
  for i := 0 to l - 1 do
  begin
    c := PChar(s)[i];
    if (i - 1) >= 0 then
      cc := PChar(s)[i-1];
    isIllegal := (c in illegalChars) and (cc <> '\');
    if not isIllegal then
      Break;
  end;

  Result := Copy(s, i+1, l);
end;

class function TRJSONHelper.TrimRight(s: string;
  illegalChars: TIllegalChars): string;
var
  i, l: Integer;
  c, cc: Char;
  isIllegal: Boolean;
begin
  Result := s;
  l := Length(s);
  cc := #0;
  for i := l - 1 downto 0 do
  begin
    c := PChar(s)[i];
    if (i - 1) >= 0 then
      cc := PChar(s)[i-1];
    isIllegal := (c in illegalChars) and (cc <> '\');
    if not isIllegal then
      Break;
  end;

  Result := Copy(s, 1, i+1);
end;

class function TRJSONHelper.Underscored(ACammelCaseString: string): string;
var
  lowerStr: string;
  buffer_1, buffer_2: PChar;
  i, len: Integer;
  isPrevUnderscored: Boolean;
begin
  lowerStr := LowerCase(ACammelCaseString);
  len := Length(ACammelCaseString);

  GetMem(buffer_1, len + 1);
  GetMem(buffer_2, len + 1);

  try
    StrPCopy(buffer_1, ACammelCaseString);
    StrPCopy(buffer_2, lowerStr);

    isPrevUnderscored := False;
    Result := '';
    for i := 0 to Length(ACammelCaseString) - 1 do
    begin
      if (buffer_1[i] <>  buffer_2[i]) and not isPrevUnderscored then
      begin
        Result := Result + '_' + buffer_2[i];
        isPrevUnderscored := True;
      end
      else begin
        Result := Result + buffer_2[i];
        isPrevUnderscored := False;
      end;
    end;

    Result := Copy(Result, 2, Length(Result));
  finally
    FreeMem(buffer_1);
    FreeMem(buffer_2);
  end;
end;

class function TRJSONHelper.Escape(s: string): string;
var
  len, i: Integer;
  c: Char;
  ps: PChar;
  escapeChar: string;
begin
  Result := '';
  len := Length(s);
  ps := PChar(s);
  for i := 0 to len-1 do
  begin
    c := ps[i];
    escapeChar := c;
    if c in ESCAPE_CHARS then
    begin
      if c = #10 then
        escapeChar := 'n'
      else if c = #13 then
        escapeChar := 'r'
      else if c = #9 then
        escapeChar := 't'
      else if c = #8 then
        escapeChar := 'b'
      else if c = #12 then
        escapeChar := 'f'
      else if c = #255 then
        escapeChar := 'xFF'
      else
        escapeChar := c;

      escapeChar := '\' + escapeChar;
    end;

    Result := Result + escapeChar;
  end;
end;

class function TRJSONHelper.Unescape(s: string): string;

  function getEscapeChar(var len: Integer; i: Integer; s: PChar): Char;
  var
    l: Integer;
  begin
    Result := #0;
    l := 0;
    if i <= Length(string(s)) - 1 then
    begin
      l := 1;
      if s[i+1] = 'x' then
      begin
        l := 3;
        Result := #255
      end
      else if s[i+1] = 'n' then
        Result := #10
      else if s[i+1] = 'r' then
        Result := #13
      else if s[i+1] = 't' then
        Result := #9
      else if s[i+1] = 'b' then
        Result := #8
      else if s[i+1] = 'f' then
        Result := #12
      else if s[i+1] = '\' then
        Result := getEscapeChar(len, i+1, s)
      else
        Result := s[i+1]
    end;
    len := len + l;
  end;

var
  i, len: Integer;
  c, escapeChar: Char;
  ps: PChar;
begin
  Result := '';
  i := 0;
  s := s + #3;
  c := #0;
  ps := PChar(s);
  while c <> #3 do
  begin
    c := ps[i];
    if c = '\' then
    begin
      len := 0;
      escapeChar := getEscapeChar(len, i, ps);
      Result := Result + escapeChar;
      Inc(i, len+1)
    end
    else begin
      Result := Result + c;
      Inc(i);
    end;
  end;
  Result := Copy(Result, 1, Length(Result)-1);
end;

class procedure TRJSONHelper.ClearList(List: TList);
var
  i: Integer;
  o: TObject;
begin
  for i := 0 to List.Count - 1 do
  begin
    o := TObject(List[i]);
    o.Free;
  end;
end;

class function TRJSONHelper.StrToBool(s: string): Boolean;
var
  value: Integer;
begin
  if TryStrToInt(s, value) then
  begin
    Result := Boolean(value);
    Exit;
  end;
  Result := LowerCase(s) = 'true';
end;

class function TRJSONHelper.FloatToStr(Float: Extended; DecimalSeparator: Char = '.'): string;
begin
  Result := SysUtils.FloatToStr(Float);
  Result := TRJSONHelper.ChangeDecimalSeparator(Result, DecimalSeparator);
end;

class function TRJSONHelper.ChangeDecimalSeparator(StringFloat: string;
  DecimalSeparator: Char): string;
var
  buffer: PChar;
  isNull: Boolean;
  i: Integer;
  sBuffer: string;
begin
  Result := '';
  sBuffer := Copy(StringFloat, 0, Length(StringFloat));
  buffer := PChar(sBuffer);
  i := 0;
  isNull := buffer[i] = #0;

  while not isNull do
  begin
    if (buffer[i] = ',') or (buffer[i] = '.') then
      buffer[i] := DecimalSeparator;

    Result := Result + buffer[i];
    Inc(i);
    isNull := buffer[i] = #0;
  end;
end;

class function TRJSONHelper.StrToFloat(StringFloat: string;
  DecimalSeparator: Char): Extended;
begin
  StringFloat := TRJSONHelper.ChangeDecimalSeparator(StringFloat, DecimalSeparator);
  Result := SysUtils.StrToFloat(StringFloat);
end;

{ TRJSONListHelper }

constructor TRJSONListHelper.Create;
begin
  inherited;

  fList := TList.Create;
end;

destructor TRJSONListHelper.Destroy;
begin
  TRJSONHelper.ClearList(fList);
  fList.Free;
  inherited;
end;

end.
