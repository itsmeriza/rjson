program Project1;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Classes,
  RJSON in '..\..\RJSON.pas';

type
  // Serialized nested object
  TMember = class(TCollectionItem)
  private
    fUserId: Integer;
    fRealName: string;
    fPlaceOfBirth: string;
    fDownlines: TCollection;
    fDateOfBirth: TDateTime;
    fUpline: TMember;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property MemberId: Integer read fUserId write fUserId;
    property RealName: string read fRealName write fRealName;
    property PlaceOfBirth: string read fPlaceOfBirth write fPlaceOfBirth;
    property DateOfBirth: TDateTime read fDateOfBirth write fDateOfBirth;
    property Upline: TMember read fUpline write fUpline;
    property Downlines: TCollection read fDownlines write fDownlines;
  end;

{ TMember }

constructor TMember.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  fDownlines := TCollection.Create(TMember);
end;

destructor TMember.Destroy;
begin
  fDownlines.Clear;
  fDownlines.Free;
  inherited;
end;

var
  upline, member, downline: TMember;
  json: string;
  f: TextFile;

begin
  // read json data from external file
  AssignFile(f, 'data.json');
  Reset(f);

  ReadLn(f, json);
  CloseFile(f);

  member := TMember.Create(nil);
  member.Upline := TMember.Create(nil);
  TRJSON.ToObject(member, json);

  WriteLn('member.MemberId: ', member.MemberId);
  WriteLn('member.RealName: ', member.RealName);
  WriteLn('member.PlaceOfBirth: ', member.PlaceOfBirth);
  WriteLn('member.DateOfBirth: ', FormatDateTime('yyyy-mm-dd', member.DateOfBirth));
  WriteLn;
  WriteLn('member.Upline.MemberId: ', member.Upline.MemberId);
  WriteLn('member.Upline.RealName: ', member.Upline.RealName);
  WriteLn('member.Upline.PlaceofBirth: ', member.Upline.PlaceofBirth);
  WriteLn('member.Upline.DateOfBirth: ', FormatDateTime('yyyy-mm-dd', member.Upline.DateOfBirth));
  WriteLn('member.Upline.DownlinesCount: ', member.Upline.Downlines.Count);
  WriteLn;

  WriteLn('member.Downlines[0].MemberId: ', (member.Downlines.Items[0] as TMember).MemberId);
  WriteLn('member.Downlines[0].RealName: ', (member.Downlines.Items[0] as TMember).RealName);
  WriteLn('member.Downlines[0].PlaceOfBirth: ', (member.Downlines.Items[0] as TMember).PlaceOfBirth);
  WriteLn('member.Downlines[0].DateOfBirth: ', FormatDateTime('yyyy-mm-dd', (member.Downlines.Items[0] as TMember).DateOfBirth));
  if (member.Downlines.Items[0] as TMember).Upline = nil then
    WriteLn('member.Downlines[0].Upline: null');
    
  WriteLn;

  WriteLn('member.Downlines[1].MemberId: ', (member.Downlines.Items[1] as TMember).MemberId);
  WriteLn('member.Downlines[1].RealName: ', (member.Downlines.Items[1] as TMember).RealName);
  WriteLn('member.Downlines[1].PlaceOfBirth: ', (member.Downlines.Items[1] as TMember).PlaceOfBirth);
  WriteLn('member.Downlines[1].DateOfBirth: ', FormatDateTime('yyyy-mm-dd', (member.Downlines.Items[1] as TMember).DateOfBirth));
  if (member.Downlines.Items[1] as TMember).Upline = nil then
    WriteLn('member.Downlines[1].Upline: null');

  ReadLn;
end.
