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

begin
  member := TMember.Create(nil);
  member.MemberId := 202200112;
  member.RealName := 'A''hmad Zarkowi';
  member.PlaceOfBirth := 'Surabaya';
  member.DateOfBirth := EncodeDate(1990, 7, 12);

  upline := TMember.Create(nil);
  upline.MemberId := 202200111;
  upline.RealName := 'Syueb Hamdan';
  upline.PlaceOfBirth := 'Jakarta';
  upline.DateOfBirth := EncodeDate(1990, 2, 1);

  member.Upline := upline;

  downline := member.Downlines.Add as TMember;
  downline.MemberId := 202200113;
  downline.RealName := 'Lia Hamzah';
  downline.PlaceOfBirth := 'Jakarta';
  downline.DateOfBirth := EncodeDate(1990, 3, 18);

  downline := member.Downlines.Add as TMember;
  downline.MemberId := 202200114;
  downline.RealName := 'Nur Azizah Putri';
  downline.PlaceOfBirth := 'Serpong';
  downline.DateOfBirth := EncodeDate(1991, 3, 1);

  json := TRJSON.ToJSON(member);
  WriteLn(json);
  ReadLn;
end.
