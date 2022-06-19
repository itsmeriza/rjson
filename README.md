# RJSON
JSON to object and object to JSON converter. JSON library for Delphi (Delphi 6 or above) and Lazarus.

## Why you should use this library
RJSON was developed to simplify the JSON serialization and parsing process. What's special about this library is that the RJSON parsing process converts JSON into objects with the same data structure as the original object. This library also supports JSON operations for nested objects, both parsing and serialization.

For example serialized JSON as follows:

```js
{
	"MemberId": 202200112,
	"RealName": "Ahmad Zarkowi",
	"PlaceOfBirth": "Surabaya",
	"DateOfBirth": 647740800,
	"Upline": {
		"MemberId": 202200111,
		"RealName": "Syueb Hamdan",
		"PlaceOfBirth": "Jakarta",
		"DateOfBirth": 633830400,
		"Upline": null,
		"Downlines": []
	},
	"Downlines": [{
		"MemberId": 202200113,
		"RealName": "Lia Hamzah",
		"PlaceOfBirth": "Jakarta",
		"DateOfBirth": 637718400,
		"Upline": null,
		"Downlines": []
	}, {
		"MemberId": 202200114,
		"RealName": "Nur Azizah Putri",
		"PlaceOfBirth": "Serpong",
		"DateOfBirth": 667785600,
		"Upline": null,
		"Downlines": []
	}]
}
```
The JSON will be converted into an object with the following data structure.

```delphi
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
```
This library is very easy to use. Please see the sample projects. :)
