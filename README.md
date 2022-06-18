# RJSON
JSON to object and object to JSON converter. JSON library for Delphi (Delphi 6 or above) and Lazarus.

## Why you should use this library
RJSON was developed to simplify the JSON serialization and parsing process. What's special about this library is that the RJSON parsing process converts JSON into objects with the same data structure as the original object.

For example serialized JSON as follows:

```js
{
  "MemberId":"2022010111",
  "UserId":"abdul",
  "RealName":"Abdul Latief",
  "GenderId":0,
  "GenderName":"Male",
  "Downlines":[
    {
      "MemberId":"2022010112",
      "UserId":"setiab",
      "RealName":"Setia Budi",
      "GenderId":0,
      "GenderName":"Male",
      "Downlines":null
    },
    {
      "MemberId":"2022010113",
      "UserId":"julia",
      "RealName":"Julia Rahman",
      "GenderId":1,
      "GenderName":"Female",
      "Downlines":null
    },
  ]
}
```
The JSON will be converted into an object with the following data structure.

```delphi
TMember = class(TCollectionItem)
private
  fMemberId: string;
  fUserId: string;
  fRealName: string;
  fGenderId: Byte;
  fGenderName: string;
  fDownlines: TCollection;
published
  property MemberId: string read fMemberId write fMemberId;
  property UserId: string read fUserId write fUserId;
  property RealName: string read fReadName write fRealName;
  property GenderId: Byte read fGenderId write fGenderId;
  property GenderName: string read fGenderName write fGenderName;
  property Downlines: TCollection read fDownlines write fDownlines;
end;
```
