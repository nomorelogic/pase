unit upasetypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TPaseArrayBound }
  TPaseArrayBound = record
    Assigned: boolean;
    LowIndex: integer;
    HighIndex: integer;
  end;

  TPaseArrayBounds = array of TPaseArrayBound;

  { TPaseBaseType}
  TPaseBaseType = (
       // unsized
       pftUnknown,
       pftBoolean, pftDate, pftTime, pftDateTime,pftBlob,
       pftMemo, pftGraphic, pftTimeStamp,  pftGuid, // pftAutoInc,
       pftString,
       // sized field
       pftIntegerSmall, pftInteger, pftSizedString, pftFixedString,
       // sized + decimal
       pftFloat,
       // array dimension + bounds
       pftStaticArray
  );

  TPaseFieldTypes = set of TPaseBaseType;

  TPaseFieldAttribute = (pfaConstructor, pfaGetter, pfaSetter, pfaRequired);
  TPaseFieldAttributes = set of TPaseFieldAttribute;
  TPaseFieldScope = (pfsPrivate, psfPublic, pfsPublished);


  TPaseHistoryItemType = (phiAnalisys, phiModel, phiEntity, phiField, phiDataType,
                          phiIndex, phiCheck, phiFk);
  TPaseHistoryOperation = (phoNew, phoEdit, phoDelete);

  { TPaseHistoryItem }
  TPaseHistoryItem = record
    DateTime: TDateTime;
    ItemType: TPaseHistoryItemType;
    Operation: TPaseHistoryOperation;
    ClassName: string;
    ItemName: string;
    ItemData: string;
  end;

  TPaseHistoryItemArray = array of TPaseHistoryItem;

  { TDefautValue }
  // switched to variant
  TDefautValue = record
    IsNull: boolean;
    case iType:TPaseBaseType of
         pftInteger:     (AsInteger: integer);
         pftFloat:       (AsFloat: extended);
         pftSizedString: (AsString: string[100]);
  end;


  TPaseException = class(exception);
  TPaseErr_FeatureNotSupported = class(TPaseException);
  TPaseErr_PaseEntity_NotFound = class(TPaseException);
  TPaseErr_PaseField_NotFound = class(TPaseException);
  TPaseErr_PaseDataType_NotFound = class(TPaseException);


const
  PASESET_UnsizedFields: TPaseFieldTypes = [
       pftUnknown,
       pftBoolean, pftDate, pftTime, pftDateTime,pftBlob,
       pftMemo, pftGraphic, pftTimeStamp,  pftGuid, // pftAutoInc,
       pftString,
       pftInteger // --> DB ha size!!!!
  ];

  PASESET_SizedFields: TPaseFieldTypes = [
       pftSizedString, pftFixedString
  ];

  PASESET_SizedWithDecimalFields: TPaseFieldTypes = [
       pftFloat
  ];

  PASESET_SizedCollectionFields: TPaseFieldTypes = [
       pftStaticArray
  ];



  function PaseFieldTypeAsString(AFieldType: TPaseBaseType): string; // deprecated
  function PaseHistoryItemTypeAsString(AHistoryItemType: TPaseHistoryItemType): string;
  function PaseHistoryOperationAsString(AHistoryOperation: TPaseHistoryOperation): string;

  function PaseGetArrayBound(AFrom, ATo: integer): TPaseArrayBound;

implementation

uses typinfo;

  function PaseFieldTypeAsString(AFieldType: TPaseBaseType): string;
  begin
    Result:=copy(GetEnumName(TypeInfo(TPaseBaseType), Ord(AFieldType)), 4, 128);
  end;

  function PaseHistoryItemTypeAsString(AHistoryItemType: TPaseHistoryItemType): string;
  begin
    Result:=copy(GetEnumName(TypeInfo(TPaseHistoryItemType), Ord(AHistoryItemType)), 4, 128);
  end;

  function PaseHistoryOperationAsString(AHistoryOperation: TPaseHistoryOperation): string;
  begin
    // Result:=copy(GetEnumName(TypeInfo(TPaseHistoryOperation), Ord(AHistoryOperation)), 4, 128);
    case AHistoryOperation of
       phoNew   : result:='+';
       phoEdit  : result:='e';
       phoDelete: result:='-';
    else
       result := '?';
    end;
  end;


  function PaseGetArrayBound(AFrom, ATo: integer): TPaseArrayBound;
  begin
    result.HighIndex:=AFrom;
    Result.LowIndex:=ATo;
  end;

end.

