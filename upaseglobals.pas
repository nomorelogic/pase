unit upaseglobals;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  , fgl // used for generics
  , contnrs // TFPHashObjectList
  , upasetypes
  ;

type

   TPaseModel = class;
   TPaseEntity = class;

  { TPaseHistory }

  TPaseHistory = class(TPersistent)
  private
    FActive: boolean;
    FPaseHistoryItemArray: TPaseHistoryItemArray;
    function GetHistoryItem( index: integer ): TPaseHistoryItem;
    function GetText: string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddHistory(const AHistoryItemType: TPaseHistoryItemType;
                         const APaseHistoryOperation: TPaseHistoryOperation;
                         const AClass, AName, AData: string);

    property Item[ index: integer ]: TPaseHistoryItem read GetHistoryItem;
  published
    property Active: boolean read FActive write FActive;
    property Text: string read GetText;
  end;

  { TPaseDataType }

  TPaseDataType = class(TPersistent)

  private
    FArrayElementType: TPaseDataType;
    FCheckValue: string;
    FDefaultAsVariant: variant;
    // FDefaultValue: TDefautValue;
    FHasFixedSize: boolean; // space padded...
    FHistory: TPaseHistory;
    FInheritedType: string;
    FIsRequired: boolean;
    FName: string;
    FBaseType: TPaseBaseType;
    FSize: integer;
    FDecimal: integer;
    FArrayBounds: array of TPaseArrayBound;
    FCollate: string;
    function GetArrayBounds(Index: integer): TPaseArrayBound;
    function GetCollate: string;
    function GetDefaultAsString: string;
    function GetHasDecimal: boolean;
    function GetIsArray: boolean;
    function GetIsCollection: boolean;
    function GetIsInherited: boolean;
    function GetIsNumeric: boolean;
    function GetIsScalar: boolean;
    function GetIsSized: boolean;
  public
    // scalar
    // function SetDefaultAsFloat(AValue: extended): TPaseDataType;
    function SetCheckValue(AValue: string): TPaseDataType;
    // function SetDefaultValue(AValue: string): TPaseDataType;
    function SetDefaultValue(AValue: variant): TPaseDataType;

    // non scalar
    function SetArrayBoundsByDim(const DimIndex, LowIndex, HighIndex: integer;
                                 const IndexType: string = 'Integer'): TPaseDataType;

    function SetRequired(AValueRequired: boolean): TPaseDataType;

    // constructors
    constructor Create(const AName: String; const AFieldType: TPaseBaseType); overload;
    constructor Create(const AName: String; const AFieldType: TPaseBaseType;
                       const ASize: integer; const AHasFixedSize: boolean=false); overload;
    constructor Create(const AName: String; const AFieldType: TPaseBaseType;
                       const ASize, ADecimal: integer); overload;
    constructor CreateArray(const AName: String; const DimCount: integer;
                            const AElementType: TPaseDataType);

    function AsString: string;

    // properties
    property ArrayBounds[Index: integer]: TPaseArrayBound read GetArrayBounds;
    property History: TPaseHistory read FHistory write FHistory;
    // property DefaultValue: TDefautValue read FDefaultValue;
  published
    property Name: string read FName write FName;

    // scalar
    property IsScalar: boolean read GetIsScalar;
    property IsSized: boolean read GetIsSized;
    property IsNumeric: boolean read GetIsNumeric;
    property IsCollection: boolean read GetIsCollection;
    property IsInherited: boolean read GetIsInherited;

    property InheritedType: string read FInheritedType write FInheritedType;
    property HasDecimals: boolean read GetHasDecimal;
    property HasFixedSize: boolean read FHasFixedSize write FHasFixedSize;

    property BaseType: TPaseBaseType read FBaseType write FBaseType;
    property Size: integer read FSize write FSize;
    property Decimal: integer read FDecimal;
    property CheckValue: string read FCheckValue;
    // property DefaultAsString: string read GetDefaultAsString;
    property DefaultValue: variant read FDefaultAsVariant;

    property Collate: string read GetCollate;

    property IsRequired: boolean read FIsRequired;

    // non scalar
    property IsArray: boolean read GetIsArray;
    property ArrayElementType: TPaseDataType read FArrayElementType;
  end;

  { TPaseFieldDataTypeList }

  TPaseFieldDataTypeList = TFPHashObjectList;


  { TPaseFieldCheck }

  TPaseFieldCheck = class(TPersistent)
  private
    FName: string;
    FDestination: string;
    FExpression: string;
  public
     constructor Create(const AName, ADestination, AExpression: String);
  published
     property Name: string read FName write FName;
     property Destination: string read FDestination write FDestination;
     property Expression: string read FExpression write FExpression;
  end;

  { TPaseCheck }

  TPaseCheck = class(TPaseFieldCheck);

  { TPaseFieldCheckList }

  TPaseFieldCheckList = TFPHashObjectList;

  { TPaseEntityCheckList }

  TPaseEntityCheckList = TFPHashObjectList;

  { TPaseIndex }

  TPaseIndex = class(TPersistent)
  private
    FDestination: string;
    FExpression: string;
    FIsPrimaryKey: boolean;
    FName: string;
  public
     constructor Create(const AName: string; const IsPK: boolean=true; const ADestination: String = '');
     function AddIndexExpression(const AFileName: string; AAscending: boolean=true): TPaseIndex;
     function SetIndexFieldList(const AFieldNames: array of string): TPaseIndex;
     procedure Clear;
  published
     property Name: string read FName write FName;
     property Destination: string read FDestination write FDestination;
     property Expression: string read FExpression;
     property IsPrimaryKey: boolean read FIsPrimaryKey write FIsPrimaryKey;
  end;

  { TPaseEntityIndexList }

  TPaseEntityIndexList = TFPHashObjectList;

  { TPaseField }

  TPaseField = class(TPersistent)
  private
    FArrayElementType: string;
    FAttributes: TPaseFieldAttributes;
    FDecimal: integer;
    FDefaultValue: TDefautValue;
    FFieldDataType: TPaseDataType;
    FHistory: TPaseHistory;
    FIsRelationField: Boolean;
    FName: String;
    FCaption: String;
    FDescription: String;
    FIsPrimary: Boolean;
    FIsNotNull: Boolean;
    FPaseFieldCheckList: TPaseFieldCheckList;
    // FFieldType: TPaseBaseType;
    FRelationEntityField: string;
    FRelationEntityName: string;
    FRelationField: TPaseField;
    FScope: TPaseFieldScope;
    FSize: integer;
    FTextProperty: string;
    FArrayBounds: array of TPaseArrayBound;
    function GetArrayBounds(Index: integer): TPaseArrayBound;
    function GetArrayDimension: integer;
    function GetConstructorAllocateResources: string;
    function GetConstructorFieldInit: string;
    function GetDecimal: integer;
    function GetDestructorFreeResources: string;
    //function GetFieldType: TPaseBaseType;
    function GetIsArray: boolean;
    function GetPropertyReadPrefix: string;
    function GetPropertyWritePrefix: string;
    function GetSize: integer;
    function GetTextProperty: string;
    procedure SetAttributes(AValue: TPaseFieldAttributes);
    procedure SetIsNotNull(AValue: Boolean);
    procedure SetIsPrimary(AValue: Boolean);
    procedure SetTextProperty(AValue: string);

    procedure ResetDataType;
  public
    constructor Create(const AName: String);
    // to deprecate?
    constructor Create(const AName: String; const AFieldDataType: TPaseDataType;
                       const ASize: integer=-1; const ADecimal: integer=-1);

    destructor Destroy; override;

    procedure SetFieldType(const AFieldDataType: TPaseDataType; ASize: integer=-1; ADecimal: integer=-1); overload;
    procedure SetFieldType(const AFieldDataType: TPaseDataType; const ABounds: TPaseArrayBounds; const AElementType: TPaseDataType); overload;

    // defautl value
    procedure SetDefaultValueAsNull; overload;
    procedure SetDefaultValueAsString(const AValue: string); overload;
    procedure AddCheck(const ACheckName, ACheckDestination, ACheckExpression: string);



    property DefaultValue: TDefautValue read FDefaultValue;

    // links and relations
    procedure SetRelation(const ARelationEntityName, ARelationFieldName: string; const AField: TPaseField);

    function GetFieldTypeAsString: string;

    // returns dimension array bounds
    property ArrayBounds[Index: integer]: TPaseArrayBound read GetArrayBounds;
    property ArrayElementType: string read FArrayElementType;

    property History: TPaseHistory read FHistory write FHistory;

  published
    property Name: String read FName write FName;
    property Caption: String read FCaption write FCaption;
    property Description: String read FDescription write FDescription;

    property IsPrimary: Boolean read FIsPrimary write SetIsPrimary;
    property IsNotNull: Boolean read FIsNotNull write SetIsNotNull;

    property FieldDataType: TPaseDataType read FFieldDataType write FFieldDataType;
    // -> to refactor...
    // property FieldType: TPaseBaseType read GetFieldType write FFieldType;
    property FieldTypeAsString: string read GetFieldTypeAsString;
    // <- to refactor

    property Size: integer read GetSize write FSize;
    property Decimal: integer read GetDecimal write FDecimal;

    property IsRelationField: Boolean read FIsRelationField;
    property RelationEntityName: string read FRelationEntityName;
    property RelationEntityField: string read FRelationEntityField;

    property Scope: TPaseFieldScope read FScope write FScope;
    property Attributes: TPaseFieldAttributes read FAttributes write SetAttributes;
    property Checks: TPaseFieldCheckList read FPaseFieldCheckList;

    property PropertyReadPrefix: string read GetPropertyReadPrefix;
    property PropertyWritePrefix: string read GetPropertyWritePrefix;

    property ConstructorAllocateResources: string read GetConstructorAllocateResources;
    property ConstructorFieldInit: string read GetConstructorFieldInit;
    property DestructorFreeResources: string read GetDestructorFreeResources;

    property IsArray: boolean read GetIsArray;
    property ArrayDimension: integer read GetArrayDimension;

    // --- to remove
    property textproperty: string read FTextProperty write SetTextProperty;
  end;

  TPaseFieldList = specialize TFPGObjectList<TPaseField>;

  { TPaseForeignKey }

  TPaseForeignKey = class(TPersistent)
  private
    FEntity: TPaseEntity;
    FFields: TStringList;
    FForeignFields: TStringList;
    FForeignEntity: TPaseEntity;
    FName: string;
  public
     constructor Create(const AName: string;
                        const AEntity: TPaseEntity; const AEntityFields: array of string;
                        const AForeignEntity: TPaseEntity; const AForeignFields: array of string);
     destructor Destroy; override;
  published
     property Name: string read FName write FName;
     property Entity: TPaseEntity read FEntity;
     property Fields: TStringList read FFields;
     property ForeignEntity: TPaseEntity read FForeignEntity;
     property ForeignFields: TStringList read FForeignFields;

  end;


  { TPaseEntityFkList }

  TPaseEntityFkList = TFPHashObjectList;


  { TPaseEntity }

  TPaseEntity = class(TPersistent)
  private
    FModel: TPaseModel;
    FCaption: string;
    FDescription: string;
    FEntityCheckList: TPaseEntityCheckList;
    FEntityIndexList: TPaseEntityIndexList;
    FForeignKeyList: TPaseEntityFkList;
    FHistory: TPaseHistory;
    FName: string;
    FPaseFieldList: TPaseFieldList;

    function DoAddField(const AFieldName, ACaption, AFieldType: string;
                        const ASize, ADecimal: integer): integer;

  public
    constructor Create(AModel: TPaseModel);
    destructor Destroy; override;

    function AddField(const AFieldName: string; const AFieldType: string; const ACaption: string = ''): integer; overload;
    function AddField(const AFieldName: string; const AFieldType: string;
                      const ASize: integer; const ACaption: string = ''): integer; overload;
    function AddField(const AFieldName: string; const AFieldType: string;
                      const ASize,ADecimal: integer; const ACaption: string = ''): integer; overload;

    function AddCheck(const ACheckName,ACheckDestination,ACheckExpression: string): integer;
    function AddIndex(const AIndexName: string; const IsPk: boolean; const AExprList: array of string; const ARdbmsEngine: string=''): TPaseIndex;


    {
    function AddArray(const AFieldName: string; const AFieldType: string; ABounds: TPaseArrayBounds;
                      const AElementType: string; const ACaption: string = ''): integer;
    }

    // to refactor... -->
    {
    function AddField(const AFieldName: string; const AFieldType: TPaseBaseType;
                      const ASize, ADecimal: integer; const ACaption: string = ''): integer; overload;

    function AddArray(const AFieldName: string; const DimCount: integer;
                      const AElementType: string; const ACaption: string = ''): integer; overload;
    }
    // <-- to refactor...

    function GetFieldByName(AFieldName: string): TPaseField;
    function AddRelation(const AFieldName, ARelationEntity, ARelationField, ACaption: string): integer;
    function AddForeignKey(const AName: string; const AEntityFields: array of string;
                           const AForeignEntity: TPaseEntity; const AForeignFields: array of string): TPaseForeignKey;

    // property FieldsX[const i: Integer]: TPaseField read GetFields;
    property Fields: TPaseFieldList read FPaseFieldList;
    property Checks: TPaseEntityCheckList read FEntityCheckList;
    property Indexes: TPaseEntityIndexList read FEntityIndexList;
    property ForeignKeys: TPaseEntityFkList read FForeignKeyList;
    property History: TPaseHistory read FHistory write FHistory;
    property Model: TPaseModel read FModel;
  published
    property Name: string read FName write FName;
    property Caption: string read FCaption write FCaption;
    property Description: string read FDescription write FDescription;
  end;

  TPaseEntityList = specialize TFPGObjectList<TPaseEntity>;


  { TPaseModel }

  TPaseModel = class(TPersistent)
  private
    FEntityList: TPaseEntityList;
    FCaption: string;
    FDataTypeList: TPaseFieldDataTypeList;
    FHistory: TPaseHistory;
    FName: string;
    FRelease: string;
    procedure SetName(AValue: string);
    procedure SetRelease(AValue: string);

  public
    constructor Create(const AName: String);
    destructor Destroy; override;

    function GetEntityByName(const AEntityName: string): TPaseEntity;
    // function GetFieldDataTypeByDataType(const AValue: TPaseBaseType): TPaseDataType;
    function GetFieldDataTypeByName(const AValue: string): TPaseDataType;

    function AddEntity(const AEntityName: String; const AEntityCaption: String; const AEntityDescription: String = ''): integer;

    function AddDataType(const AUserTypeName: string; const ABaseDataType: TPaseBaseType): TPaseDataType;

    function AddUserType(const AUserTypeName, AInherithTypeName: string): TPaseDataType; overload;
    function AddUserType(const AUserTypeName, AInherithTypeName: string; const ASize: integer): TPaseDataType; overload;
    function AddUserType(const AUserTypeName, AInherithTypeName: string; const ASize, ADecimal: integer): TPaseDataType; overload;

    function AddUserType_Array(const AUserTypeName, ADataType: string; const DimCount: integer): TPaseDataType;


    // property Entities[const index: integer]: TPaseEntity read GetEntity; // to remove
    property Entities: TPaseEntityList read FEntityList;
    property DataTypes: TPaseFieldDataTypeList read FDataTypeList;
  published
    property Name: string read FName write SetName;
    property Caption: string read FCaption write FCaption;
    property History: TPaseHistory read FHistory;
    property Release: string read FRelease write SetRelease;
  end;

implementation

uses typinfo, StrUtils, variants;

function iif(const ACondition: boolean; const AValueTrue, AValueFalse: string): string;
begin
  if ACondition then Result := AValueTrue
                else Result := AValueFalse;
end;

{ TPaseForeignKey }

constructor TPaseForeignKey.Create(const AName: string;
                                   const AEntity: TPaseEntity; const AEntityFields: array of string;
                                   const AForeignEntity: TPaseEntity; const AForeignFields: array of string);
type TRelationMatrix = record
        Field: TPaseField;
        ForeignField: TPaseField;
     end;
var s,h,f1,f2: string;
    m: array of TRelationMatrix;
    scan: integer;
begin
  // test
  if not Assigned(AForeignEntity) then
     raise TPaseErr_PaseEntity_NotFound.Create('TPaseForeignKey.Create: AForeignEntity not found!');

  for s in AForeignFields do
      if not Assigned(AForeignEntity.GetFieldByName(s)) then
         raise TPaseErr_PaseField_NotFound.CreateFmt('TPaseForeignKey.Create: field %s not found in %s entity!',
                                                     [s, AForeignEntity.Name]);

  // create relation data matric
  SetLength(m, length(AEntityFields));
  for scan:=low(AEntityFields) to high(AEntityFields) do begin
     m[scan].Field :=AEntity.GetFieldByName(AEntityFields[scan]);
     m[scan].ForeignField := AForeignEntity.GetFieldByName(AForeignFields[scan]);

     if not Assigned(m[scan].Field) then
        raise TPaseErr_PaseField_NotFound.CreateFmt('Unable to find %s.%s field!',
                                                    [AEntity.Name, AEntityFields[scan]]);
     if not Assigned(m[scan].ForeignField) then
        raise TPaseErr_PaseField_NotFound.CreateFmt('Unable to find %s.%s field!',
                                                    [AForeignEntity.Name, AForeignFields[scan]]);
  end;

  // allocate resources
  inherited Create;
  FName:=AName;
  FEntity:=nil;
  FFields:=TStringList.Create;
  FForeignEntity:=nil;
  FForeignFields:=TStringList.Create;

  // set relation
  FEntity:=AEntity;
  FForeignEntity:=AForeignEntity;

  for scan:=low(m) to high(m) do begin
     Fields.Add(TPaseField(m[scan].Field).Name);
     ForeignFields.Add(TPaseField(m[scan].ForeignField).Name);

     TPaseField(m[scan].Field).SetRelation(AName,
                                           TPaseField(m[scan].Field).Name,
                                           TPaseField(m[scan].ForeignField));
  end;


  // history
  f1:='';
  for s in Fields do
      f1:=f1+IfThen(f1='', '', ',')+s;
  f2:='';
  for s in ForeignFields do
      f2:=f2+IfThen(f2='', '', ',')+s;
  h:=Format('%s(%s) -> %s(%s)', [AEntity.Name, f1, AForeignEntity.Name, f2]);

  if AEntity.History.Active then
     AEntity.History.AddHistory(phiFk, phoNew, self.ClassName, AName, h);



end;

destructor TPaseForeignKey.Destroy;
begin
  FreeAndNil(FFields);
  FreeAndNil(FForeignFields);
  inherited Destroy;
end;

{ TPaseIndex }

constructor TPaseIndex.Create(const AName: string; const IsPK: boolean;
  const ADestination: String);
begin
  inherited Create;
  FName:=AName;
  FDestination:=ADestination;
  FIsPrimaryKey:=IsPK;
end;

function TPaseIndex.AddIndexExpression(const AFileName: string; AAscending: boolean): TPaseIndex;
begin
   FExpression:=FExpression+
                IfThen(FExpression<>'', ', ', '') +
                AFileName +
                IfThen(AAscending, '', ' desc');

   result := self;
end;

function TPaseIndex.SetIndexFieldList(const AFieldNames: array of string): TPaseIndex;
var s: string;
begin
  FExpression:='';

  for s in AFieldNames do begin
     FExpression:=FExpression+
                  IfThen(FExpression<>'', ', ', '') +
                  s;
  end;
  result := self;
end;

procedure TPaseIndex.Clear;
begin
  FExpression:='';
end;

{ TPaseFieldCheck }

constructor TPaseFieldCheck.Create(const AName, ADestination,AExpression: String);
begin
  inherited Create;

  FName:=AName;
  FDestination:=ADestination;
  FExpression:=AExpression;

end;

{ TPaseDataType }

function TPaseDataType.GetHasDecimal: boolean;
begin
  result:=(FBaseType in [pftFloat]) and (FDecimal > 0);
end;

function TPaseDataType.GetArrayBounds(Index: integer): TPaseArrayBound;
begin
  result:=FArrayBounds[Index];
end;

function TPaseDataType.GetCollate: string;
begin

  if FBaseType in [pftFixedString, pftSizedString, pftString] then
     result := FCollate
  else
     result :='';

end;

function TPaseDataType.GetDefaultAsString: string;
begin

  result:=VarToStr(FDefaultAsVariant);

end;

function TPaseDataType.GetIsArray: boolean;
begin
  result:= FBaseType in [pftStaticArray];
end;

function TPaseDataType.GetIsCollection: boolean;
begin
  result:=false; // not yet implemented
end;

function TPaseDataType.GetIsInherited: boolean;
begin
  result:=Trim(InheritedType)<>'';
end;


function TPaseDataType.GetIsNumeric: boolean;
begin
  result:=FBaseType in [pftInteger, pftFloat];
end;

function TPaseDataType.GetIsScalar: boolean;
begin
  result:=FBaseType in [ pftBoolean, pftDate, pftTime, pftDateTime,pftBlob,
                         pftMemo, pftGraphic, pftTimeStamp,  pftGuid,
                         pftString, pftInteger, pftSizedString, pftFixedString,
                         pftFloat ];
end;

function TPaseDataType.GetIsSized: boolean;
begin
  result:=(BaseType in [pftInteger, pftSizedString, pftFixedString, pftFloat])
          or
          (FSize>0);
end;


{
function TPaseDataType.SetDefaultAsFloat(AValue: extended): TPaseDataType;
begin
  FDefaultValue.AsFloat:=AValue;
  FDefaultAsVariant:=AValue;

  result := self;

  if History.Active then
     History.AddHistory(phiDataType, phoEdit, self.ClassName, self.Name,
                        Format('Default: %*.*f', [self.Size, self.Decimal, AValue]));
end;
}

function TPaseDataType.SetCheckValue(AValue: string): TPaseDataType;
begin
  FCheckValue:=AValue;

  if History.Active then
     History.AddHistory(phiDataType, phoEdit, self.ClassName, self.Name,
                        IfThen(Length(FCheckValue)> 80,
                               copy(FCheckValue,1,77)+'...',
                               FCheckValue));

  result := self;
end;

function TPaseDataType.SetDefaultValue(AValue: variant): TPaseDataType;
var s: string;
begin
  FDefaultAsVariant:=AValue;

  if History.Active then begin
     s:=VarToStr(FDefaultAsVariant);
     History.AddHistory(phiDataType, phoEdit, self.ClassName, self.Name,
                        'DEFAULT: ' +
                        IfThen(Length(s)> 80,
                               copy(s,1,77)+'...',
                               s  ));
  end;

  result := self;
end;

{
function TPaseDataType.SetDefaultValue(AValue: string): TPaseDataType;
begin
  FDefaultValue.AsString:=AValue;

  if History.Active then
     History.AddHistory(phiDataType, phoEdit, self.ClassName, self.Name,
                        'DEFAULT: ' +
                        IfThen(Length(FDefaultValue.AsString)> 80,
                               copy(FDefaultValue.AsString,1,77)+'...',
                               FDefaultValue.AsString));

  result := self;
end;
}

function TPaseDataType.SetArrayBoundsByDim(const DimIndex, LowIndex,
  HighIndex: integer; const IndexType: string): TPaseDataType;
begin

  if CompareText(IndexType, 'integer') <> 0 then
     raise TPaseErr_FeatureNotSupported.Create('array index of type ' + IndexType + ' not supported!');


  result:=self;

  FArrayBounds[DimIndex].Assigned:=True;
  FArrayBounds[DimIndex].HighIndex:=HighIndex;
  FArrayBounds[DimIndex].LowIndex:=LowIndex;

  if History.Active then
     History.AddHistory(phiDataType, phoEdit, self.ClassName, self.Name, self.AsString);

end;

function TPaseDataType.SetRequired(AValueRequired: boolean): TPaseDataType;
begin
   result := self;

   if FIsRequired=AValueRequired then
      exit;

   FIsRequired:=AValueRequired;
end;

constructor TPaseDataType.Create(const AName: String; const AFieldType: TPaseBaseType);
begin
  inherited Create;

  FName:=AName;

  FIsRequired    :=FALSE;
  FCollate       :='COLLATE NONE';
  FInheritedType := '';
  FBaseType      :=AFieldType;
  FSize          :=-1;
  FDecimal       :=-1;
  SetLength(FArrayBounds, 0);

  FArrayElementType:=nil;
end;

constructor TPaseDataType.Create(const AName: String; const AFieldType: TPaseBaseType;
                                      const ASize: integer; const AHasFixedSize: boolean);
begin
  Create(AName, AFieldType);
  FSize:=ASize;
  FHasFixedSize:=AHasFixedSize;
end;

constructor TPaseDataType.Create(const AName: String; const AFieldType: TPaseBaseType;
                                      const ASize, ADecimal: integer);
begin
  if not (AFieldType in [pftFloat]) then
     raise exception.Create('Only pftFloat data type can have decimals!');

  Create(AName, AFieldType);
  FHasFixedSize:=False;
  FSize:=ASize;
  FDecimal:=ADecimal;
end;

constructor TPaseDataType.CreateArray(const AName: String;
  const DimCount: integer; const AElementType: TPaseDataType);
var i: integer;
begin
  Create(AName, pftStaticArray);
  FArrayElementType:=AElementType;
  SetLength(FArrayBounds, DimCount);
  for i:=Low(FArrayBounds) to High(FArrayBounds) do
    FArrayBounds[i].Assigned:=FALSE;
end;


function TPaseDataType.AsString: string;
var i: integer;
begin
  case FBaseType of
     pftStaticArray: begin
                        Result:=GetEnumName(TypeInfo(TPaseBaseType), Ord(FBaseType)) + '[';

                        for i:=low(FArrayBounds) to high(FArrayBounds) do
                            result:= result +
                                     IfThen(i=0, '', ',') +
                                     IfThen(ArrayBounds[i].Assigned,
                                            IntToStr(ArrayBounds[i].HighIndex) + '..' + IntToStr(ArrayBounds[i].LowIndex),
                                            '?');
                        result:=result + '] of ' + FArrayElementType.Name;

                     end;

     pftSizedString: begin

        Result:=GetEnumName(TypeInfo(TPaseBaseType), Ord(FBaseType));
        // if not IsInherited then begin

           Result:=result + '(' + IfThen(IsSized, IntToStr(Size), '?') + ')';
           if HasFixedSize then
              result:=result + IfThen(FHasFixedSize, ' space padded', '');

        // end else begin // IsInherited

        //    Result:=Result+'<-';

        // end;
     end;

     pftFloat: begin
        Result:=GetEnumName(TypeInfo(TPaseBaseType), Ord(FBaseType)) +
                '(' + IfThen(IsSized, IntToStr(Size), '?') +
                      IfThen(HasDecimals, ',' + IntToStr(Decimal), '') + ')';

     end;

  else
     Result:=GetEnumName(TypeInfo(TPaseBaseType), Ord(FBaseType));
  end;

end;

{ TPaseHistory }

function TPaseHistory.GetHistoryItem( index: integer ): TPaseHistoryItem;
begin
  result :=  FPaseHistoryItemArray[index];
end;

function TPaseHistory.GetText: string;
var hi: TPaseHistoryItem;
begin
  result := '';
  for hi in FPaseHistoryItemArray do begin
     result := result + Format('%-18.18s %-10.10s %-1.1s %-13.13s %-22.22s: %s' + #$A,
                               [ FormatDateTime('yyyy-mm-dd hh:nn:ss', hi.DateTime),
                                 PaseHistoryItemTypeAsString(hi.ItemType),
                                 PaseHistoryOperationAsString(hi.Operation),
                                 hi.ClassName, hi.ItemName, hi.ItemData
                               ]);
  end;

end;

constructor TPaseHistory.Create;
begin
  SetLength(FPaseHistoryItemArray, 0);
  FActive := True;
end;

destructor TPaseHistory.Destroy;
begin
  SetLength(FPaseHistoryItemArray, 0);
  inherited Destroy;
end;

procedure TPaseHistory.AddHistory(const AHistoryItemType: TPaseHistoryItemType;
                                  const APaseHistoryOperation: TPaseHistoryOperation;
                                  const AClass, AName, AData: string);
var i: integer;
begin
  i := Length(FPaseHistoryItemArray);
  SetLength(FPaseHistoryItemArray, i + 1);
  with FPaseHistoryItemArray[i] do begin
     DateTime:=Now;
     ItemType:=AHistoryItemType;
     Operation:=APaseHistoryOperation;
     ClassName:=AClass;
     ItemName:=AName;
     ItemData:=AData;
  end;
end;



{ TPaseModel }


// function TPaseModel.GetEntity(const index: integer): TPaseEntity;
// begin result := FPaseEntityList[index]; end;


function TPaseModel.GetEntityByName(const AEntityName: string): TPaseEntity;
var Entity: TPaseEntity;
begin
  result := nil;
  for Entity in FEntityList do
    if Entity.Name = AEntityName then begin
       result := Entity;
       break;
    end;
end;

function TPaseModel.GetFieldDataTypeByName(const AValue: string): TPaseDataType;
begin

  result := TPaseDataType(DataTypes.Find(AValue));

  if not Assigned(result) then
     raise TPaseErr_PaseDataType_NotFound.Create('Unable to find "' + AValue + '" TPaseDataType!');

end;


procedure TPaseModel.SetRelease(AValue: string);
begin
  if FRelease=AValue then Exit;
  FRelease:=AValue;

  History.AddHistory(phiModel, phoEdit, ClassName, 'Release', AValue);
end;

procedure TPaseModel.SetName(AValue: string);
begin
  if FName=AValue then Exit;
  FName:=AValue;

  if History.Active then
     History.AddHistory(phiModel, phoEdit, self.ClassName, self.FName, 'Renamed: ' + self.FName);

end;

constructor TPaseModel.Create(const AName: String);
begin
    FHistory:=TPaseHistory.Create;
    FName:=AName;
    FCaption:=AName;

    if History.Active then
       History.AddHistory(phiModel, phoNew, self.ClassName, self.Name, self.Caption);

    FEntityList:=TPaseEntityList.Create;

    FDataTypeList:=TPaseFieldDataTypeList.Create;

    // create field data types
    AddDataType('Unknown',     pftUnknown);
    // sized field
    AddDataType('SizedString', pftSizedString);
    AddDataType('FixedString', pftFixedString);
    AddDataType('Integer',     pftInteger);
    AddDataType('Smallint',    pftIntegerSmall);
    AddDataType('Float',       pftFloat);
    // unsized
    AddDataType('String',     pftString);
    AddDataType('Boolean',    pftBoolean);


end;

destructor TPaseModel.Destroy;
begin
  FHistory.Free;
  FEntityList.Free;
  FDataTypeList.Free;
  inherited Destroy;
end;

function TPaseModel.AddEntity(const AEntityName: String;
  const AEntityCaption: String; const AEntityDescription: String): integer;
begin

  result := FEntityList.Add(TPaseEntity.Create(self));
  with FEntityList[ result ] do begin
    Name:= AEntityName;
    Caption:= AEntityCaption;
    Description:= AEntityDescription;
    History:=self.History;
  end;

  if History.Active then
     History.AddHistory(phiEntity, phoNew, TPaseEntity.ClassName, AEntityName, Format('Caption=%s;', [AEntityName, AEntityCaption]));

end;

function TPaseModel.AddDataType(const AUserTypeName: string; const ABaseDataType: TPaseBaseType): TPaseDataType;
begin

  result := DataTypes[ DataTypes.Add( AUserTypeName,
                                                TPaseDataType.Create(AUserTypeName,
                                                                     ABaseDataType) )
                          ] as TPaseDataType;

  result.History:=self.History;

  if History.Active then
     History.AddHistory(phiDataType, phoNew, result.ClassName, result.Name, result.AsString);

end;

function TPaseModel.AddUserType(const AUserTypeName, AInherithTypeName: string): TPaseDataType;
var xDataType: TPaseDataType;
begin

   xDataType := TPaseDataType(DataTypes.Find(AInherithTypeName));
   result := DataTypes[ DataTypes.Add( AUserTypeName,
                                                 TPaseDataType.Create(AUserTypeName,
                                                                      xDataType.BaseType) )
                           ] as TPaseDataType;

   result.History:=self.History;
   result.InheritedType:=AInherithTypeName;

   if History.Active then
      History.AddHistory(phiDataType, phoNew, result.ClassName, result.Name, result.AsString);

end;

function TPaseModel.AddUserType(const AUserTypeName,
  AInherithTypeName: string; const ASize: integer): TPaseDataType;
var xDataType: TPaseDataType;
begin

   xDataType := TPaseDataType(DataTypes.Find(AInherithTypeName));
   result := DataTypes[ DataTypes.Add( AUserTypeName,
                                                 TPaseDataType.Create(AUserTypeName,
                                                                      xDataType.BaseType,ASize) )
                           ] as TPaseDataType;

   result.History:=self.History;
   result.InheritedType:=AInherithTypeName;
   result.Size:=ASize;

   if History.Active then
      History.AddHistory(phiDataType, phoNew, result.ClassName, result.Name, result.AsString);

end;

function TPaseModel.AddUserType(const AUserTypeName,
  AInherithTypeName: string; const ASize, ADecimal: integer): TPaseDataType;
var xDataType: TPaseDataType;
begin

  xDataType := TPaseDataType(DataTypes.Find(AInherithTypeName));
  result := DataTypes[
              DataTypes.Add( AUserTypeName,
                                  TPaseDataType.Create(AUserTypeName, xDataType.BaseType,ASize,ADecimal) )
                          ] as TPaseDataType;

  result.History:=self.History;
  result.InheritedType:=AInherithTypeName;

  if History.Active then
     History.AddHistory(phiDataType, phoNew, result.ClassName, result.Name, result.AsString);

end;

function TPaseModel.AddUserType_Array(const AUserTypeName, ADataType: string; const DimCount: integer): TPaseDataType;
begin

   result := DataTypes[
                  DataTypes.Add( AUserTypeName,
                                      TPaseDataType.CreateArray(AUserTypeName,
                                                                DimCount,
                                                                GetFieldDataTypeByName(ADataType))
                  )
             ] as TPaseDataType;

   result.History:=self.History;

   if History.Active then
      History.AddHistory(phiDataType, phoNew, result.ClassName, result.Name, result.AsString);

end;

{ TPaseEntity }

function TPaseEntity.DoAddField(const AFieldName, ACaption, AFieldType: string;
  const ASize, ADecimal: integer): integer;
begin

  result := FPaseFieldList.Add(TPaseField.Create(AFieldName));


  with FPaseFieldList[result] do begin
     Caption:=ACaption;
     History:=self.History;

     with FPaseFieldList[result] do
        try
           SetFieldType(Model.GetFieldDataTypeByName(AFieldType), ASize, ADecimal);
        except
           on e: exception do
              SetFieldType(Model.GetFieldDataTypeByName('Unknown'));
        end;
   end;


  if History.Active then
     History.AddHistory(phiField, phoNew, TPaseField.ClassName, AFieldName,
                        Format('Type=%s;Caption=%s;', [FPaseFieldList[result].GetFieldTypeAsString,
                                                       FPaseFieldList[result].Caption]));

end;


constructor TPaseEntity.Create(AModel: TPaseModel);
begin
  FModel:=AModel;
  FPaseFieldList:= TPaseFieldList.Create(true);
  FEntityCheckList:=TPaseEntityCheckList.Create(true);
  FEntityIndexList:=TPaseEntityIndexList.Create(true);
  FForeignKeyList:=TPaseEntityFkList.Create(true);
end;

destructor TPaseEntity.Destroy;
begin
  FForeignKeyList.Free;
  FEntityIndexList.Free;
  FEntityCheckList.Free;
  FPaseFieldList.Free;
  inherited Destroy;
end;


function TPaseEntity.AddField(const AFieldName: string; const AFieldType: string;
                              const ACaption: string): integer;
begin

  // add field / no size needed
  result:=DoAddField(AFieldName, ACaption, AFieldType, -1, -1);

end;

function TPaseEntity.AddField(const AFieldName: string; const AFieldType: string;
                              const ASize: integer; const ACaption: string): integer;
begin

  // add field / size needed
  result:=DoAddField(AFieldName, ACaption, AFieldType, ASize, -1);

end;

function TPaseEntity.AddField(const AFieldName: string; const AFieldType: string;
                              const ASize, ADecimal: integer; const ACaption: string): integer;
begin

  // add field / size needed
  result:=DoAddField(AFieldName, ACaption, AFieldType, ASize, ADecimal);

end;

function TPaseEntity.AddCheck(const ACheckName, ACheckDestination, ACheckExpression: string): integer;
begin
  result := FEntityCheckList.Add(ACheckName, TPaseCheck.Create(ACheckName, ACheckDestination, ACheckExpression));
  if History.Active then
     History.AddHistory(phiCheck, phoNew, FEntityCheckList[result].ClassName, ACheckName,
                        Format('%s: %s', [ACheckDestination, ACheckExpression]));

end;

function TPaseEntity.AddIndex(const AIndexName: string; const IsPk: boolean;
  const AExprList: array of string; const ARdbmsEngine: string): TPaseIndex;
begin
  result := TPaseIndex.Create(AIndexName, IsPk, ARdbmsEngine);
  TPaseIndex(FEntityIndexList[FEntityIndexList.Add(AIndexName, result)]).SetIndexFieldList(AExprList);
  if History.Active then
     History.AddHistory(phiIndex, phoNew, result.ClassName, result.Name,
                        IfThen(ARdbmsEngine='', '', ARdbmsEngine + ': ') +
                        IfThen(IsPk, 'Primary Key', 'Index') + ': ' + result.Expression);
end;


function TPaseEntity.GetFieldByName(AFieldName: string): TPaseField;
var Field: TPaseField;
begin
  result := nil;
  for Field in FPaseFieldList do
    if Field.Name = AFieldName then begin
       result := Field;
       break;
    end;
end;

function TPaseEntity.AddRelation(const AFieldName, ARelationEntity, ARelationField, ACaption: string): integer;
var Entity: TPaseEntity;
    Field: TPaseField;
begin
  // test
  Entity := Model.GetEntityByName(ARelationEntity);
  if Entity = nil then
     raise exception.CreateFmt('Unable to find "%s" entity!', [ARelationEntity]);
  Field := Entity.GetFieldByName(ARelationField);
  if Field = nil then
     raise exception.CreateFmt('Unable to find "%s" field in entity "%"!', [ARelationField, ARelationEntity]);

  // add relation
  result := FPaseFieldList.Add(TPaseField.Create(AFieldName));
  with FPaseFieldList[result] do begin
    SetRelation(ARelationEntity, ARelationField, Field);
    Caption:=ACaption;
  end;

  if History.Active then
     History.AddHistory(phiField, phoNew, TPaseField.ClassName,
                        AFieldName + iif(FPaseFieldList[result].IsRelationField, ' (R)', ''),
                        Format('Relation=%s.%s;Type=%s;Caption=%s;',
                               [ FPaseFieldList[result].RelationEntityName,
                                 FPaseFieldList[result].RelationEntityField,
                                 FPaseFieldList[result].GetFieldTypeAsString,
                                 FPaseFieldList[result].Caption]));

end;

function TPaseEntity.AddForeignKey(const AName: string; const AEntityFields: array of string;
                                   const AForeignEntity: TPaseEntity; const AForeignFields: array of string): TPaseForeignKey;
begin

  // add relation

  result := TPaseForeignKey(FForeignKeyList[
                 FForeignKeyList.Add(AName, TPaseForeignKey.Create(AName, self, AEntityFields, AForeignEntity, AForeignFields))
            ]);


end;


{ TPaseField }

procedure TPaseField.SetIsNotNull(AValue: Boolean);
begin
  if FIsNotNull=AValue then
     Exit;

  if FIsPrimary and (not AValue) then
     raise Exception.Create('Primary Key field cannot be null!');

  FIsNotNull:=AValue;
end;

function TPaseField.GetDecimal: integer;
begin
  if not IsRelationField then
     result := FDecimal
  else
     result:=FRelationField.GetDecimal;
end;

function TPaseField.GetConstructorAllocateResources: string;
begin
  result := '// GetConstructorAllocateResources';
end;

function TPaseField.GetArrayBounds(Index: integer): TPaseArrayBound;
begin
  result:=FArrayBounds[Index];
end;



function TPaseField.GetArrayDimension: integer;
begin
   result:=Length(FArrayBounds);
end;

function TPaseField.GetConstructorFieldInit: string;
begin
  result := '// GetConstructorFieldInit';
end;

function TPaseField.GetDestructorFreeResources: string;
begin
  result := '// GetDestructorFreeResources';
end;


function TPaseField.GetIsArray: boolean;
begin
  result := Length(FArrayBounds) > 0;
end;

function TPaseField.GetPropertyReadPrefix: string;
begin
  if pfaGetter in Attributes then
     result:='Get'
  else
     result:='F';
end;

function TPaseField.GetPropertyWritePrefix: string;
begin
  if pfaSetter in Attributes then
     result:='Set'
  else
     result:='F';
end;

function TPaseField.GetSize: integer;
begin
  if not IsRelationField then
     result := FSize
  else
     result:=FRelationField.GetSize;
end;

function TPaseField.GetTextProperty: string;
begin
   result := FTextProperty;
end;

procedure TPaseField.SetAttributes(AValue: TPaseFieldAttributes);
var sApp: string;
    xAttrib: TPaseFieldAttribute;
begin
  if FAttributes=AValue then Exit;
  FAttributes:=AValue;

  if History.Active then begin
     sApp:='';
     for xAttrib in AValue do
       sApp:=sApp+IfThen(sApp<>'', ',', '')+GetEnumName(TypeInfo(TPaseFieldAttribute), Ord(xAttrib));;

     History.AddHistory(phiField, phoEdit, self.ClassName, self.Name,
                        Format('Attributes=%s;', [sApp]));
  end;

end;

procedure TPaseField.SetIsPrimary(AValue: Boolean);
begin
  if FIsPrimary=AValue then
     Exit;

  FIsPrimary:=AValue;
  if AValue then
     FIsNotNull:=True;
end;

procedure TPaseField.SetTextProperty(AValue: string);
begin
  if FTextProperty=AValue then Exit;
  FTextProperty:=AValue;
end;

procedure TPaseField.ResetDataType;
begin
  FFieldDataType:=nil;

  FIsPrimary:=False;
  FIsNotNull := False;
  FSize:=-1;
  FDecimal:=-1;
  FIsRelationField:= False;
  FScope:=pfsPublished;
  FAttributes:=[];
  FArrayElementType:='';
  SetLength(FArrayBounds, 0);
end;

constructor TPaseField.Create(const AName: String;
                              const AFieldDataType: TPaseDataType;
                              const ASize: integer; const ADecimal: integer);
begin
  Create(AName);
  SetFieldType(AFieldDataType, ASize, ADecimal);
end;


procedure TPaseField.SetFieldType(const AFieldDataType: TPaseDataType;
  ASize: integer; ADecimal: integer);
begin

  ResetDataType;

  FFieldDataType:=AFieldDataType;

  if FFieldDataType.IsSized then
     FFieldDataType.FSize:=ASize;

  if FFieldDataType.IsNumeric then
     if FFieldDataType.HasDecimals then
        FFieldDataType.FDecimal:=ADecimal;

end;

procedure TPaseField.SetFieldType(const AFieldDataType: TPaseDataType;
  const ABounds: TPaseArrayBounds; const AElementType: TPaseDataType);
begin
  ResetDataType;

  // WARNING / ANTI HINT
  if AElementType = nil then
     FFieldDataType:=nil;

  FFieldDataType:=AFieldDataType;
  SetLength(FFieldDataType.FArrayBounds, Length(ABounds));
  FArrayBounds:=ABounds;

end;

procedure TPaseField.SetDefaultValueAsNull;
begin
  FDefaultValue.IsNull:=TRUE;

  if History.Active then
     History.AddHistory(phiField, phoEdit, self.ClassName, self.Name, 'Default=NULL');
end;

procedure TPaseField.SetDefaultValueAsString(const AValue: string);
begin
  FDefaultValue.IsNull:=False;

  if History.Active then
     History.AddHistory(phiField, phoEdit, self.ClassName, self.Name, 'Default='+QuotedStr(AValue));
end;

procedure TPaseField.AddCheck(const ACheckName, ACheckDestination,
  ACheckExpression: string);
begin

  FPaseFieldCheckList.Add(ACheckName, TPaseFieldCheck.Create(ACheckName, ACheckDestination, ACheckExpression));

  if History.Active then
       History.AddHistory(phiField, phoEdit, self.ClassName, self.Name,
                          Format('Check %s (%s): %s', [ACheckName, ACheckDestination, ACheckExpression]));

end;

procedure TPaseField.SetRelation(const ARelationEntityName,
  ARelationFieldName: string; const AField: TPaseField);
begin
  FIsRelationField:=True;
  FRelationEntityName:=ARelationEntityName;
  FRelationEntityField:=ARelationfieldName;
  FRelationField:=AField;
end;

function TPaseField.GetFieldTypeAsString: string;
var xFldTpe: TPaseDataType;
    xSize, xDecimal: integer;
begin

  result:= '';

  if not IsRelationField then begin
     // common entity field
     xFldTpe:=FieldDataType;
     xSize:=Size;
     xDecimal:=Decimal;
  end else begin
     // relation field
     xFldTpe:=FRelationField.FieldDataType;
     xSize:=FRelationField.Size;
     xDecimal:=FRelationField.Decimal;
  end;

  // generate description
  result:=FieldDataType.Name;

  if xFldTpe.IsArray then begin
     for xSize:=low(FArrayBounds) to high(FArrayBounds) do
       result:=result+
               '['+IntToStr(FArrayBounds[xSize].LowIndex)+','+
                   IntToStr(FArrayBounds[xSize].HighIndex) + ']';
     result:=result+' of ...'; // + FieldDataType.ArrayElementType.FName;
     xFldTpe:=FieldDataType.ArrayElementType;
     xSize:=Size;
     xDecimal:=Decimal;
  end;


  if xFldTpe.IsSized then
     if xFldTpe.IsInherited then begin // inherited
        result:=FieldDataType.Name;
     end else begin // not inherited
       if xFldTpe.IsNumeric and xFldTpe.HasDecimals then
          result:=FieldDataType.Name+Format('[%2d,%2d]', [xSize, xDecimal])
       else
          result:=FieldDataType.Name+IfThen(FieldDataType.Size>0, Format('[%2d]', [FieldDataType.Size]), '[?]');
     end;

  if xFldTpe.IsCollection then begin
     result:=FieldDataType.Name + ' (collection not yet implemented)';
  end;

end;

constructor TPaseField.Create(const AName: String);
begin
  inherited Create;

  FHistory:=nil;
  FName := AName;
  FPaseFieldCheckList:= TPaseFieldCheckList.Create(true);
  ResetDataType;
end;


destructor TPaseField.Destroy;
begin
  SetLength(FArrayBounds, 0);
  FPaseFieldCheckList.Free;
  inherited Destroy;
end;

end.

