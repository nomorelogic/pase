unit upase_render_rdbms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  , upaseglobals
  , QTemplate
  , typinfo;


type

   { TPase_RenderRdbms }

   TPase_RenderRdbms = class(TPersistent)

   private
     FHeader: TStringList;
     FModel: TPaseModel;

     dt: TPaseDataType;

     FTemplateFolder: string;

     function DoRenderDataTypes: string;
     function ReplaceTags(const ATag: String; AParams: TStringList): String;
   public
     constructor Create(const AModel: TPaseModel; const ATemplateFolder: string);
     destructor Destroy; override;
     function Render: string;

   published
      property Model: TPaseModel read FModel write FModel;
      property Header: TStringList read FHeader write FHeader;
      property TemplateFolder: string read FTemplateFolder write FTemplateFolder;
   end;



implementation

uses variants
     , Regexpr
     , StrUtils;

{ TPase_RenderRdbms }

function TPase_RenderRdbms.DoRenderDataTypes: string;
var scan, i: integer;
    sl, curr: TStringList;
    g: TQTemplate;
    s,sBreak,t: string;
    NeedReplace: boolean;
begin

   sl:=TStringList.Create;
   curr:=TStringList.Create;
   g:=TQTemplate.Create('');
   try
     sl.LoadFromFile(TemplateFolder + PathDelim + 'rdbms_datatype_firebird.sql');
     t:=sl.Text;
     sl.Clear;

     // todo: get properties from rtti

     // assign callbacks
     g.Tags['Name'] := @ReplaceTags;
     g.Tags['BaseType'] := @ReplaceTags;
     g.Tags['Size'] := @ReplaceTags;
     g.Tags['Decimal'] := @ReplaceTags;
     g.Tags['CheckValue'] := @ReplaceTags;
     g.Tags['DefaultValue'] := @ReplaceTags;
     g.Tags['IsRequired'] := @ReplaceTags;
     g.Tags['Collate'] := @ReplaceTags;


     // user data type
     sl.Add('/* user data type */');
     sl.Add('');
     for scan := 0 to Model.DataTypes.Count -1 do begin
         dt:=Model.DataTypes[scan] as TPaseDataType;
         if dt.InheritedType <> '' then begin
            s:='';
            g.Template:=t;
            NeedReplace:=False;
            repeat
               sBreak:=s;
               s:=g.GetContent;
               s:=StringReplace(s, '{<', '{+', [rfReplaceAll]);
               s:=StringReplace(s, '>}', '+}', [rfReplaceAll]);
               NeedReplace:=(pos(g.StartDelimiter,s) > 0) and (s<>sBreak) ;
               if NeedReplace then
                  g.Template:=s;
            until not NeedReplace;
            // remove empty lines -> bugged??
            // s := ReplaceRegExpr('\n\r?\s*\n\r?', s, '', True);
            i:=0;
            curr.Text:=s;
            while i<curr.Count do
               if trim(curr[i])='' then
                  curr.Delete(i)
               else
                  inc(i);
            // add
            sl.Add( curr.Text );
         end;
     end;

   finally
     result:=sl.Text;
     g.Free;
     sl.Free;
     curr.Free;
   end;

end;

function TPase_RenderRdbms.ReplaceTags(const ATag: String; AParams: TStringList): String;
var PI : PPropInfo;
    sPaseBaseType: string;
    v: variant;
    IgnoreParams: boolean = False;
begin
   result:='';
   PI:=GetPropInfo(dt,ATag);


   if Assigned(PI) then begin

      // get custom type at runtime: pi^.PROPTYPEREF^^
      case pi^.PropType^.Kind of
         tkAString         : result:=GetStrProp(dt,ATag); //  + '/'+pi^.PropType^.Name;
         tkInteger, tkInt64: result:=IntToStr(GetInt64Prop(dt,ATag));
         tkBool            : begin
             if GetInt64Prop(dt,ATag)=0 then result:='False'
                                        else result:='True';
             if (result<>'') and (not IgnoreParams) then
                if AParams.IndexOfName(result) >= 0 then
                   result := AParams.Values[result];
             IgnoreParams:=True;
         end;
         tkVariant         : begin
             v:=GetVariantProp(dt,ATag);
             result:=VarToStr(v);
             if VarType(v)=varstring then
                result:=QuotedStr(result);
          end;

         tkEnumeration: begin
             sPaseBaseType:=GetEnumProp(dt,ATag);
             result:=AParams.Values[sPaseBaseType];
         end
      else

        IgnoreParams:=True;
        result:=LineEnding + 'ERROR: "' + pi^.PropType^.Name + '" type not yet implemented!';

      end;

      //result:=GetStrProp(dt,ATag) + '*'+pi^.PropType^.Name;
      if (result <> '') and (not IgnoreParams) then
         if AParams.IndexOfName('Format') >= 0 then
            result := AParams.Values['Format'];

   end else begin
       result:=LineEnding + 'ERROR: "' + ATag + '" property not found!';
   end;

   // writeln(result);

end;

constructor TPase_RenderRdbms.Create(const AModel: TPaseModel; const ATemplateFolder: string);
begin
  inherited Create;

  FHeader:=TStringList.Create;

  FModel:=AModel;
  FTemplateFolder:=ATemplateFolder;

end;

destructor TPase_RenderRdbms.Destroy;
begin
  FHeader.Free;

  inherited Destroy;
end;

function TPase_RenderRdbms.Render: string;
var sl: TStringList;
begin
   sl:=TStringList.Create;
   try
     // header
     sl.AddStrings(Header);

     sl.Add(DoRenderDataTypes);

   finally
     result:=sl.Text;
     FreeAndNil(sl);
   end;
end;

end.

