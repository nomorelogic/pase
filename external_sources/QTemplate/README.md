<a href="https://www.paypal.com/cgi-bin/webscr?cmd=_donations&business=7WRXRMDSM72W8&lc=ID&currency_code=USD&bn=PP%2dDonationsBF%3abtn_donateCC_LG%2egif%3aNonHosted">
  <img src="https://www.paypalobjects.com/en_US/GB/i/btn/btn_donateCC_LG.gif" />
</a>

QTemplate - Object Pascal's quick templating engine
===

This unit implements an alternative to FPTemplate engine, with different approach.
FPTemplate uses event driven approach where a the core lies in OnReplaceTag callback.
For example, consider the following template:
```pascal
{+MyTag [Param1=Value1] [Param2=Value2]+}

An example OnReplaceTag callback:

procedure TMyTemplateHandler.MyReplaceTag(
        Sender      : TObject;
  const TagString   : String;
        TagParams   : TStringList;
  out   ReplaceText : String
);
begin
  (*
    Here, the parameter values are:
    TagString = 'MyTag'
    TagParams = [
      'Param1' = 'Value1'
      'Param2' = 'Value2'
    ]
  *)
  // Common implementation
  if TagString = 'MyTag' then begin
    // do replacement for MyTag
    ReplaceText := ... ;
  end else if TagString = ... begin
    // do replacement for other tag
  end else if TagString = ... begin
    // do replacement for other tag
  end else begin
    // not a recognized tag
  end;
end;
```
As you can see, a series of if statement must be implemented. This could get messy for template
with a lot of tags. QTemplate removes above burden to check for tag and instead gives a nice and
easy to use map like functionality to connect tag and callback. QTemplate way to solve above case
is:
```pascal
function TMyTemplateHandler.ReplaceMyTag(const ATag: String; AParams: TStringList): String;
begin
  
end;

function TMyTemplateHandler.ReplaceMyOtherTag(const ATag: String; AParams: TStringList): String;
begin
  
end;
```
then somewhere (constructor is a good choice):
```pascal
begin
  with TQTemplate.Create('templatefilename') do
    try
      Tags['MyTag'] := @ReplaceMyTag;
      Tags['MyOtherTag'] := @ReplaceMyOtherTag;
      { you may have same handler for a tag handling that differs a little, hence the idea
        of ATag parameter in the callback }
      Tags['MySomeOtherTag'] := @ReplaceMyOtherTag;
      WriteOutput(GetContent);
    finally
      Free;
    end;
end;
```
