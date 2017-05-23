program pase;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp
  , upaseglobals
  , upasetypes
  , upase_diagram_employee
  , upase_render_rdbms
  { you can add units after this };


type

  { TErDiagram }

  TErDiagram = class(TCustomApplication)
  private
    FModel: TPaseModel;
    FOutType: string;
    FProjectRoot: string;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;

    property Model: TPaseModel read FModel write FModel;
    property ProjectRoot: string read FProjectRoot write FProjectRoot;
    property OutType: string read FOutType write FOutType;
  end;

{ TErDiagram }

procedure TErDiagram.DoRun;
var ErrorMsg: String;

begin
  Writeln('');
  Writeln('pase - by Basso Marcello');
  Writeln('release: experimental');

  // quick check parameters
  ErrorMsg:=CheckOptions('hpo', ['help', 'project', 'output']);
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if HasOption('p', 'project') then begin
     FProjectRoot:=ExpandFileName( GetOptionValue('p', 'project') );
  end;

  if HasOption('o', 'output') then begin
     FOutType:=GetOptionValue('o', 'output');
     if FOutType='' then
        FOutType:='SQL';
     FOutType:=copy(UpperCase(FOutType),1,3);
     case FOutType of
        'SQL': FOutType:='SCRIPT SQL';
        'HIS': FOutType:='HISTORY';
     else
        FOutType:='ERR';
     end;
  end;

  writeln('Project root: ', FProjectRoot);
  writeln('Output type : ', FOutType);
  if FOutType='ERR' then begin
     WriteHelp;
     Terminate;
     Exit;
  end;
  writeln('');



  CreateDiagramEmployee(Model);


  { add your program here }
  case OutType of
     'SQL':
           with TPase_RenderRdbms.Create(Model, FProjectRoot + PathDelim + 'templates') do
              try
                Writeln(Render);
              finally
                Free;
              end;

     'HISTORY':
           Writeln(Model.History.Text);
  end;

  // stop program loop
  Terminate;
end;

constructor TErDiagram.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FModel:=TPaseModel.Create('unnamed');
  FProjectRoot:=ExtractFilePath(ExeName);
  FOutType:='SQL';
  StopOnException:=True;
end;

destructor TErDiagram.Destroy;
begin
  FModel.Free;
  inherited Destroy;
end;

procedure TErDiagram.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
  writeln('Usage: ', ExeName, ' -p projectroot [-o {SQL|HIS}]');
end;

var
  Application: TErDiagram;
begin
  Application:=TErDiagram.Create(nil);
  Application.Title:='pase';
  Application.Run;
  Application.Free;
end.

