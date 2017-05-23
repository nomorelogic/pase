unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterPas, SynHighlighterSQL,
  Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, ActnList
  , upaseglobals;

type

  { TForm1 }

  TForm1 = class(TForm)
    acLoadProject: TAction;
    acGetSQL: TAction;
    acGetHistory: TAction;
    acGetSource: TAction;
    acGetTemplate: TAction;
    ActionList1: TActionList;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    edRoot: TEdit;
    Label1: TLabel;
    pnTopHead: TPanel;
    pnTopTools: TPanel;
    pnTop: TPanel;
    pnBody: TPanel;
    SynEdit1: TSynEdit;
    SynPasSyn1: TSynPasSyn;
    SynSQLSyn1: TSynSQLSyn;
    procedure acGetHistoryExecute(Sender: TObject);
    procedure acGetSourceExecute(Sender: TObject);
    procedure acGetSQLExecute(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FModel: TPaseModel;
    FProjectRoot: string;
    procedure SetProjectRoot(AValue: string);
  private
    { private declarations }
    property ProjectRoot: string read FProjectRoot write SetProjectRoot;
    property Model: TPaseModel read FModel write FModel;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses upase_diagram_employee
     , upase_render_rdbms;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  SynEdit1.Clear;
  ProjectRoot:=ExtractFilePath(Application.ExeName);
  FModel:=TPaseModel.Create('undefined');
  CreateDiagramEmployee(Model);
end;

procedure TForm1.SetProjectRoot(AValue: string);
begin
  FModel.Free;

  if FProjectRoot=AValue then Exit;
  FProjectRoot:=AValue;

  edRoot.Text:=FProjectRoot;
end;

procedure TForm1.acGetSourceExecute(Sender: TObject);
begin
  SynEdit1.Clear;
  SynEdit1.Highlighter:=SynPasSyn1;
  SynEdit1.Lines.LoadFromFile(ProjectRoot + PathDelim + 'upase_diagram_employee.pas');
end;

procedure TForm1.acGetHistoryExecute(Sender: TObject);
begin
  SynEdit1.Clear;
  SynEdit1.Highlighter:=nil;
  SynEdit1.Lines.Text:=Model.History.Text;
end;

procedure TForm1.acGetSQLExecute(Sender: TObject);
begin
  SynEdit1.Clear;
  SynEdit1.Highlighter:=SynSQLSyn1;

  with TPase_RenderRdbms.Create(Model, FProjectRoot + PathDelim + 'templates') do
      try
        SynEdit1.Lines.Text:=Render;
      finally
        Free;
      end;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  SynEdit1.Clear;
  SynEdit1.Highlighter:=nil;
  SynEdit1.Lines.LoadFromFile(ProjectRoot + PathDelim + 'templates' + PathDelim + 'rdbms_datatype_firebird.sql');
end;

end.

