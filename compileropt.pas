unit CompilerOpt;

{$mode Delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, PrintersDlgs,
  IniFiles;

type

  { TForm2 }

  TForm2 = class(TForm)
    btnSelect: TButton;
    btnCancel: TButton;
    bntOK: TButton;
    btnAddParam: TButton;
    btnDeleteParam: TButton;
    btnTestParams: TButton;
    btnAddCommands: TButton;
    btnDeleteCommands: TButton;
    edtAdditionalCommands: TEdit;
    edtParams: TEdit;
    edtCompiler: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    dOpen: TOpenDialog;
    Label3: TLabel;
    lbParams: TListBox;
    lbAdditionalCommands: TListBox;
    procedure bntOKClick(Sender: TObject);
    procedure btnAddCommandsClick(Sender: TObject);
    procedure btnAddParamClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnDeleteCommandsClick(Sender: TObject);
    procedure btnDeleteParamClick(Sender: TObject);
    procedure btnSelectClick(Sender: TObject);
    procedure btnTestParamsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lbAdditionalCommandsDblClick(Sender: TObject);
    procedure lbParamsDblClick(Sender: TObject);
  private
    procedure SaveUserSettings();
    procedure LoadUserSettings();
  public
    function ChangeFlagToParam(const AParamArray: TStringArray): TStringArray;
  end;

var
  Form2: TForm2;

implementation
uses
  Unit1;

{$R *.lfm}

{ TForm2 }

function TForm2.ChangeFlagToParam(const AParamArray: TStringArray): TStringArray;
var
  i: Integer;
begin
  //Look for...
  for i:= Low(AParamArray) to High(AParamArray) do
   begin
     // $(sourcefilepath) flag
     APAramArray[i]:= StringReplace(AParamArray[i], '$(sourcefilepath)', ExtractFilePath(FileDirectory), [rfReplaceAll]);

     // $(sourcefile) flag
     APAramArray[i]:= StringReplace(AParamArray[i], '$(sourcefile)', FileDirectory, [rfReplaceAll]);

     // $(sourcefilename) flag
     APAramArray[i]:= StringReplace(AParamArray[i], '$(sourcefilename)', ChangeFileExt(ExtractFileName(FileDirectory), ''), [rfReplaceAll]);
   end;

  Result:= AParamArray;;
end;

procedure TForm2.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TForm2.btnDeleteCommandsClick(Sender: TObject);
begin
  if (lbAdditionalCommands.ItemIndex >= 0) then
   begin
     lbAdditionalCommands.Items.Delete(lbAdditionalCommands.ItemIndex);
   end;
end;

procedure TForm2.btnDeleteParamClick(Sender: TObject);
begin
  if (lbParams.ItemIndex >= 0) then
   begin
     lbParams.Items.Delete(lbParams.ItemIndex);
   end;
end;

procedure TForm2.btnSelectClick(Sender: TObject);
begin
  if dOpen.Execute then
   begin
     edtCompiler.Text:= dOpen.FileName;
   end;
end;

procedure TForm2.btnTestParamsClick(Sender: TObject);
var
  i, j: Integer;
  AParamArr: TStringArray;
  AParamsStr: String;
begin
  for i:= 0 to lbParams.Items.Count -1 do
   begin
     //Split params by space
     AParamArr:= lbParams.Items[i].Split(' ');

     AParamArr:= ChangeFlagToParam(AParamArr);

     AParamsStr:= '';

     for j:= Low(AParamArr) to High(AParamArr) do
      begin
        AParamsStr:= AParamsStr + ' ' + AParamArr[j];
      end;

     ShowMessage(edtCompiler.Text + ' ' + AParamsStr);
   end;

  for i:= 0 to lbAdditionalCommands.Items.Count -1 do
   begin
     AParamArr:= lbAdditionalCommands.Items[i].Split(' ');

     AParamArr:= ChangeFlagToParam(AParamArr);

     AParamsStr:= '';

     for j:= Low(AParamArr) to High(AParamArr) do
      begin
        AParamsStr:= AParamsStr + ' ' + AParamArr[j];
      end;

     ShowMessage(AParamsStr);
   end;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  LoadUserSettings();

  if (edtCompiler.Text = '') then
   begin
     dOpen.InitialDir:= RootDirectory;
   end
  else
   begin
     dOpen.InitialDir:= edtCompiler.Text;
   end;
end;

procedure TForm2.lbAdditionalCommandsDblClick(Sender: TObject);
begin
  if (lbAdditionalCommands.ItemIndex >= 0) then
   begin
     edtAdditionalCommands.Text:= lbAdditionalCommands.Items[lbAdditionalCommands.ItemIndex];
   end;
end;

procedure TForm2.lbParamsDblClick(Sender: TObject);
begin
  if (lbParams.ItemIndex >= 0) then
   begin
     edtParams.Text:= lbParams.Items[lbParams.ItemIndex];
   end;
end;

procedure TForm2.SaveUserSettings();
var
  i: Integer;
begin
  UserSettings:= TIniFile.Create(RootDirectory + USER_SETTINGS_FILENAME);

  UserSettings.WriteString('COMPILER', 'CompilerPath', edtCompiler.Text);
  UserSettings.WriteInteger('COMPILER', 'ParamListCount', lbParams.Items.Count);
  UserSettings.WriteInteger('COMPILER', 'CommandsCount', lbAdditionalCommands.Items.Count);

  for i:= 0 to lbParams.Items.Count -1 do
   begin
     UserSettings.WriteString('COMPILER', 'Param' + i.ToString(), lbParams.Items[i]);
   end;

  for i:= 0 to lbAdditionalCommands.Items.Count -1 do
   begin
     UserSettings.WriteString('COMPILER', 'Command' + i.ToString(), lbAdditionalCommands.Items[i]);
   end;

  UserSettings.Destroy;
end;

procedure TForm2.LoadUserSettings();
var
  i: Integer;
  AParamListCount: Integer;
  ACommandListCount: Integer;
  AParamsStr: String;
  ACommandStr: String;
begin
  UserSettings:= TIniFile.Create(RootDirectory + USER_SETTINGS_FILENAME);

  edtCompiler.Text:= UserSettings.ReadString('COMPILER', 'CompilerPath', '');
  AParamListCount:= UserSettings.ReadInteger('COMPILER', 'ParamListCount', 0);
  ACommandListCount:= UserSettings.ReadInteger('COMPILER', 'CommandsCount', 0);

  for i:= 0 to AParamListCount -1 do
   begin
     AParamsStr:= UserSettings.ReadString('COMPILER', 'Param' + i.ToString(), '');
     lbParams.Items.Add(AParamsStr);
   end;

  for i:= 0 to ACommandListCount -1 do
   begin
     ACommandStr:= UserSettings.ReadString('COMPILER', 'Command' + i.ToString(), '');
     lbAdditionalCommands.Items.Add(ACommandStr);
   end;

  UserSettings.Destroy;
end;

procedure TForm2.bntOKClick(Sender: TObject);
begin
  SaveUserSettings();
  Close;
end;

procedure TForm2.btnAddCommandsClick(Sender: TObject);
begin
  lbAdditionalCommands.Items.Add(edtAdditionalCommands.Text);
  edtAdditionalCommands.Clear;
end;

procedure TForm2.btnAddParamClick(Sender: TObject);
begin
  lbParams.Items.Add(edtParams.Text);
  edtParams.Clear;
end;

end.

