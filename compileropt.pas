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
    edtParams: TEdit;
    edtCompiler: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    dOpen: TOpenDialog;
    procedure bntOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnSelectClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure SaveUserSettings();
    procedure LoadUserSettings();
  public

  end;

var
  Form2: TForm2;

implementation
uses
  Unit1;

{$R *.lfm}

{ TForm2 }

procedure TForm2.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TForm2.btnSelectClick(Sender: TObject);
begin
  if dOpen.Execute then
   begin
     edtCompiler.Text:= dOpen.FileName;
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

procedure TForm2.SaveUserSettings();
begin
  UserSettings:= TIniFile.Create(RootDirectory + USER_SETTINGS_FILENAME);

  UserSettings.WriteString('COMPILER', 'CompilerPath', edtCompiler.Text);
  UserSettings.WriteString('COMPILER', 'Params', edtParams.Text);

  UserSettings.Destroy;
end;

procedure TForm2.LoadUserSettings();
begin
  UserSettings:= TIniFile.Create(RootDirectory + USER_SETTINGS_FILENAME);

  edtCompiler.Text:= UserSettings.ReadString('COMPILER', 'CompilerPath', '');
  edtParams.Text:= UserSettings.ReadString('COMPILER', 'Params', '');

  UserSettings.Destroy;
end;

procedure TForm2.bntOKClick(Sender: TObject);
begin
  SaveUserSettings();
  Close;
end;

end.

