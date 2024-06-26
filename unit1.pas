unit Unit1;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ComCtrls,
  SynEdit, SynHighlighterPas, SynHighlighterXML, SynHighlighterCpp,
  SynHighlighterMulti, SynHighlighterAny, LCLType, IniFiles, process;


Const
  APP_VER   = 'ver 1.0';
  APP_TITLE = 'DevNotepad by Konrad Kluczewski';

  //User settings filename
  USER_SETTINGS_FILENAME = 'Settings.ini';

type

  { TForm1 }

  TForm1 = class(TForm)
    dFont: TFontDialog;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    miMessages: TMenuItem;
    Separator8: TMenuItem;
    pmSelectAll: TMenuItem;
    pmSelectLine: TMenuItem;
    Separator7: TMenuItem;
    Separator6: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    pmCut: TMenuItem;
    pmCopy: TMenuItem;
    pmPaste: TMenuItem;
    miCompile: TMenuItem;
    miRun: TMenuItem;
    miCompilerOptions: TMenuItem;
    miSelectFont: TMenuItem;
    pmTextField: TPopupMenu;
    Separator5: TMenuItem;
    miSynHi: TMenuItem;
    miSelectLine: TMenuItem;
    MenuItem2: TMenuItem;
    miUndo: TMenuItem;
    miRedo: TMenuItem;
    miCut: TMenuItem;
    miCopy: TMenuItem;
    miPaste: TMenuItem;
    miSelectAll: TMenuItem;
    Separator4: TMenuItem;
    Separator3: TMenuItem;
    Separator2: TMenuItem;
    miSave: TMenuItem;
    miSaveAs: TMenuItem;
    miOpen: TMenuItem;
    miExit: TMenuItem;
    miNew: TMenuItem;
    MenuItem4: TMenuItem;
    miAbout: TMenuItem;
    dOpen: TOpenDialog;
    dSave: TSaveDialog;
    Separator1: TMenuItem;
    sbBar: TStatusBar;
    seTextField: TSynEdit;
    SynAnySyn1: TSynAnySyn;
    SynCppSyn1: TSynCppSyn;
    SynPasSyn1: TSynPasSyn;
    SynXMLSyn1: TSynXMLSyn;
    procedure FormChangeBounds(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure miCompileClick(Sender: TObject);
    procedure miCompilerOptionsClick(Sender: TObject);
    procedure miCopyClick(Sender: TObject);
    procedure miCutClick(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure miMessagesClick(Sender: TObject);
    procedure miNewClick(Sender: TObject);
    procedure miAboutClick(Sender: TObject);
    procedure miOpenClick(Sender: TObject);
    procedure miPasteClick(Sender: TObject);
    procedure miRedoClick(Sender: TObject);
    procedure miSaveAsClick(Sender: TObject);
    procedure miSaveClick(Sender: TObject);
    procedure miSelectAllClick(Sender: TObject);
    procedure miSelectFontClick(Sender: TObject);
    procedure miSelectLineClick(Sender: TObject);
    procedure miSynHiClick(Sender: TObject);
    procedure miUndoClick(Sender: TObject);
    procedure pmCopyClick(Sender: TObject);
    procedure pmCutClick(Sender: TObject);
    procedure pmPasteClick(Sender: TObject);
    procedure pmSelectAllClick(Sender: TObject);
    procedure pmSelectLineClick(Sender: TObject);
    procedure seTextFieldChange(Sender: TObject);
    procedure seTextFieldClick(Sender: TObject);
  private
     procedure SetupStatusCursor();
     procedure SetupHighlighter();
     procedure LoadUserSettings();
     procedure SaveUserSettings();
  public

  end;

const
  About = APP_TITLE + #10#13 +
          APP_VER + #10#13 +
          'Made with Lazarus 3.2';

  msgWRN01 = 'Document was modified!' + #10#13 +
             'Would you like to save this document now?';

  msgERR01 = 'You must first complete compiler path and params.';

var
  //Main form
  Form1         : TForm1;

  //User settings
  UserSettings  : TIniFile;

  //Root directory
  RootDirectory : String;

  //Document file directory
  FileDirectory : String;
  //File extansion
  FileExtansion : String;
  //Is modified
  Modified      : Boolean;


implementation
uses
  CompilerOpt, OutputMessages;

{$R *.lfm}

{ TForm1 }

procedure TForm1.LoadUserSettings();
begin
  UserSettings:= TIniFile.Create(RootDirectory + USER_SETTINGS_FILENAME);

  if FileDirectory = '' then
   begin
     dOpen.InitialDir:= UserSettings.ReadString('SETTINGS', 'LastDir', '');
     dSave.InitialDir:= UserSettings.ReadString('SETTINGS', 'LastDir', '');
   end;

  miSynHi.Checked:= UserSettings.ReadBool('SETTINGS', 'SyntaxHighlighting', False);

  Left:= UserSettings.ReadInteger('WINDOW', 'Left', 256);
  Top:= UserSettings.ReadInteger('WINDOW', 'Top', 256);
  Width:= UserSettings.ReadInteger('WINDOW', 'Width', 1000);
  Height:= UserSettings.ReadInteger('WINDOW', 'Height', 600);

  seTextField.Font.Name:= UserSettings.ReadString('FONT', 'FontName', '');
  seTextField.Font.Size:= UserSettings.ReadInteger('FONT', 'FontSize', 10);
  seTextField.Font.Color:= UserSettings.ReadInteger('FONT', 'FontColor', clSilver);
  seTextField.Font.Bold:= UserSettings.ReadBool('FONT', 'FontBold', False);
  seTextField.Font.Italic:= UserSettings.ReadBool('FONT', 'FontItalic', False);
  seTextField.Font.Underline:= UserSettings.ReadBool('FONT', 'FontUnderline', False);

  UserSettings.Destroy();
end;

procedure TForm1.SaveUserSettings();
begin
  UserSettings:= TIniFile.Create(RootDirectory + USER_SETTINGS_FILENAME);

  UserSettings.WriteString('SETTINGS', 'LastDir', ExtractFilePath(FileDirectory));
  UserSettings.WriteBool('SETTINGS', 'SyntaxHighlighting', miSynHi.Checked);

  UserSettings.WriteInteger('WINDOW', 'Left', Left);
  UserSettings.WriteInteger('WINDOW', 'Top', Top);
  UserSettings.WriteInteger('WINDOW', 'Width', Width);
  UserSettings.WriteInteger('WINDOW', 'Height', Height);

  UserSettings.WriteString('FONT', 'FontName', seTextField.Font.Name);
  UserSettings.WriteInteger('FONT', 'FontSize', seTextField.Font.Size);
  UserSettings.WriteInteger('FONT', 'FontColor', seTextField.Font.Color);
  UserSettings.WriteBool('FONT', 'FontBold', seTextField.Font.Bold);
  UserSettings.WriteBool('FONT', 'FontItalic', seTextField.Font.Italic);
  UserSettings.WriteBool('FONT', 'FontUnderline', seTextField.Font.Underline);

  UserSettings.Destroy();
end;

procedure TForm1.SetupStatusCursor();
begin
  sbBar.Panels[0].Text:= Format('[%d: %d]', [seTextField.CaretY, seTextField.CaretX]);
end;

procedure TForm1.SetupHighlighter();
begin
  if (UpperCase(FileExtansion) = '.PAS') or (UpperCase(FileExtansion) = '.DPR') or (UpperCase(FileExtansion) = '.LPR') then
   seTextField.Highlighter:= SynPasSyn1;

  if (UpperCase(FileExtansion) = '.CS') or (UpperCase(FileExtansion) = '.SLN') then
   seTextField.Highlighter:= SynCppSyn1;

  if (UpperCase(FileExtansion) = '.XML') then
   seTextField.Highlighter:= SynXMLSyn1;

  if (UpperCase(FileExtansion) = '.ASM') then
   seTextField.Highlighter:= SynAnySyn1;

  miSynHi.Checked:= True;

  SaveUserSettings();
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Caption:= APP_TITLE + ' ' + APP_VER;
  FileDirectory:= '';
  Modified:= False;

  RootDirectory:= ExtractFilePath(Application.ExeName);
  if FileExists(RootDirectory + USER_SETTINGS_FILENAME) then
   begin
     LoadUserSettings();
   end;
end;

procedure TForm1.FormChangeBounds(Sender: TObject);
begin
  SaveUserSettings();
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SaveUserSettings();
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  SaveUserSettings();
end;

procedure TForm1.miCompileClick(Sender: TObject);
var
  i, j: Integer;
  Ini: TIniFile;
  ACompilerPath: String;
  AParamsCount: Integer;
  AParams: TStringArray;
  ASingleParamArr: TStringArray;
  ACommandsCount: Integer;
  ACommands: TStringArray;
  ASingleCommandArr: TStringArray;
  AOutMsg: TStringList;
  AErrMsg: TStringList;
  hProcess: TProcess;
  bCompilingError: Boolean;
begin
  miSave.Click;
  Form3.lbOutput.Clear;
  miMessages.Click;

  Ini:= TIniFile.Create(RootDirectory + USER_SETTINGS_FILENAME);

  ACompilerPath:= Ini.ReadString('COMPILER', 'CompilerPath', '');

  AParamsCount:= Ini.ReadInteger('COMPILER', 'ParamListCount', 0);

  if ((ACompilerPath = '') or (AParamsCount = 0)) then
   begin
     MessageDlg('Error', msgERR01, mtError, [mbOK], 0);

     Ini.Destroy();

     Exit;
   end;

  ACommandsCount:= Ini.ReadInteger('COMPILER', 'CommandsCount', 0);

  SetLength(AParams, AParamsCount);
  SetLength(ACommands, ACommandsCount);

  bCompilingError:= False;

  for i:= 0 to AParamsCount -1 do
   begin
     AParams[i]:= Ini.ReadString('COMPILER', 'Param' + i.ToString(), '');
     AParams:= Form2.ChangeFlagToParam(AParams);
     ASingleParamArr:= AParams[i].Split(' ');

     hProcess:= TProcess.Create(NIL);
     AOutMsg:= TStringList.Create();
     AErrMsg:= TStringList.Create();

     hProcess.Executable:= ACompilerPath;

     for j:= Low(ASingleParamArr) to High(ASingleParamArr) do
      begin
        hProcess.Parameters.Add(ASingleParamArr[j]);
      end;

     hProcess.Options:= hProcess.Options + [poWaitOnExit, poUsePipes];
     hProcess.Execute;

     AOutMsg.Add('stdout:');
     AOutMsg.LoadFromStream(hProcess.Output);
     AErrMsg.Add('stderr:');
     AErrMsg.LoadFromStream(hProcess.StdErr);

     if (AErrMsg.Text = '') then
      begin
        if (AOutMsg.Text <> '') then
         Form3.lbOutput.Items.Append(AOutMsg.Text);
      end
     else
      begin
        bCompilingError:= True;
        Form3.lbOutput.Items.Append(AOutMsg.Text + AErrMsg.Text);
      end;

     AErrMsg.Destroy();
     AOutMsg.Destroy();
     hProcess.Destroy();
   end;

  if (bCompilingError) then
   Exit;

  for i:= 0 to ACommandsCount -1 do
   begin
     ACommands[i]:= Ini.ReadString('COMPILER', 'Command' + i.ToString(), '');
     ACommands:= Form2.ChangeFlagToParam(ACommands);
     ASingleCommandArr:= ACommands[i].Split(' ');

     hProcess:= TProcess.Create(NIL);
     AOutMsg:= TStringList.Create();
     AErrMsg:= TStringList.Create();

     hProcess.Executable:= ASingleCommandArr[0];

     for j:= 1 to High(ASingleCommandArr) do
      begin
        hProcess.Parameters.Add(ASingleCommandArr[j]);
      end;

     hProcess.Options:= hProcess.Options + [poWaitOnExit, poUsePipes];
     hProcess.Execute;

     AOutMsg.Add('stdout:');
     AOutMsg.LoadFromStream(hProcess.Output);
     AErrMsg.Add('stderr:');
     AErrMsg.LoadFromStream(hProcess.StdErr);

     if (AErrMsg.Text = '') then
      begin
        if (AOutMsg.Text <> '') then
         Form3.lbOutput.Items.Append(AOutMsg.Text);
      end
     else
      begin
        bCompilingError:= True;
        Form3.lbOutput.Items.Append(AOutMsg.Text + AErrMsg.Text);
      end;

     AErrMsg.Destroy();
     AOutMsg.Destroy();
     hProcess.Destroy();
   end;

  miMessages.Click;

  Ini.Destroy();
end;

procedure TForm1.miCompilerOptionsClick(Sender: TObject);
begin
  Form2.ShowModal;
end;

procedure TForm1.miCopyClick(Sender: TObject);
begin
  seTextField.CopyToClipboard;
end;

procedure TForm1.miCutClick(Sender: TObject);
begin
  seTextField.CutToClipboard;
end;

procedure TForm1.miExitClick(Sender: TObject);
begin
  if (Modified) then
   begin
     Case MessageDlg('Warning', msgWRN01, mtWarning, [mbYes, mbNo, mbCancel], 0) Of
       mrYes:
         begin
           if dSave.Execute then
            begin
              FileDirectory:= dSave.FileName;
              seTextField.Lines.SaveToFile(FileDirectory);

              Application.Terminate;
            end;
         end;

       mrNo:
         begin
           Application.Terminate;
         end;

       mrCancel:
         begin
           Exit;
         end;
     end;
   end
  else
   begin
     Application.Terminate;
   end;
end;

procedure TForm1.miMessagesClick(Sender: TObject);
begin
  Form3.Show;
  Form3.Top:= Top + Height;
  Form3.Left:= Left;
  Form3.Width:= Width;
end;

procedure TForm1.miNewClick(Sender: TObject);
begin
  if (Modified) then
   begin
     Case MessageDlg('Warning', msgWRN01, mtWarning, [mbYes, mbNo, mbCancel], 0) Of
       mrYes:
         begin
           if dSave.Execute then
            begin
              FileDirectory:= dSave.FileName;
              seTextField.Lines.SaveToFile(FileDirectory);
              Modified:= False;
              FileExtansion:= '';

              seTextField.Clear;

              SetupStatusCursor();
              sbBar.Panels[1].Text:= '';
            end;
         end;

       mrNo:
         begin
           seTextField.Clear;

           FileDirectory:= '';
           FileExtansion:= '';
           Modified:= False;

           SetupStatusCursor();
           sbBar.Panels[1].Text:= '';
         end;

       mrCancel:
         begin
           Exit;
         end;
     end;
   end
  else
   begin
     seTextField.Clear;

     SetupStatusCursor();
   end;
end;

procedure TForm1.miAboutClick(Sender: TObject);
begin
  MessageDlg('Notepad', About, mtInformation, [mbOk], 'Help keyword');
end;

procedure TForm1.miOpenClick(Sender: TObject);
begin
  if (Modified) then
   begin
     Case MessageDlg('Warning', msgWRN01, mtWarning, [mbYes, mbNo, mbCancel], 0) Of
       mrYes:
         begin
           if dSave.Execute then
            begin
              FileDirectory:= dSave.FileName;
              seTextField.Lines.SaveToFile(FileDirectory);

              if dOpen.Execute then
               begin
                 FileDirectory:= dOpen.FileName;
                 FileExtansion:= ExtractFileExt(FileDirectory);
                 SetupHighlighter();
                 seTextField.Lines.LoadFromFile(FileDirectory);

                 SaveUserSettings();
               end;

              Modified:= False;
              SetupStatusCursor();
              sbBar.Panels[1].Text:= '';
              sbBar.Panels[2].Text:= FileDirectory;
            end;
         end;

       mrNo:
         begin
           if dOpen.Execute then
            begin
              FileDirectory:= dOpen.FileName;
              FileExtansion:= ExtractFileExt(FileDirectory);
              SetupHighlighter();
              seTextField.Lines.LoadFromFile(FileDirectory);

              Modified:= False;
              SetupStatusCursor();
              sbBar.Panels[1].Text:= '';
              sbBar.Panels[2].Text:= FileDirectory;

              SaveUserSettings();
            end;
         end;

       mrCancel:
         begin
           Exit;
         end;
     end;
   end
  else
   begin
     if dOpen.Execute then
      begin
        FileDirectory:= dOpen.FileName;
        FileExtansion:= ExtractFileExt(FileDirectory);
        SetupHighlighter();
        seTextField.Lines.LoadFromFile(FileDirectory);

        Modified:= False;
        SetupStatusCursor();
        sbBar.Panels[1].Text:= '';
        sbBar.Panels[2].Text:= FileDirectory;

        SaveUserSettings();
      end;
   end;
end;

procedure TForm1.miPasteClick(Sender: TObject);
begin
  seTextField.PasteFromClipboard();
end;

procedure TForm1.miRedoClick(Sender: TObject);
begin
  seTextField.Redo;
end;

procedure TForm1.miSaveAsClick(Sender: TObject);
begin
  if dSave.Execute then
   begin
     FileDirectory:= dSave.FileName;
     FileExtansion:= ExtractFileExt(FileDirectory);
     SetupHighlighter();
     seTextField.Lines.SaveToFile(FileDirectory);

     Modified:= False;
     sbBar.Panels[1].Text:= '';
     sbBar.Panels[2].Text:= FileDirectory;

     SaveUserSettings();
   end;
end;

procedure TForm1.miSaveClick(Sender: TObject);
begin
  if FileDirectory = '' then
   begin
     if dSave.Execute then
      begin
        FileDirectory:= dSave.FileName;
        FileExtansion:= ExtractFileExt(FileDirectory);
        SetupHighlighter();
        seTextField.Lines.SaveToFile(FileDirectory);

        Modified:= False;
        sbBar.Panels[1].Text:= '';
        sbBar.Panels[2].Text:= FileDirectory;

        SaveUserSettings();
      end;
   end
  else
   begin
     seTextField.Lines.SaveToFile(FileDirectory);

     Modified:= False;

     sbBar.Panels[1].Text:= '';
   end;
end;

procedure TForm1.miSelectAllClick(Sender: TObject);
begin
  seTextField.SelectAll;
end;

procedure TForm1.miSelectFontClick(Sender: TObject);
begin
  dFont.Font:= seTextField.Font;

  if dFont.Execute then
   begin
     seTextField.Font:= dFont.Font;

     SaveUserSettings();
   end;
end;

procedure TForm1.miSelectLineClick(Sender: TObject);
begin
  seTextField.SelectLine();
end;

procedure TForm1.miSynHiClick(Sender: TObject);
begin
  miSynHi.Checked:= not miSynHi.Checked;

  SynPasSyn1.Enabled:= miSynHi.Checked;
  SynCppSyn1.Enabled:= miSynHi.Checked;
  SynXMLSyn1.Enabled:= miSynHi.Checked;
  SynAnySyn1.Enabled:= miSynHi.Checked;

  if (not miSynHi.Checked) then
   begin
     seTextField.Highlighter:= Nil;
   end
  else
   begin
     SetupHighlighter();
   end;

  SaveUserSettings();
end;

procedure TForm1.miUndoClick(Sender: TObject);
begin
  seTextField.Undo;
end;

procedure TForm1.pmCopyClick(Sender: TObject);
begin
  miCopy.Click;
end;

procedure TForm1.pmCutClick(Sender: TObject);
begin
  miCut.Click;
end;

procedure TForm1.pmPasteClick(Sender: TObject);
begin
  miPaste.Click;
end;

procedure TForm1.pmSelectAllClick(Sender: TObject);
begin
  miSelectAll.Click;
end;

procedure TForm1.pmSelectLineClick(Sender: TObject);
begin
  miSelectLine.Click;
end;

procedure TForm1.seTextFieldChange(Sender: TObject);
begin
  SetupStatusCursor();
  sbBar.Panels[1].Text:= 'Modified';
  Modified:= True;
end;

procedure TForm1.seTextFieldClick(Sender: TObject);
begin
  SetupStatusCursor();
end;

end.

