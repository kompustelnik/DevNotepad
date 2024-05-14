unit Unit1;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ComCtrls,
  SynEdit, SynHighlighterPas, SynHighlighterXML, SynHighlighterCpp, LCLType,
  IniFiles;


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
    MenuItem3: TMenuItem;
    miSelectFont: TMenuItem;
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
    SynCppSyn1: TSynCppSyn;
    SynPasSyn1: TSynPasSyn;
    SynXMLSyn1: TSynXMLSyn;
    procedure FormChangeBounds(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure miCopyClick(Sender: TObject);
    procedure miCutClick(Sender: TObject);
    procedure miExitClick(Sender: TObject);
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
  sbBar.Panels[0].Text:= Format('[%d: %d]', [seTextField.CaretX, seTextField.CaretY]);
end;

procedure TForm1.SetupHighlighter();
begin
  if (UpperCase(FileExtansion) = '.PAS') or (UpperCase(FileExtansion) = '.DPR') or (UpperCase(FileExtansion) = '.LPR') then
   seTextField.Highlighter:= SynPasSyn1;

  if (UpperCase(FileExtansion) = '.CS') or (UpperCase(FileExtansion) = '.SLN') then
   seTextField.Highlighter:= SynCppSyn1;

  if (UpperCase(FileExtansion) = '.XML') then
   seTextField.Highlighter:= SynXMLSyn1;

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

