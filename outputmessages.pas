unit OutputMessages;

{$mode Delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus, Clipbrd;

type

  { TForm3 }

  TForm3 = class(TForm)
    lbOutput: TListBox;
    pmCopy: TMenuItem;
    pmCopyAll: TMenuItem;
    pmClear: TMenuItem;
    Separator1: TMenuItem;
    pmMessages: TPopupMenu;
    procedure pmClearClick(Sender: TObject);
    procedure pmCopyAllClick(Sender: TObject);
    procedure pmCopyClick(Sender: TObject);
  private

  public

  end;

var
  Form3: TForm3;

implementation

{$R *.lfm}

{ TForm3 }

procedure TForm3.pmClearClick(Sender: TObject);
begin
  lbOutput.Clear;
end;

procedure TForm3.pmCopyAllClick(Sender: TObject);
begin
  Clipboard.AsText:= lbOutput.Items.Text;
end;

procedure TForm3.pmCopyClick(Sender: TObject);
begin
  if (lbOutput.ItemIndex >= 0) then
  begin
    Clipboard.AsText:= lbOutput.Items[lbOutput.ItemIndex];
  end;
end;

end.

