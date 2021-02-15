unit lfm_main;

{$mode objfpc}{$H+}
{-$define debug}
interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  clipbrd,
  bc_strings;

type

  { TfrmToken }

  TfrmToken = class(TForm)
    btnEdit: TButton;
    btnToken: TButton;
    edtMisc: TEdit;
    memPaste: TMemo;
    procedure btnEditClick(Sender: TObject);
    procedure btnTokenClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  protected
    fToken: string;
    fTokenString: string;
    fTokenFilename: string;
    fTokenFile: TStringList;
  public
    function LoadTokenFile(aFilename: string): boolean;
    function ParseTokenString(const aTokenstring: string): boolean;
    function InsertIntoClipboard(aToken: string): boolean;
  end;

var
  frmToken: TfrmToken;

implementation
const
  DefaultFilename = '/home/bc/src/git_help/token.git';
//const
//  Token = 'cab6370477f1e1fd542e98a86727254a091ccb2e';
//  The quick brown fox jumped over the lazy dog
//  The quick brown fox jumped over the lazy dog
{$R *.lfm}

{ TfrmToken }

procedure TfrmToken.btnEditClick(Sender: TObject);
begin
  edtMisc.SelectAll;
  edtMisc.CopyToClipboard;
end;

procedure TfrmToken.btnTokenClick(Sender: TObject);
begin
  if fTokenString <> '' then ParseTokenString(fTokenString);
  if InsertIntoClipboard(fToken) then begin
    {$ifdef debug}
    memPaste.Lines.Add('Token inserted into Clipboard');
    {$endif}
  end;
//  Clipboard.AsText:= Token;
end;

procedure TfrmToken.FormCreate(Sender: TObject);
begin
  { start out with default filename }
  fTokenFilename:= DefaultFilename;
  LoadTokenFile(fTokenFilename);
end;

procedure TfrmToken.FormShow(Sender: TObject);
begin
  {$ifdef debug}
  Caption:= 'Running with default filename';
  {$endif}
end;

function TfrmToken.LoadTokenFile(aFilename: string): boolean;
var
  Idx: integer;
begin
  Result:= false;
  fTokenFile:= TStringList.Create;
  try
    if aFilename <> '' then begin
      fTokenFile.LoadFromFile(aFilename);
    end;
    { linear search }
    for Idx:= 0 to fTokenFile.Count-1 do if pos('Token', fTokenFile[Idx]) > 0 then begin
      fTokenString:= fTokenFile[Idx];
      break;
    end;
    {$ifdef debug}
    memPaste.Lines.Add(aFilename+' loaded');
    memPaste.Lines.Add('TokenString = '+fTokenString);
    {$endif}
    Result:= true;
  finally FreeAndNil(fTokenFile); end;
end;

function TfrmToken.ParseTokenString(const aTokenstring: string): boolean;
begin
  Result:= false;
  if aTokenstring <> '' then begin
    fToken:= bcGetFieldToken(2,aTokenstring,':');
    fToken:= trim(fToken);
    {$ifdef debug}
    memPaste.Lines.Add('Token -> '+fToken);
    {$endif}
    Result:= true;
  end;
end;

function TfrmToken.InsertIntoClipboard(aToken: string): boolean;
begin
  Result:= false;
  { clipboard.astext calls clear }
  {$ifdef debug}
  memPaste.Lines.Add('aToken -> '+aToken);
  {$endif}
  Clipboard.AsText:= aToken; { works only in gui?!? }
  memPaste.Lines.Add('Token copied succesfuly to clipboard!');
  Result:= true;
end;

end.

