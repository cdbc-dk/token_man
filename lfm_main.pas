unit lfm_main;

{$mode objfpc}{$H+}
{-$define debug}
interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  clipbrd,
  bc_strings,
  bc_datetime;

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
    function Getparameters(var aLocation: string): boolean;
    function LoadTokenFile(aFilename: string): boolean;
    function ParseTokenString(const aTokenstring: string): boolean;
    function InsertIntoClipboard(aToken: string): boolean;
  end;

var
  frmToken: TfrmToken;

implementation
const
  DefaultFilename = '/home/bc/src/git_help/token.git';

{$R *.lfm}

{ TfrmToken }

procedure TfrmToken.btnEditClick(Sender: TObject);
begin
  edtMisc.SelectAll;
  edtMisc.CopyToClipboard;
  memPaste.Lines.Add(bcDateTimeToStr(now)+' Edit text copied succesfuly to clipboard!');
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
  if Getparameters(fTokenFilename) then
  else fTokenFilename:= DefaultFilename; { run with default filename }
  LoadTokenFile(fTokenFilename);
  {$ifdef debug}
    memPaste.Lines.Add(fTokenFilename+' Loaded');
  {$endif}
end;

procedure TfrmToken.FormShow(Sender: TObject);
begin
  {$ifdef debug}
    Caption:= 'Running with default filename';
  {$endif}
end;

function TfrmToken.Getparameters(var aLocation: string): boolean;
begin
  aLocation:= '';
  Result:= false;
  if Application.ParamCount >= 2 then begin
    if Application.Params[1] = '-l' then begin
      aLocation:= Application.Params[2];
      Result:= true;
      {$ifdef debug}
        memPaste.Lines.Add('Params[2] = '+aLocation);
        memPaste.Lines.Add('Params[1] = '+Application.ParamCount.ToString);
      {$endif}
    end;
  end;
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
  memPaste.Lines.Add(bcDateTimeToStr(now)+' Token copied succesfuly to clipboard!');
  Result:= true;
end;

end.

