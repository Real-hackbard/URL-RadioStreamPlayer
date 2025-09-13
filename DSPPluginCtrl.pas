
unit DSPPluginCtrl;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, StdCtrls;

type
  TDSPControlForm = class(TForm)
    Label1: TLabel;
    Label3: TLabel;
    Label2: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    PluginList: TListBox;
    btnConfigure: TButton;
    btnStart: TButton;
    Descript: TEdit;
    btnStop: TButton;
    DSPModules: TStringGrid;
    btnClose: TButton;
    procedure FormShow(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure PluginListClick(Sender: TObject);
    procedure btnConfigureClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    procedure GetPluginInfo;
  public
    { Public declarations }
  end;

var
  DSPControlForm: TDSPControlForm;

implementation

{$R *.DFM}

uses
   ioPlug, PluginCtrl, BassTest;

var
   DSPmod : TDSPmod;
   IndexNum,
   NumDSPmod : integer;


procedure TDSPControlForm.GetPluginInfo;
var
   DSP_Plugin : string;
   DLLHandle : THandle;
   getDSPHeader2 : function : pointer; stdcall;
   DSPheader : PWinampDSPHeader;
   i : integer;
begin
   NumDSPmod := 0;
   if DSPModules.RowCount > 0 then
      DSPModules.Cells[0, 0] := '';
   DSPModules.RowCount := 0;
   Descript.Text := '';

   IndexNum := PluginList.ItemIndex;
   DSP_Plugin := GetProgDir + 'Plugins\' + PluginList.Items[IndexNum];

   if DSP_Plugin = GetLoadedDSPDLL then   // LoadedDSPDLL = Active DSP module
   begin
      DSPheader := getDSPHeader;
      if DSPHeader = nil then
         exit;
   end else
   begin
      DLLHandle := LoadLibrary(pchar(DSP_Plugin));
      if (DLLHandle = 0) then
         exit;

      getDSPHeader2 := GetProcAddress(DLLHandle, 'winampDSPGetHeader2');
      if @getDSPHeader2 = nil then
      begin
         FreeLibrary(DLLHandle);
         exit;
      end;

      DSPheader := getDSPHeader2;
      if DSPHeader = nil then
      begin
         FreeLibrary(DLLHandle);
         exit;
      end;
   end;

   for i := 0 to (maxDSPmodNum - 1) do
   begin
      DSPmod[i] := DSPheader.getModule(i);
      if DSPmod[i] <> nil then
      begin
         DSPModules.Cells[0, i] := DSPmod[i].description;
         inc(NumDSPmod);
      end else
         break;
   end;

//   LoadDSPModule(DSP_Plugin, DSPheader, DSPmod, NumDSPmod, 0);

   DSPModules.RowCount := NumDSPmod;
   if NumDSPmod > 0 then
      Descript.Text := string(DSPheader^.description)

//   for i := 0 to (NumDSPmod - 1) do
//      DSPModules.Cells[0, i] := DSPmod[i].description;

//   if DSP_Plugin <> LoadedDSPDLL then
//      FreeLibrary(DLLHandle);
end;


procedure TDSPControlForm.FormShow(Sender: TObject);
var
   SearchRec : TSearchRec;
begin
   IndexNum := -1;
   NumDSPmod := 0;
   PluginList.Clear;

   if FindFirst(GetProgDir + 'Plugins\DSP_*.dll', 0, SearchRec) = 0 then
   begin
      PluginList.Items.Add(SearchRec.Name);
      while FindNext(SearchRec) = 0 do
         PluginList.Items.Add(SearchRec.Name);
   end;
   FindClose(SearchRec);

   if PluginList.Items.Count > 0 then
   begin
      PluginList.ItemIndex := 0;
      GetPluginInfo;
   end else
   begin
      Descript.Text := '';
      DSPModules.RowCount := 0;
   end;

end;

procedure TDSPControlForm.btnCloseClick(Sender: TObject);
begin
   Close;
end;

procedure TDSPControlForm.PluginListClick(Sender: TObject);
begin
   if PluginList.ItemIndex = IndexNum then
      exit;

   UnloadDSPModule;
   GetPluginInfo;
end;

procedure TDSPControlForm.btnConfigureClick(Sender: TObject);
var
   NewSelection : TGridRect;
begin
   if NumDSPmod = 0 then
   begin
      Application.MessageBox('Invalid plug-in.', 'Error', MB_OK + MB_ICONERROR);
      exit;
   end;

   NewSelection := DSPModules.Selection;
   if NewSelection.Top > (NumDSPmod - 1) then
      exit;

   DSPmod[NewSelection.Top].Config(DSPmod[NewSelection.Top]);

end;

procedure TDSPControlForm.btnStartClick(Sender: TObject);
begin
   if NumDSPmod = 0 then
   begin
      Application.MessageBox('Invalid plug-in.', 'Error', MB_OK + MB_ICONERROR);
      exit;
   end;

   BassPlayer1.RunDSPPlugin(GetProgDir + 'Plugins\' + PluginList.Items[IndexNum],
                                DSPModules.Selection.Top{, MainForm.Handle});
end;

procedure TDSPControlForm.btnStopClick(Sender: TObject);
begin
   BassPlayer1.QuitDSPPlugin;
end;

procedure TDSPControlForm.FormDestroy(Sender: TObject);
begin
   UnloadDSPModule;
end;

end.
