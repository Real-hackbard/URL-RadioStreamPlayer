
unit VisPluginCtrl;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, StdCtrls, ComCtrls, ExtCtrls, PluginCtrl;

type
  TVisControlForm = class(TForm)
    PluginList: TListBox;
    btnConfigure: TButton;
    Label3: TLabel;
    btnStart: TButton;
    Descript: TEdit;
    Label2: TLabel;
    btnStop: TButton;
    VisModules: TStringGrid;
    Label6: TLabel;
    VisScaleBar: TTrackBar;
    Label10: TLabel;
    btnClose: TButton;
    Label5: TLabel;
    Label1: TLabel;
    UseWinampLook: TCheckBox;

    procedure FormShow(Sender: TObject);
    procedure PluginListClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnConfigureClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure cbSyncVisClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure VisScaleBarChange(Sender: TObject);
    procedure UseWinampLookClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PluginListDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure VisModulesDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
  private
    { Private declarations }
    procedure GetPluginInfo;
  public
    { Public declarations }
    procedure SetVisWindowAttr(VisWindowAttr : TVisPluginInfo); // * New at Ver 1.23
  end;

var
  VisControlForm: TVisControlForm;

implementation

{$R *.DFM}

uses
   ioPlug, BassTest;

var
   Vismod : TVismod;
   CurIndexNum,
   NumVismod : integer;
   CurVisWindowAttr : TVisPluginInfo;
 //  WaitingPrevVisStop : boolean = false;

   IsShownConfigWindow : boolean;


procedure TVisControlForm.SetVisWindowAttr(VisWindowAttr : TVisPluginInfo);
begin
   CurVisWindowAttr := VisWindowAttr;

   PluginList.Repaint;
   VisModules.Repaint;

  // See comment in the procedure btnStartClick
  { if CurVisWindowAttr.VisHandle <> 0 then
      WaitingPrevVisStop := false
   else
      if WaitingPrevVisStop then
         btnStartClick(Self); }
end;

procedure TVisControlForm.GetPluginInfo;
var
   Vis_Plugin : string;
   Visheader : PWinampVisHeader;
   i : integer;
begin
   CurIndexNum := PluginList.ItemIndex;
   Vis_Plugin := GetProgDir + 'Plugins\' + PluginList.Items[CurIndexNum];
   LoadVisModule(Vis_Plugin, Visheader, Vismod, NumVismod, Form1.Handle);

   if NumVismod = 0 then
      exit;

   Descript.Text := string(Visheader^.description);

   VisModules.RowCount := NumVismod;
   for i := 0 to (NumVismod - 1) do
      VisModules.Cells[0, i] := Vismod[i].description;

end;

procedure TVisControlForm.FormShow(Sender: TObject);
var
   SearchRec : TSearchRec;
begin
   CurIndexNum := -1;
   NumVismod := 0;
   PluginList.Clear;

   if FindFirst(GetProgDir + 'Plugins\vis_*.dll', 0, SearchRec) = 0 then
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
      VisModules.RowCount := 0;
   end;

  // cbSyncVis.Checked := BassPlayer1.SyncVisWindow;
   UseWinampLook.Checked := BassPlayer1.UseVisDrawer;
   VisScaleBar.Position := (BassPlayer1.VisScale - 256) div 24;

end;

procedure TVisControlForm.PluginListClick(Sender: TObject);
begin
   if PluginList.ItemIndex = CurIndexNum then
      exit;

   UnloadVisModule;
   GetPluginInfo;
end;

procedure TVisControlForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   if IsShownConfigWindow then
   begin
      Application.MessageBox('Close plug-in''s Configuration window first.', 'Confirm',
                              MB_OK + MB_ICONINFORMATION);
      Action := caNone;
      exit;
   end;

   UnloadVisModule;
end;

procedure TVisControlForm.btnConfigureClick(Sender: TObject);
var
   NewSelection : TGridRect;
   Vis_Plugin : string;
   Visheader : PWinampVisHeader;
   SelectedIndexNum : integer;
begin
   if IsShownConfigWindow then
      exit;

 // Reload vis plug-in if it is released.
   if NumVismod = 0 then
      if PluginList.Items.Count > 0 then
      begin
         SelectedIndexNum := PluginList.ItemIndex;
         Vis_Plugin := GetProgDir + 'Plugins\' + PluginList.Items[SelectedIndexNum];
         LoadVisModule(Vis_Plugin, Visheader, Vismod, NumVismod, Form1.Handle);
         if NumVismod = 0 then
            exit;
      end else
        exit;

   NewSelection := VisModules.Selection;
   if NewSelection.Top > (NumVismod - 1) then
      exit;

   IsShownConfigWindow := true;
   PluginList.Enabled := false;
   Vismod[NewSelection.Top].Config(Vismod[NewSelection.Top]);
   IsShownConfigWindow := false;
   PluginList.Enabled := true;
end;

procedure TVisControlForm.btnStartClick(Sender: TObject);
var
   Vis_Plugin : string;
   SelectedIndexNum : integer;
begin
   if IsShownConfigWindow then
      exit;

 // The vis plug-in should be loaded by the seperate thread which runs that.
 // Some vis plug-in gets main program's thread id (i.e., not the driver of vis plug-in's
 // thread id) if they are not unloaded before running vis plug-in even though they are
 // reloaded by the driver of vis plug-in.
 // In such case it is difficult to know whether vis window is created.

   if PluginList.Items.Count = 0 then    // if no vis plug-in
      exit;

   SelectedIndexNum := PluginList.ItemIndex;
   Vis_Plugin := GetProgDir + 'Plugins\' + PluginList.Items[SelectedIndexNum];
   if Vis_Plugin = string(CurVisWindowAttr.PluginPath) then  // Is currently running vis plug-in ?
      if VisModules.Selection.Top = CurVisWindowAttr.ModNo then
         exit;

   UnloadVisModule;
   NumVismod := 0;

  // TBASSPlayer is modified to enforce stability at starting a vis plug-in, while other
  // plug-in is running.  So, following code is not necessary and left only for reference.
  { if CurVisWindowAttr.VisHandle <> 0 then   // Visualization is active ?
   begin
    // call BassPlayer1.RunVisPlugin indirectly if vis plug-in is running, for stability.
    // procedure btnStartClick is re-called in procedure SetVisWindowAttr at detecting the
    // the signal of closing previous vis plug-in.
      WaitingPrevVisStop := true;
      BassPlayer1.QuitVisPlugin;
      exit;
   end; }

   BassPlayer1.RunVisPlugin(GetProgDir + 'Plugins\' + PluginList.Items[SelectedIndexNum],
                                VisModules.Selection.Top);
end;

procedure TVisControlForm.btnStopClick(Sender: TObject);
begin
   if CurVisWindowAttr.VisHandle = 0 then   // Visualization is not active ?
      exit;

   BassPlayer1.QuitVisPlugin;
end;

procedure TVisControlForm.cbSyncVisClick(Sender: TObject);
begin
 //  BassPlayer1.SyncVisWindow := cbSyncVis.Checked;
end;

procedure TVisControlForm.VisScaleBarChange(Sender: TObject);
begin
   BassPlayer1.VisScale := VisScaleBar.Position * 24 + 256;
end;

procedure TVisControlForm.btnCloseClick(Sender: TObject);
begin
   Close;
end;

procedure TVisControlForm.UseWinampLookClick(Sender: TObject);
begin
   if UseWinampLook.Checked then
      BassPlayer1.UseVisDrawer := true
   else
      BassPlayer1.UseVisDrawer := false;
end;

procedure TVisControlForm.FormCreate(Sender: TObject);
begin
   CurVisWindowAttr.VisHandle := 0;
end;

procedure TVisControlForm.PluginListDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
   Vis_Plugin : string;
   IsRunningPlugin : boolean;
begin
   Vis_Plugin := GetProgDir + 'Plugins\' + PluginList.Items[Index];
   IsRunningPlugin := (Vis_Plugin = string(CurVisWindowAttr.PluginPath));

   with (Control as TListBox).Canvas do
   begin
      if odSelected in State then
         Brush.Color := clHighlight
      else
         Brush.Color := (Control as TListBox).Color;

    	FillRect(Rect);       // clear the rectangle

      if IsRunningPlugin then
         if odSelected in State then
            Font.Color := clYellow
         else
            Font.Color := clRed
      else
         if odSelected in State then
            Font.Color := clWhite
         else
            Font.Color := clBlack;

      Font.Charset := ANSI_CHARSET;
      Font.Name := 'Arial';
      Font.Size := 9;
      TextOut(Rect.Left + 3, Rect.Top + 1, PluginList.Items[Index]);
      if odFocused in State then
         DrawFocusRect(Rect);
   end;
end;

procedure TVisControlForm.VisModulesDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
var
   Vis_Plugin, ModuleName : string;
   IsRunningModule : boolean;
   SelectedIndexNum : integer;
begin
   SelectedIndexNum := PluginList.ItemIndex;
   Vis_Plugin := GetProgDir + 'Plugins\' + PluginList.Items[SelectedIndexNum];
   if Vis_Plugin = string(CurVisWindowAttr.PluginPath) then
      if ARow = CurVisWindowAttr.ModNo then
         IsRunningModule := true
      else
         IsRunningModule := false
   else
      IsRunningModule := false;

   ModuleName := VisModules.Cells[0, ARow];
   with (Sender as TDrawGrid).Canvas do
   begin
      if gdSelected in State then
         Brush.Color := clHighlight
      else
         Brush.Color := (Sender as TDrawGrid).Color;

      FillRect(Rect);   // clear the rectangle

      if IsRunningModule then
         if gdSelected in State then
            Font.Color := clYellow
         else
            Font.Color := clRed
      else if gdSelected in State then
         Font.Color := clWhite
      else
         Font.Color := clBlack;

      Font.Charset := ANSI_CHARSET;
      Font.Name := 'Arial';
      Font.Size := 9;
      TextOut(Rect.Left + 3, Rect.Top + 1, ModuleName);

      if gdFocused in State then
         DrawFocusRect(Rect);
   end;
end;

end.
