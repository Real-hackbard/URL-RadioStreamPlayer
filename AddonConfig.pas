unit AddonConfig;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Grids, BassPlayer, PluginCtrl;

type
  TAddonConfigForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    PluginGrid: TStringGrid;
    Label3: TLabel;
    btnLoad: TButton;
    btnUnload: TButton;
    btnClose: TButton;
    Label5: TLabel;
    PluginList: TStringGrid;
    Label4: TLabel;
    procedure ClearPluginGrid;
    procedure FillPluginGrid;
    procedure btnCloseClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure btnUnloadClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PluginGridSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure PluginListDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure PluginGridDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure SetInUsePlugin(CurPluginName : string);
  end;

var
  AddonConfigForm: TAddonConfigForm;


implementation

uses BassTest;

{$R *.DFM}


var
   LoadCounter : integer;
   PluginCounter : integer;
   CurSelection : TGridRect;
   IndexNum : integer;
   PluginName : string;
   InUsePlugin : string = '';
 //  MainMessageHandle : hwnd;
   BASSAddonList : TBASSAddonList;

procedure TAddonConfigForm.ClearPluginGrid;
var
   i : integer;
begin
   with PluginGrid do
   begin
     Perform(WM_SETREDRAW, 0, 0);  // block visual updates
     try
       for i := FixedRows to RowCount - 1 do
         Rows[i].Clear;
     finally
       Perform(WM_SETREDRAW, 1, 0);
       Invalidate;
     end;
   end;
end;

procedure TAddonConfigForm.FillPluginGrid;
var
   i, j : integer;
   s1, s2 : string;


begin
   LoadCounter := 0;
   BASSAddonList := BassPlayer1.GetBASSAddonList;
   for i := 1 to MaxAddon do
   begin
      if BASSAddonList[i].Handle <> 0 then     // Handle = 0 : empty element
      begin
         inc(LoadCounter);
         PluginGrid.Cells[0, LoadCounter] := BASSAddonList[i].Name;
         s1 := '';
         s2 := '';
         for j := 1 to BASSAddonList[i].NumFormat do
         begin
            if s1 <> '' then
               s1 := s1 + ', ' + BASSAddonList[i].FormatP[j-1].name
            else
               s1 := BASSAddonList[i].FormatP[j-1].name;
            if s2 <> '' then
               s2 := s2 + ';' + BASSAddonList[i].FormatP[j-1].exts
            else
               s2 := BASSAddonList[i].FormatP[j-1].exts;
         end;
         PluginGrid.Cells[1, LoadCounter] := s1 + ' V' +
                                          intToStr(Hi(HiWord(BASSAddonList[i].Version))) + '.' +
                                          intToStr(Lo(HiWord(BASSAddonList[i].Version))) + '.' +
                                          intToStr(Hi(LoWord(BASSAddonList[i].Version))) + '.' +
                                          intToStr(Lo(LoWord(BASSAddonList[i].Version)));
         PluginGrid.Cells[2, LoadCounter] := s2;
      end;
   end;

   if LoadCounter = 0 then
      btnUnload.Enabled := false
   else begin
      CurSelection := PluginGrid.Selection;
      if CurSelection.Top > 0 then
         if PluginGrid.Cells[0, CurSelection.Top] = InUsePlugin then
            btnUnload.Enabled := false
         else
            btnUnload.Enabled := true
      else
         btnUnload.Enabled := false;
   end;
end;

procedure TAddonConfigForm.btnCloseClick(Sender: TObject);
begin
   Close;
end;

procedure TAddonConfigForm.btnLoadClick(Sender: TObject);
const
   msg2 = 'Select an add-on to load first';
   msg6 = 'Load denied : Check if it is for enabling the '#10#10 +
          '          playback of additional file formats.';
var
   BASSAddonInfo : TBASSAddonInfo;
   s1, s2 : string;
   j : integer;
begin
   IndexNum := PluginList.Selection.Top;
   PluginName := PluginList.Cells[0, IndexNum];
   if PluginName = '' then
   begin
      Application.MessageBox(msg2, 'Confirm', MB_ICONINFORMATION + MB_OK);
      exit;
   end;

   BASSAddonInfo := BassPlayer1.BASSAddonLoad(GetProgDir + 'Plugins\' + PluginName);
   if BASSAddonInfo.Handle <> 0 then
   begin
      inc(LoadCounter);
      PluginGrid.Cells[0, LoadCounter] := BASSAddonInfo.Name;
      s1 := '';
      s2 := '';
      for j := 1 to BASSAddonInfo.NumFormat do
      begin
         if s1 <> '' then
            s1 := s1 + ', ' + BASSAddonInfo.FormatP[j-1].name
         else
            s1 := BASSAddonInfo.FormatP[j-1].name;
         if s2 <> '' then
            s2 := s2 + ';' + BASSAddonInfo.FormatP[j-1].exts
         else
            s2 := BASSAddonInfo.FormatP[j-1].exts;
      end;
      PluginGrid.Cells[1, LoadCounter] := s1 + ' V' +
                                          intToStr(Hi(HiWord(BASSAddonInfo.Version))) + '.' +
                                          intToStr(Lo(HiWord(BASSAddonInfo.Version))) + '.' +
                                          intToStr(Hi(LoWord(BASSAddonInfo.Version))) + '.' +
                                          intToStr(Lo(LoWord(BASSAddonInfo.Version)));
      PluginGrid.Cells[2, LoadCounter] := s2;

   { if LoadCounter = 1 then
      begin } // put the 1st add-on to selected state
      CurSelection.Top := LoadCounter;
      CurSelection.Left := 0;
      CurSelection.Right := 2;
      CurSelection.Bottom := LoadCounter;
      PluginGrid.Selection := CurSelection;
      if PluginGrid.Cells[0, CurSelection.Top] = InUsePlugin then
          btnUnload.Enabled := false
      else
          btnUnload.Enabled := true;
   { end; }
      PluginList.Invalidate;
      //    btnLoad.Enabled := false;
   end else
      Application.MessageBox(msg6, 'Confirm', MB_ICONSTOP + MB_OK);
end;


procedure TAddonConfigForm.btnUnloadClick(Sender: TObject);
const
   msg1 = 'No add-ons are loaded';
   msg2 = 'Select an add-on to unload first';
   msg3 = 'Cannot unload : Unknown Error';
   msg4 = 'Cannot get the handle of add-on';

var
   AddonName : string;
   AddonHandle : DWORD;
   Foundmatched : boolean;
   i : integer;
begin
   if LoadCounter = 0 then
   begin
      Application.MessageBox(msg1, 'Confirm', MB_ICONINFORMATION + MB_OK);
      exit;
   end;

   CurSelection := PluginGrid.Selection;
   if CurSelection.Top = -1 then
   begin
      Application.MessageBox(msg2, 'Confirm', MB_ICONINFORMATION + MB_OK);
      exit;
   end;

   AddonName := PluginGrid.Cells[0, CurSelection.Top];
   if AddonName = '' then
   begin
      Application.MessageBox(msg2, 'Confirm', MB_ICONINFORMATION + MB_OK);
      exit;
   end;

   Foundmatched := false;
   BASSAddonList := BassPlayer1.GetBASSAddonList;
   for i := 1 to MaxAddon do
      if BASSAddonList[i].Name = AddonName then
      begin
         AddonHandle := BASSAddonList[i].Handle;
         Foundmatched := true;
      end;

   if not Foundmatched then
   begin
      Application.MessageBox(msg4, 'Confirm', MB_ICONINFORMATION + MB_OK);
      exit;
   end;

   if BassPlayer1.BASSAddonFree(AddonHandle) > 0 then
   begin
      if CurSelection.Top = LoadCounter then   // the last item in PluginGrid ?
      begin
         PluginGrid.Cells[0, LoadCounter] := '';
         PluginGrid.Cells[1, LoadCounter] := '';
         PluginGrid.Cells[2, LoadCounter] := '';
         dec(LoadCounter);
         if LoadCounter > 0 then
         begin  // Move selected position
            CurSelection.Top := LoadCounter;
            CurSelection.Left := 0;
            CurSelection.Right := 2;
            CurSelection.Bottom := LoadCounter;
         end else
         begin  // No selected position
            CurSelection.Top := -1;
            CurSelection.Left := 0;
            CurSelection.Right := 2;
            CurSelection.Bottom := -1;
         end;
         PluginGrid.Selection := CurSelection;
         if LoadCounter = 0 then  // No add-on is loaded.
            btnUnload.Enabled := false
         else if PluginGrid.Cells[0, CurSelection.Top] = InUsePlugin then
            btnUnload.Enabled := false
         else
            btnUnload.Enabled := true
      end else
      begin
         ClearPluginGrid;
         FillPluginGrid;
      end;
      PluginList.Invalidate;
   end else
      Application.MessageBox(msg3, 'Confirm', MB_ICONINFORMATION + MB_OK);
end;


procedure TAddonConfigForm.FormShow(Sender: TObject);
var
   GridRect : TGridRect;
   SearchRec : TSearchRec;
   i : integer;
begin
 // Clear PluginList's contents
   PluginList.RowCount := 1;
   PluginList.Cells[0, 0] := '';
   btnLoad.Enabled := false;

   i := 0;
   if FindFirst(GetProgDir + 'Plugins\bass_*.dll', 0, SearchRec) = 0 then
   begin
      PluginList.Cells[0, 0] := SearchRec.Name;
      i := 1;
      while FindNext(SearchRec) = 0 do
      begin
         PluginList.RowCount := i + 1;
         PluginList.Cells[0, i] := SearchRec.Name;
         inc(i);
      end;
   end;
   FindClose(SearchRec);
   PluginCounter := i;

   ClearPluginGrid;
   FillPluginGrid;

   if PluginCounter = 0 then   // There are no add-on in <Prog_Dir>\Plugins directory..
   begin
      GridRect.Top := -1;
      GridRect.Left := -1;
      GridRect.Right := -1;
      GridRect.Bottom := -1;
      PluginList.Selection := GridRect;
      PluginList.Enabled := false;
      PluginGrid.Enabled := false;
   end else
   begin
      GridRect.Top := 0;
      GridRect.Left := 0;
      GridRect.Right := 0;
      GridRect.Bottom := 0;
      PluginList.Selection := GridRect;
      PluginList.Enabled := true;
      PluginGrid.Enabled := true;
   end;

   if LoadCounter = 0 then   // There are no loaded add-on.
   begin
      GridRect.Top := -1;
      GridRect.Left := -1;
      GridRect.Right := -1;
      GridRect.Bottom := -1;
      PluginGrid.Selection := GridRect;
   end else
   begin
      GridRect.Top := 1;
      GridRect.Left := 0;
      GridRect.Bottom := 1;
      GridRect.Right := 2;
      PluginGrid.Selection := GridRect;
   end;

end;

procedure TAddonConfigForm.FormCreate(Sender: TObject);
begin
   with PluginGrid do
   begin
      Cells[0, 0] := 'Name';
      Cells[1, 0] := 'Decsription';
      Cells[2, 0] := 'Playable File Types';
   end;
end;

procedure TAddonConfigForm.PluginGridSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
   if PluginGrid.Cells[ACol, ARow] = '' then
      CanSelect := false
   else begin
      if PluginGrid.Cells[0, ARow] = InUsePlugin then
         btnUnload.Enabled := false
      else
         btnUnload.Enabled := true;
      CanSelect := true;
   end;
end;

procedure TAddonConfigForm.PluginListDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
var
 //  PluginName : string;
   PluginLoaded : boolean;
   i : integer;
begin
   PluginName := PluginList.Cells[0, ARow];

   PluginLoaded := false;
   for i := 1 to LoadCounter do
      if Uppercase(PluginGrid.Cells[0, i]) = Uppercase(PluginName) then
      begin
          PluginLoaded := true;
          break;
      end;

   with Sender as TDrawGrid do
   begin
      if gdSelected in State then
      begin
         Canvas.Brush.Color := clHighlight;
         if PluginLoaded then
            btnLoad.Enabled := false
         else
            btnLoad.Enabled := true;
      end else
         Canvas.Brush.Color := (Sender as TDrawGrid).Color;
      Canvas.FillRect(Rect);

      if PluginLoaded then
         Canvas.Font.Color := clLtGray
      else
         if gdSelected in State then
            Canvas.Font.Color := clWhite
         else
            Canvas.Font.Color := clBlack;

      Canvas.Font.Charset := ANSI_CHARSET;
      Canvas.Font.Name := 'Arial';
      Canvas.Font.Size := 9;
      Canvas.TextOut(Rect.Left + 3, Rect.Top + 1, PluginName);
      if gdFocused in State then
         Canvas.DrawFocusRect(Rect);
   end;
end;

// Enables or disables "Unload" button according to the state of a selected add-on,
//  if selected add-on is in use then "Unload" button is set to disabled state
//  else set to enabled state.
procedure TAddonConfigForm.SetInUsePlugin(CurPluginName : string);
begin
   InUsePlugin := CurPluginName;

   CurSelection := PluginGrid.Selection;
   if CurSelection.Top > 0 then
      PluginName := PluginGrid.Cells[0, CurSelection.Top]
   else
      PluginName := '';

   if InUsePlugin = '' then   // no add-on is in use.
      btnUnload.Enabled := true
   else if InUsePlugin = PluginName then
      btnUnload.Enabled := false
   else
      btnUnload.Enabled := true;

   PluginGrid.Invalidate;   // Redraw PluginGrid surface
end;

// Paints with different colors according to the state of add-ons,
// i.e., paints with Red color if the add-on is in use.
procedure TAddonConfigForm.PluginGridDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
var
   PluginName : string;
   Descr : string;
   ExtStr : string;
begin
   PluginName := PluginGrid.Cells[0, ARow];
   case ACol of
     1 : Descr := PluginGrid.Cells[1, ARow];
     2 : ExtStr := PluginGrid.Cells[2, ARow];
   end;

   with Sender as TDrawGrid do
   begin
      if ARow = 0 then
         Canvas.Brush.Color := TColor($C0DCC0)  // = clMoneyGreen
      else if gdSelected in State then
         Canvas.Brush.Color := clHighlight
      else
         Canvas.Brush.Color := (Sender as TDrawGrid).Color;
      Canvas.FillRect(Rect);

      if PluginName = InUsePlugin then
         if gdSelected in State then
            Canvas.Font.Color := clYellow
         else
            Canvas.Font.Color := clRed
      else if gdSelected in State then
         Canvas.Font.Color := clWhite
      else
         Canvas.Font.Color := clBlack;

      Canvas.Font.Charset := ANSI_CHARSET;
      Canvas.Font.Name := 'Arial';
      Canvas.Font.Size := 9;
      case ACol of
         0 : Canvas.TextOut(Rect.Left + 3, Rect.Top + 1, PluginName);
         1 : Canvas.TextOut(Rect.Left + 3, Rect.Top + 1, Descr);
         2 : Canvas.TextOut(Rect.Left + 3, Rect.Top + 1, ExtStr);
      end;

      if gdFocused in State then
         Canvas.DrawFocusRect(Rect);
   end;
end;

end.
