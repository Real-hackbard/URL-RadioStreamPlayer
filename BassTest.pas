unit BassTest;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, BassPlayer, PluginCtrl, Knob, slider, ExtCtrls,
  ComCtrls, Buttons, XPMan;

type
  TForm1 = class(TForm)
    Label_State: TLabel;
    Label_Length: TLabel;
    Panel1: TPanel;
    GaugePaintBox: TPaintBox;
    PageControl1: TPageControl;
    BasicSheet: TTabSheet;
    VisSheet: TTabSheet;
    InfoMemo: TMemo;
    Panel2: TPanel;
    Label5: TLabel;
    Label7: TLabel;
    ComboBox1: TComboBox;
    ProgressBar1: TProgressBar;
    btnNetOpen: TButton;
    Panel3: TPanel;
    Label2: TLabel;
    cbEqualizer: TCheckBox;
    cbSingleChannelMode: TCheckBox;
    GroupBox1: TGroupBox;
    cbFlanger: TCheckBox;
    cbEcho: TCheckBox;
    cbReverb: TCheckBox;
    OpenDialog1: TOpenDialog;
    Timer_Stat: TTimer;
    cbMergeVisWindow: TCheckBox;
    VisPanel: TPanel;
    Label4: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label10: TLabel;
    cbEMBEDSwitchMode: TCheckBox;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    VolumeKnob: TScrollBar;
    Slider1: TSlider;
    Label19: TLabel;
    EchoSlider: TSlider;
    ReverbSlider: TSlider;
    Slider2: TSlider;
    Slider3: TSlider;
    Slider4: TSlider;
    Slider5: TSlider;
    Slider6: TSlider;
    Slider7: TSlider;
    Slider8: TSlider;
    Slider9: TSlider;
    Slider10: TSlider;
    TabSheet1: TTabSheet;
    btnOpen: TButton;
    btnPlay: TButton;
    btnPause: TButton;
    btnStop: TButton;
    btnFileInfo: TButton;
    Label1: TLabel;
    PosSlider: TSlider;
    Label20: TLabel;
    btnAddonSetup: TButton;
    btnPluginSetup: TButton;
    btnShowDSPCtrlForm: TButton;
    btnShowVisCtrlForm: TButton;
    Button1: TButton;
    Label6: TLabel;
    edBitrate: TEdit;
    StatusBar1: TStatusBar;
    Label3: TLabel;

    procedure FormShow(Sender: TObject);
    function  OpenStream(StreamName : string) : boolean;
    procedure btnOpenClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnPlayClick(Sender: TObject);
    procedure btnPauseClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure cbFlangerClick(Sender: TObject);
    procedure cbEchoClick(Sender: TObject);
    procedure cbReverbClick(Sender: TObject);
    procedure VolumeKnobChange(Sender: TObject);
    procedure EQSliderChange(Sender: TObject);
    procedure cbEqualizerClick(Sender: TObject);
    procedure PosSliderStartTracking(Sender: TObject);
    procedure PosSliderStopTracking(Sender: TObject);
    procedure Timer_StatTimer(Sender: TObject);
    procedure CreateBasicImage;
    procedure ShowBackground;
    procedure PluginStartPlay(Sender: TObject; ChannelInfo : PChannelInfo);
    procedure PluginChannelInfo(Sender: TObject; ChannelInfo : PChannelInfo);
    procedure DisplayFFTBand(Sender: TObject; Bands : TBandOut);
    procedure GetMETAFromNet(Sender: TObject; Content : string);
    procedure GetMIDILyric(Sender: TObject; TextP : pchar);
    procedure DownloadEnded(Sender: TObject; Content : string);
    procedure ModeChanged(Sender: TObject; OldMode, NewMode : TPlayerMode);
    procedure ProcessPluginRequest(Sender: TObject; GenParam : DWORD); // * New at Ver 1.92
    procedure btnPluginSetupClick(Sender: TObject);
    procedure EchoSliderChange(Sender: TObject);
    procedure ReverbSliderChange(Sender: TObject);
    procedure btnShowVisCtrlFormClick(Sender: TObject);
    procedure btnShowDSPCtrlFormClick(Sender: TObject);
    procedure btnNetOpenClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnFileInfoClick(Sender: TObject);
    procedure cbSingleChannelModeClick(Sender: TObject);
    procedure btnAddonSetupClick(Sender: TObject);
    procedure GetVisPluginInfo(Sender: TObject; VisWindowAttr : TVisPluginInfo);
    procedure cbMergeVisWindowClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure FormCanResize(Sender: TObject; var NewWidth,
      NewHeight: Integer; var Resize: Boolean);
    procedure cbEMBEDSwitchModeClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure WndProc(var Msg : TMessage); override;
  end;

var
  Form1: TForm1;
  BassPlayer1 : TBassPlayer;

implementation

uses VisPluginCtrl, DSPPluginCtrl, AddonConfig, wa_ipc, WasabiAPI;

{$R *.dfm}

const
// Constants for frequency spectrum visualizzer
   BlockWidth = 3;
   HBlockGap = 1;
   HBlockCount = NumFFTBands;
   VLimit = 24;    // Intensity range : 0 ~ 24
   BackColor = clBlack;

   WarningMsg : array[0..1023] of char
            = 'The DSP by Winamp DSP plug-ins can be applied only if the ' + chr(10)
            + 'stream being played is decoded by a Winamp input plug-in' + chr(10)
            + 'while TBASSPlayer is set to operate in "Single Channel Mode".' + chr(10) + chr(10)
            + '(note)' + chr(10)
            + '"Single channel mode" means one channel is used to process' + chr(10)
            + 'a stream, i.e., decoding & output processes are done within' + chr(10)
            + 'one BASS channel.' + chr(10)
            + '"Dual channel mode" means two BASS channels are used to' + chr(10)
            + 'process a stream, one for decoding and the other for output.' + chr(10)
            + '"Dual channel mode" is default operation mode and used to' + chr(10)
            + 'support Winamp DSP plug-ins.' + chr(10)
            + 'So you do not need load Winamp input plug-in to activate' + chr(10)
            + 'Winamp DSP plug-in while TBASSPlayer is set to operate' + chr(10)
            + 'in "Dual channel mode".' + chr(10)
            + 'But "Single channel mode" is more reliable (there is no' + chr(10)
            + 'intermediate operation between channels) and less resource' + chr(10)
            + 'consuming than "Dual channel mode."' + chr(0);


var
   SPS : LongInt;    // sample rate
   NowTracking : boolean;
   BasicBMP : TBitMap;
   GaugeTempBMP : TBitMap;
   DisplayBar: TBitmap;
   GaugeRect : TRect;
   EQGains : TEQGains;
   PeakValue : array[1..NumFFTBands] of single;
   PassedCounter : array[1..NumFFTBands] of integer;
   IsNetRadio : boolean;
   IsNetStream : boolean;
   CurMode : TPlayerMode;
   NowLogOff : boolean = false;
   ProgDir : string;
   Saved8087CW: Word;

   CurVisWindowAttr : TVisPluginInfo;

   VisPanel_Handle : HWND;
   CycleNo : integer = 0;
   StartedInType : TVisWindowIs = UnAvailable;

 //  WaitingPrevVisStop : boolean = false;
   Vis_Plugin_ToRun : string;
   SelectedModule : integer;


procedure TForm1.WndProc(var Msg : TMessage);
begin
   if NowLogOff then
   begin
      inherited WndProc(Msg);
      exit;
   end;
   if BassPlayer1 = nil then   // if before creation of BassPlayer1
   begin
      inherited WndProc(Msg);
      exit;
   end;

 // Show or hide vis window when BassPlay is showing or hiding
   if (Msg.Msg = WM_SHOWWINDOW) then
   begin
    //  VisWindowHandle := BassPlayer1.GetVisWindowHandle;
    // VisWindowHandle = 0 : There is no vis window.
      if (CurVisWindowAttr.VisHandle = 0) then
      begin
         inherited WndProc(Msg);
         exit;
      end;

      if Msg.wParam = 0 then         // = BassPlay is hiding
         BASSPlayer1.HideVisWindow   // hide vis window
      else                           // = BassPlay is showing
         BASSPlayer1.ShowVisWindow;  // show vis window

    // Following is the method using direct system call
    {  if Msg.wParam <> 0 then
         ShowWindow(CurVisWindowAttr.VisHandle, SW_SHOW)
      else
         ShowWindow(CurVisWindowAttr.VisHandle, SW_HIDE); }

    //  Msg.result := 0;
      inherited WndProc(Msg);
   end else
   if (Msg.Msg = WM_QueryEndSession) then
   begin
   // Timer_Stat should be disabled soon after receiving WM_QueryEndSession message
   // to prevent problems at log off or shut down system.
      Timer_Stat.Enabled := false;
      NowLogOff := true;
      Msg.Result := 1;    // Allow system termination
   end else
   if Msg.Msg = WM_WA_IPC then
   begin
     if Msg.LParam = IPC_GET_API_SERVICE then
     begin
      // Msg.Result := integer(@APIService);
        Msg.Result := WaAPIServiceEntry;
     end;
   end else
     inherited WndProc(Msg);
end;

procedure TForm1.FormShow(Sender: TObject);
var
   i : integer;
 //  BassAddonInfo : TBassAddonInfo;
   MIDI_FONTINFO : TMIDI_FONTINFO;
   UnloadedNames : string;
begin
   BassPlayer1 := TBassPlayer.Create(Self);

 // Check if required DLL's are succefully loaded.
   if BassPlayer1.BASSDLLVer = '' then
   begin
      Application.MessageBox('BASS.DLL is not loaded.', 'Confirm', MB_OK or MB_ICONINFORMATION);
      exit;
   end;

   UnloadedNames := '';
   if not BassPlayer1.BASSWMAReady then UnloadedNames := 'BASSWMA.DLL';
   if not BassPlayer1.BASSCDReady then
      if UnloadedNames = '' then
         UnloadedNames := 'BASSCD.DLL'
      else
         UnloadedNames := UnloadedNames + ', BASSCD.DLL';
   if not BassPlayer1.BASSMIDIReady then
      if UnloadedNames = '' then
         UnloadedNames := 'BASSMIDI.DLL'
       else
         UnloadedNames := UnloadedNames + ', BASSMIDI.DLL';
   if UnloadedNames <> '' then
      Application.MessageBox(pchar('Following BASS Library(s) is(are) not loaded.' + char(10) +
                           char(10) + UnloadedNames), 'Confirm', MB_OK or MB_ICONINFORMATION);

   BassPlayer1.OnNewFFTData := DisplayFFTBand;
   BassPlayer1.OnPluginStartPlay := PluginStartPlay;
   BassPlayer1.OnGetChannelInfo := PluginChannelInfo;
   BassPlayer1.OnGetMeta := GetMETAFromNet;
   BassPlayer1.OnGetLyric := GetMIDILyric;
   BassPlayer1.OnDownloaded := DownloadEnded;
   BassPlayer1.OnModeChange := ModeChanged;
   BassPlayer1.OnVisWindowShow := GetVisPluginInfo;
   BassPlayer1.OnPluginRequest := ProcessPluginRequest;

   VolumeKnob.Position := BassPlayer1.Volume;
   CreateBasicImage;
   ShowBackground;
   for i := 1 to NumFFTBands do
      PeakValue[i] := 0;

   EQGains[0] := Slider1.Value - 15.0;
   EQGains[1] := Slider2.Value - 15.0;
   EQGains[2] := Slider3.Value - 15.0;
   EQGains[3] := Slider4.Value - 15.0;
   EQGains[4] := Slider5.Value - 15.0;
   EQGains[5] := Slider6.Value - 15.0;
   EQGains[6] := Slider7.Value - 15.0;
   EQGains[7] := Slider8.Value - 15.0;
   EQGains[8] := Slider9.Value - 15.0;
   EQGains[9] := Slider10.Value - 15.0;

   EchoSlider.Value := BassPlayer1.EchoLevel;
   ReverbSlider.Value := BassPlayer1.ReverbLevel;

   if BassPlayer1.PlayerReady then
   begin
      //LoadWinampPlugin('in_cdda.dll');
      //LoadWinampPlugin('in_asf.dll');
      //LoadWinampPlugin('in_midi.dll');   // I have found that it is unable to load in_midi v3.16.
        // So, I use in_midi v3.07.

      //LoadWinampPlugin('in_wm_old.dll');

      BassPlayer1.PluginFirst := false;

    // Following sentences are for supporting BASSMDID
      if BassPlayer1.BASSMIDIReady then
         if FileExists(ProgDir + 'Plugins\Chorium.SF2') then
         begin
         // You can use any SF2 soundfont other than Chorium.SF2 to activate BASSMIDI.dll.
         // However this demo program requires Chorium.SF2.
            if BassPlayer1.MIDIFontInit(ProgDir + 'Plugins\Chorium.SF2', MIDI_FONTINFO) then
            begin
               if BassPlayer1.MIDISoundReady then
                  InfoMemo.Lines.Add('MIDI sound ready !');

               InfoMemo.Lines.Add('MIDI soundfont Chorium.SF2 loaded');
               InfoMemo.Lines.Add(' Name : ' + MIDI_FONTINFO.FontName);
               InfoMemo.Lines.Add(' Presets : ' + formatFloat('#,##0', MIDI_FONTINFO.Presets));
               InfoMemo.Lines.Add(' Sample Size : ');
               InfoMemo.Lines.Add('         ' + formatFloat('#,##0', MIDI_FONTINFO.SampleSize) + ' bytes');
            end
         end else
         begin
            InfoMemo.Lines.Add('Cannot support BASS MIDI plugin because MIDI soundfont'#10 +
                               ' "Chorium.SF2" does not exist in the <Program_Directory>\Plugins directory.');
         end;

      if BassPlayer1.VisDrawerReady then
         BassPlayer1.UseVisDrawer := true
      else
      begin
         VisControlForm.UseWinampLook.Checked := false;
         VisControlForm.UseWinampLook.Enabled := false;
         Application.MessageBox('Gen_VisDrawer.DLL does not exist or not loaded.'#10 +
                                'Winamp-like visualization form is not supported.', 'Confirm',
                                MB_OK or MB_ICONINFORMATION);
      end;

   end;

   cbSingleChannelMode.Checked := BassPlayer1.SingleChannel;
   CurVisWindowAttr.VisHandle := 0;
   CurVisWindowAttr.VisType := Unavailable;

   VisPanel_Handle := VisPanel.Handle;
   if cbMergeVisWindow.Checked then
       BassPlayer1.VisEmbedHandle := VisPanel_Handle
    else
       BassPlayer1.VisEmbedHandle := 0;
   if BassPlayer1.VisEMBEDSwitchMode = WindowMove then
      cbEMBEDSwitchMode.Checked := true
   else
      cbEMBEDSwitchMode.Checked := false;

   if BassPlayer1.PlayerReady then
      Timer_Stat.Enabled := true;

   Saved8087CW := Default8087CW;
   Set8087CW($133f);  // Disable all fpu exceptions

   ComboBox1.Items.LoadFromFile(ExtractFilePath(Application.ExeName) +  'URL\lst.ini');
   ComboBox1.ItemIndex := 0;
end;

function TForm1.OpenStream(StreamName : string) : boolean;
var
   s1, s2 : string;
   URLInfo : pchar;
   i : integer;
 //  MIDITrackInfo : TMIDITrackInfo;
begin
   result := false;
   IsNetRadio := false;
   IsNetStream := false;

   if BassPlayer1.Open(StreamName) then
   begin
      Statusbar1.Panels[5].Text := BassPlayer1.StreamInfo.Title;
      Label_Length.Caption := 'Length ' +
            FormatDateTime ('nn:ss', BassPlayer1.PlayLength / (1000 * 24 * 60 * 60));
      InfoMemo.Clear;
      SPS := 0;
      edBitrate.Text := '';

      if BassPlayer1.IsNetRadio then
      begin
         InfoMemo.Lines.Add('Connected to an internet radio station.');

         URLInfo := BassPlayer1.ICYInfo;
         if URLInfo <> nil then
            while StrLen(URLInfo) > 0 do
            begin
               if pos('icy-name:', string(URLInfo)) <> 0 then
               begin
                  inc(URLInfo, 9);
                  InfoMemo.Lines.Add('icy-name : ' + string(URLInfo));
                  break;
               end;
               inc(URLInfo, StrLen(URLInfo) + 1);
            end;

         if BassPlayer1.StreamInfo.BitRate > 0 then
            edBitrate.Text := intToStr(BassPlayer1.StreamInfo.BitRate) + 'KBPS';
         IsNetRadio := true;

      end else
      if BassPlayer1.IsNetStream then
      begin
         SPS := BassPlayer1.StreamInfo.SampleRate;
         if SPS <> 0 then
         begin
            InfoMemo.Lines.Add('Opened a stream on internet.');
            edBitrate.Text := intToStr(BassPlayer1.StreamInfo.BitRate) + 'KBPS';
         end;
         URLInfo := BassPlayer1.HTTPInfo;
         if URLInfo <> nil then
            while StrLen(URLInfo) > 0 do
            begin
               if pos('Server:', string(URLInfo)) <> 0 then
               begin
                  InfoMemo.Lines.Add(string(URLInfo));
                  break;
               end;
               inc(URLInfo, StrLen(URLInfo) + 1);
            end;
         IsNetStream := true;

        // <-
         URLInfo := BassPlayer1.ICYInfo;
         if URLInfo <> nil then
            while StrLen(URLInfo) > 0 do
            begin
               InfoMemo.Lines.Add(string(URLInfo));
               inc(URLInfo, StrLen(URLInfo) + 1);
            end;   // <-

      end else   // local stream file
      begin
         InfoMemo.Lines.Add('Opened ' + ExtractFileName(BassPlayer1.StreamInfo.FileName));
         if BassPlayer1.DecodingByPlugin then
            InfoMemo.Lines.Add(' (Being decoded by Winamp plug-in)')
         else if BassPlayer1.DecoderName <> 'BASS_Native' then
            InfoMemo.Lines.Add(' (Being decoded by ' + BassPlayer1.DecoderName + ')');

         edBitrate.Text := intToStr(BassPlayer1.StreamInfo.BitRate) + 'KBPS';
      end;

      if not IsNetStream then
         SPS := BassPlayer1.StreamInfo.SampleRate;
      if SPS <> 0 then
      begin
         s1 := intToStr(SPS) + 'Hz';
         if BassPlayer1.StreamInfo.Channels = 1 then
            S2 := 'Mono'
         else if BassPlayer1.StreamInfo.Channels = 2 then
            S2 := 'Stereo'
         else if BassPlayer1.StreamInfo.Channels > 2 then
            S2 := intToStr(BassPlayer1.StreamInfo.Channels) + ' Channel';
         InfoMemo.Lines.Add(s1 + ', ' + s2);
      end;

      if not BassPlayer1.DecodingByPlugin then
         if BassPlayer1.SingleChannel then
            InfoMemo.Lines.Add('(Single channel mode)');
        { else
            InfoMemo.Lines.Add(' (Operates in dual channel mode)')};

    // Activate following sentences if you want to know the track information of a opened
    // MIDI file.
     { if pos(UpperCase(ExtractFileExt(StreamName)), MIDIFileExt) > 0 then
      begin
         if BassPlayer1.MIDIGetTrackInfo(StreamName, MIDITrackInfo) then
            for i := 1 to MIDITrackInfo.TrackCount do
               InfoMemo.Lines.Add('Track[' + intToStr(i-1) + '] : ' + MIDITrackInfo.TrackText[i-1]);
      end; }

   // Following statements are only to scroll down Memo1's contents, i.e.,
   //  to make Memo1's 1st line as the top line.
      if PageControl1.ActivePage = BasicSheet then
      begin
         InfoMemo.SetFocus;
         InfoMemo.SelStart := 0;
         InfoMemo.SelLength := 0;
      end;

      if IsNetRadio then
         PosSlider.ThumbVisible := false
      else
         PosSlider.ThumbVisible := true;

      for i := 1 to HBlockCount do   // * Added at Ver 1.92
          PeakValue[i] := 0;

      BassPlayer1.Play;

      result := true;
   end;
end;

procedure TForm1.btnOpenClick(Sender: TObject);
var
   s1, s2, s3 : string;
begin
   if not BassPlayer1.PlayerReady then
   begin
      Application.MessageBox('Cannot open file (BassPlayer is not ready)',
                 'Confirm', MB_OK or MB_ICONINFORMATION);
      exit;
   end;

   OpenDialog1.FileName := '';
   s1 := BassPlayer1.NativeFileExts;
   s2 := BassPlayer1.PluginFileExts;
   s3 := BassPlayer1.BASSAddonExts;
   if (s2 = '') and (s3 = '') then
      OpenDialog1.Filter := 'BASS native files (' + s1 + ')|' + s1 + '|'
   else
      OpenDialog1.Filter := 'All playable files |' + s1 + s2 + s3 + '|' +
                            'BASS native files (' + s1 + ')|' + s1 + '|';
   if s2 <> '' then
      OpenDialog1.Filter := OpenDialog1.Filter + 'Winamp plug-in supported files (' + s2 + ')|' + s2 + '|';
   if s3 <> '' then
      OpenDialog1.Filter := OpenDialog1.Filter + 'BASS add-on supported files (' + s3 + ')|' + s3 + '|' ;

   if OpenDialog1.Execute then
      if OpenStream(OpenDialog1.FileName) then
      begin
         s1 := BassPlayer1.DecoderName;
         if copy(s1, 1, 5) = 'bass_' then
            AddonConfigForm.SetInUsePlugin(s1)
         else
            AddonConfigForm.SetInUsePlugin('');
      end;
end;


procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   BassPlayer1.BASSAddonFree(0);  // Release all add-ons
   BassPlayer1.Free;
   BasicBMP.Free;
   GaugeTempBMP.Free;
   DisplayBar.Free;

   Set8087CW(Saved8087CW)
end;

procedure TForm1.btnPlayClick(Sender: TObject);
var
   i : integer;
begin
   if BassPlayer1.Mode = plmPlaying then
      exit;

   for i := 1 to HBlockCount do  // * Added at Ver 1.92
       PeakValue[i] := 0;

   BassPlayer1.Play;
end;

procedure TForm1.btnPauseClick(Sender: TObject);
begin
   if BassPlayer1.Mode = plmPaused then
      exit;

   BassPlayer1.Pause(true);
end;

procedure TForm1.btnStopClick(Sender: TObject);
begin
   if BassPlayer1.Mode = plmStopped then
      exit;

   BassPlayer1.Stop;
end;

procedure TForm1.cbFlangerClick(Sender: TObject);
begin
   if cbFlanger.Checked then
      BassPlayer1.SoundEffects := BassPlayer1.SoundEffects + [Flanger]
   else
      BassPlayer1.SoundEffects := BassPlayer1.SoundEffects - [Flanger];

end;

procedure TForm1.cbEchoClick(Sender: TObject);
begin
   if cbEcho.Checked then
      BassPlayer1.SoundEffects := BassPlayer1.SoundEffects + [Echo]
   else
      BassPlayer1.SoundEffects := BassPlayer1.SoundEffects - [Echo];
end;

procedure TForm1.cbReverbClick(Sender: TObject);
begin
   if cbReverb.Checked then
      BassPlayer1.SoundEffects := BassPlayer1.SoundEffects + [Reverb]
   else
      BassPlayer1.SoundEffects := BassPlayer1.SoundEffects - [Reverb];
end;


procedure TForm1.cbEqualizerClick(Sender: TObject);
begin
   if cbEqualizer.Checked then
      BassPlayer1.SoundEffects := BassPlayer1.SoundEffects + [Equalizer]
   else
      BassPlayer1.SoundEffects := BassPlayer1.SoundEffects - [Equalizer];
end;

procedure TForm1.VolumeKnobChange(Sender: TObject);
begin
   BassPlayer1.Volume := VolumeKnob.Position;
   Statusbar1.Panels[3].Text := IntToStr(VolumeKnob.Position) + ' %';
end;

procedure TForm1.EQSliderChange(Sender: TObject);
var
   BandNum : integer;
begin
   BandNum := (Sender as TSlider).Tag - 1;
   EQGains[BandNum] := (Sender as TSlider).Value - 15.0;

 //  BassPlayer1.EQGains := EQGains;
   BassPlayer1.SetAEQGain(BandNum, EQGains[BandNum]);
end;


procedure TForm1.PosSliderStartTracking(Sender: TObject);
begin
   NowTracking := true;
end;

procedure TForm1.PosSliderStopTracking(Sender: TObject);
var
   SongPos : DWORD;
begin
   if BassPlayer1.Seekable then
   begin
      SongPos := Trunc(PosSlider.Value * BassPlayer1.PlayLength / PosSlider.MaxValue);
      BassPlayer1.Position := SongPos;
   end;

   NowTracking := false;
end;

procedure TForm1.ModeChanged(Sender: TObject; OldMode, NewMode : TPlayerMode);
begin
   CurMode := NewMode;

   if CurMode = plmStandby then
   begin
      edBitrate.Text := '';
      InfoMemo.Clear;
      Label_State.Caption := 'Standby';
   end else if CurMode = plmReady then
      Label_State.Caption := 'Ready to play'
   else if CurMode = plmPlaying then
      Timer_Stat.Enabled := true
   else if CurMode = plmStopped then
   begin
      Label_State.Caption := 'Stopped';
    //  if IsNetradio then
   end      
   else if CurMode = plmPaused then
      Label_State.Caption := 'Paused';

   if (OldMode = plmPlaying) and (CurMode <> plmPlaying) then
   begin
      Timer_Stat.Enabled := false;
      VisSheet.Caption := 'Visualization';
      ShowBackground;
   end;
end;

procedure TForm1.Timer_StatTimer(Sender: TObject);
var
   BarPosition : integer;
begin
   Timer_Stat.Enabled := false;

   if (CurMode = plmPlaying) and (not IsNetRadio) then
   begin
      Label_State.Caption := 'Playing '
                 +  FormatDateTime ('nn:ss', BassPlayer1.Position / (1000 * 24 * 60 * 60));

      if not NowTracking and (BassPlayer1.Mode <> plmStopped) then
         if BassPlayer1.PlayLength > 0 then
            PosSlider.Value := (BassPlayer1.Position * PosSlider.MaxValue) div BassPlayer1.PlayLength;
   end else
   if IsNetRadio and (CurMode = plmPlaying) then
      if Label_State.Caption <> 'Playing (Radio)' then
         Label_State.Caption := 'Playing (Radio)';

   // Decoding Radio Stream by WinAmp Plug_in
   if IsNetStream then
   begin
      if BassPlayer1.DecodingByPlugin then
      begin
         if ProgressBar1.Position <> 0 then
            ProgressBar1.Position := 0  // not available if decoded by Winamp plug-in
      end else
      begin
         BarPosition := round((BassPlayer1.DownloadProgress * ProgressBar1.Max)
                                   / BassPlayer1.StreamInfo.FileSize);
         if ProgressBar1.Position <> BarPosition then
            ProgressBar1.Position := BarPosition;
      end;      
   end else
      ProgressBar1.Position := 0;

   if (PageControl1.ActivePage = BasicSheet) and
      (CurVisWindowAttr.VisType = OnAssignedWindow) then
   begin
      if CycleNo < 9 then
      begin
         if CycleNo = 1 then
            VisSheet.Caption := 'Visualization';
      end else
      begin
         if CycleNo = 9 then
            VisSheet.Caption := 'Displaying...'
         else
            if CycleNo >= 15 then
               CycleNo := 0;
      end;
      
      inc(CycleNo);
   end;
   Timer_Stat.Enabled := true;
   Label3.Caption := IntToStr(ProgressBar1.Position) + ' %';
end;

// Set background image & bar image for spectrum display
procedure TForm1.CreateBasicImage;
var
   i : integer;
   R, G, B: Integer;
begin
   BasicBMP := TBitMap.Create;
   BasicBMP.Width := (BlockWidth + HBlockGap) * HBlockCount - HBlockGap + 3;
   BasicBMP.Height := VLimit + 1;
   GaugePaintBox.Width := BasicBMP.Width;
   GaugePaintBox.Height := BasicBMP.Height;
   GaugeTempBMP := TBitMap.Create;
   GaugeTempBMP.Width := BasicBMP.Width;
   GaugeTempBMP.Height := BasicBMP.Height;
   GaugeRect.Left := 0;
   GaugeRect.Top := 0;
   GaugeRect.Right := BasicBMP.Width;
   GaugeRect.Bottom := BasicBMP.Height;

   with BasicBMP.Canvas do
   begin
      Brush.Color := BackColor;
      Brush.Style := bsSolid;
      FillRect(Rect(0, 0, BasicBMP.Width, BasicBMP.Height));
      Pen.Color := clBlue;
      Pen.Width := 1;
      Pen.Style := psDot;
      MoveTo(0, 0);
      LineTo(0, BasicBMP.Height);        // Draw Y-axis line
      MoveTo(0, BasicBMP.Height - 1);
      LineTo(BasicBMP.Width, BasicBMP.Height - 1); // Draw X-axis line
   end;

   DisplayBar := TBitmap.Create;
   DisplayBar.PixelFormat := pf32bit;
   DisplayBar.Width := BlockWidth;
   DisplayBar.Height := VLimit;

   R := 255;
   G := 0;
   B := 0;

   for i := 0 to VLimit - 1 do
   begin
     if i > VLimit / 2 then
        Dec(R, Trunc(256 / VLimit))
     else
        Inc(G, Trunc(768 / VLimit));
     if R < 0 then R := 0;
     if G > 255 then G := 255;
     DisplayBar.Canvas.Brush.Color := TColor(RGB(R, G, B));
     DisplayBar.Canvas.FillRect(Rect(0, i, BlockWidth, i + 1));
   end;

end;

// Winamp plug-in notified that it has started playing a stream file
procedure TForm1.PluginStartPlay(Sender: TObject; ChannelInfo : PChannelInfo);
var
   s1, s2 : string;
begin
   if SPS = 0 then     // = have not gotten stream information yet
   begin
      if IsNetStream then
      begin
         InfoMemo.Lines.Add('Opened an internet stream.');
         if BassPlayer1.DecodingByPlugin then
            InfoMemo.Lines.Add(' (Being decoded by Winamp plug-in)');
      end;

      SPS := ChannelInfo^.SampleRate;
      if SPS <> 0 then
      begin
         s1 := intToStr(SPS) + 'Hz';
         if ChannelInfo^.Channels = 1 then
            S2 := 'Mono'
         else if BassPlayer1.StreamInfo.Channels = 2 then
            S2 := 'Stereo'
         else if BassPlayer1.StreamInfo.Channels > 2 then
            S2 := intToStr(BassPlayer1.StreamInfo.Channels) + ' Channel';
         InfoMemo.Lines.Add(s1 + ', ' + s2);
      end;

      if PageControl1.ActivePage = BasicSheet then
      begin
         InfoMemo.SetFocus;
         InfoMemo.SelStart := 0;
         InfoMemo.SelLength := 0;
      end;

      Label_Length.Caption := 'Length ' +
         FormatDateTime ('nn:ss', BassPlayer1.PlayLength / (1000 * 24 * 60 * 60));
      Statusbar1.Panels[5].Text := BassPlayer1.StreamInfo.Title;
   end;
end;

// Winamp plug-in has notified that it had gotten channel information on a playing stream file
// The first call of this procedure precedes PluginStartPlay.
procedure TForm1.PluginChannelInfo(Sender: TObject; ChannelInfo : PChannelInfo);
begin
  // SPS := ChannelInfo^.SampleRate;
   edBitrate.Text := intToStr(ChannelInfo^.BitRate) + 'KBPS';
end;

procedure TForm1.GetMETAFromNet(Sender: TObject; Content : string);
begin
   Statusbar1.Panels[5].Text := Content;
end;

procedure TForm1.GetMIDILyric(Sender: TObject; TextP : pchar);
var
   RawLyricStr : string;
begin
   RawLyricStr := string(TextP);
   if RawLyricStr[1] = '\' then
   begin
      InfoMemo.Clear;
      InfoMemo.Lines.Add(copy(RawLyricStr, 2, length(RawLyricStr) - 1));
   end
   else if RawLyricStr[1] = '/' then
       InfoMemo.Lines.Add(copy(RawLyricStr, 2, length(RawLyricStr) - 1))
   else
      InfoMemo.Lines[InfoMemo.Lines.Count - 1] :=
         InfoMemo.Lines[InfoMemo.Lines.Count - 1] + RawLyricStr;
end;

procedure TForm1.DownloadEnded(Sender: TObject; Content : string);
begin
 // Now we can get the exact value of playback length of an internet stream.
   Label_Length.Caption := 'Length ' +
            FormatDateTime ('nn:ss', StrToInt(Content) / (1000 * 24 * 60 * 60));

 // We may have gotten correct 'Title' for playing stream file.
   if (BassPlayer1.StreamInfo.Title <> '') then
      Statusbar1.Panels[5].Text := BassPlayer1.StreamInfo.Title;
end;

// Display spectrum image
procedure TForm1.DisplayFFTBand(Sender: TObject; Bands : TBandOut);
var
   tmpRect, BarRect : TRect;
   j : integer;
begin
 // To prevent flickering, use temporary image buffer and process as follows
 // 1) Draw output image on temporary image buffer.
 // 2) Copy temporary image buffer's image to display canvas

 // Copy BasicBMP's image to GaugeTempBMP(used as temporary image buffer)
   BitBlt(GaugeTempBMP.Canvas.Handle, // handle to destination device context
          GaugeRect.Left,	// x-coordinate of destination rectangle's upper-left corner
          GaugeRect.Top,	// y-coordinate of destination rectangle's upper-left corner
          GaugeTempBMP.Width,	// width of destination rectangle
          GaugeTempBMP.Height,	// height of destination rectangle
          BasicBMP.Canvas.Handle, // handle to source device context
          GaugeRect.Left,	// x-coordinate of source rectangle's upper-left corner
          GaugeRect.Top,	// y-coordinate of source rectangle's upper-left corner
          SRCCOPY);             // Copies the source rectangle directly to the destination rectangle.
 // Substituted Canvas.CopyRect with BitBlt for speed up
 // GaugeTempBMP.Canvas.CopyRect(GaugeRect, BasicBMP.Canvas, GaugeRect);

 // Draw spectrum image to GaugeTempBMP
   for j := 1 to HBlockCount do
   begin
      if Bands[j-1] > VLimit then
         Bands[j-1] := VLimit;

      if Bands[j-1] > 0 then
      begin
     // Copy partial image of DisplayBar to GaugeTempBMP
        BarRect.Left := 0;
        BarRect.Right := BlockWidth;
        BarRect.Top := VLimit - Bands[j-1];
        if BarRect.Top < 0 then
           BarRect.Top := 0;
        BarRect.Bottom := DisplayBar.Height;

        tmpRect.Left := (BlockWidth + HBlockGap) * (j - 1) + 2;
        tmpRect.Right := tmpRect.Left + BlockWidth;
        tmpRect.Top := BarRect.Top;
        tmpRect.Bottom := BarRect.Bottom;

        BitBlt(GaugeTempBMP.Canvas.Handle,
               tmpRect.Left,
               tmpRect.Top,
               BlockWidth,
               tmpRect.Bottom - tmpRect.Top + 1,
               DisplayBar.Canvas.Handle,
               BarRect.Left,
               BarRect.Top,
               SRCCOPY);
      end;

      if Bands[j-1] >= trunc(PeakValue[j]) then
      begin
         PeakValue[j] := Bands[j-1] + 0.01;  // 0.01 : to compensate round off
         PassedCounter[j] := 0;
      end else if Bands[j-1] < trunc(PeakValue[j]) then
      begin
         if trunc(PeakValue[j]) > 0 then
         begin
            with GaugeTempBMP.Canvas do
            begin
            // Draw peak line
               Pen.Color := TColor(RGB(192, 192, 192));   // color for peak line
               MoveTo((BlockWidth + HBlockGap) * (j - 1) + 2, VLimit - trunc(PeakValue[j]));
               LineTo((BlockWidth + HBlockGap) * (j - 1) + 2 + BlockWidth, VLimit - trunc(PeakValue[j]));
            end;

      // Followings are to show simillar spectrum image to WINAMP's
      //  - Put delay time before lowering peak line
      //  - Accerate lowering speed according to the time elapsed
            if PassedCounter[j] >= 8 then
                PeakValue[j] := PeakValue[j] - 0.3 * (PassedCounter[j] - 8);

            if PeakValue[j] < 0 then
               PeakValue[j] := 0
            else
               inc(PassedCounter[j]);
         end;
      end;

   end;

 // Copy GaugeTempBMP's image to GaugePaintBox
   BitBlt(GaugePaintBox.Canvas.Handle,
          GaugeRect.Left,
          GaugeRect.Top,
          GaugeTempBMP.Width,
          GaugeTempBMP.Height,
          GaugeTempBMP.Canvas.Handle,
          GaugeRect.Left,
          GaugeRect.Top,
          SRCCOPY);
end;

// Show background image during non-playing period
procedure TForm1.ShowBackground;
begin
   BitBlt(GaugePaintBox.Canvas.Handle,
          GaugeRect.Left,
          GaugeRect.Top,
          BasicBMP.Width,
          BasicBMP.Height,
          BasicBMP.Canvas.Handle,
          GaugeRect.Left,
          GaugeRect.Top,
          SRCCOPY);
end;


procedure TForm1.btnPluginSetupClick(Sender: TObject);
begin
   BassPlayer1.ShowPluginConfigForm;
end;

procedure TForm1.EchoSliderChange(Sender: TObject);
begin
   BassPlayer1.EchoLevel := EchoSlider.Value;
end;

procedure TForm1.ReverbSliderChange(Sender: TObject);
begin
   BassPlayer1.ReverbLevel := ReverbSlider.Value;
end;

procedure TForm1.btnShowVisCtrlFormClick(Sender: TObject);
begin
   VisControlForm.Show;
end;

procedure TForm1.btnShowDSPCtrlFormClick(Sender: TObject);
begin
   DSPControlForm.Show;
end;

procedure TForm1.btnNetOpenClick(Sender: TObject);
begin
   Screen.Cursor := crHourGlass;
   if not BassPlayer1.PlayerReady then
   begin
      Application.MessageBox('Cannot open file (BassPlayer is not ready)',
                 'Confirm', MB_OK or MB_ICONINFORMATION);
      Screen.Cursor := crDefault;
      Statusbar1.Panels[1].Text := 'Stream Error.';
      exit;
   end;

   Statusbar1.Panels[5].Text := '';
   Label_Length.Caption := 'Length 00:00';
   if OpenStream(ComboBox1.Text) then
   begin
      if copy(BassPlayer1.DecoderName, 1, 5) = 'bass_' then
         AddonConfigForm.SetInUsePlugin(BassPlayer1.DecoderName)
      else
         AddonConfigForm.SetInUsePlugin('');
   end;
   Statusbar1.Panels[1].Text := 'Play';
   Screen.Cursor := crDefault;
   Statusbar1.SetFocus;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
   ProgDir := ExtractFilePath(ParamStr(0));
end;

procedure TForm1.btnFileInfoClick(Sender: TObject);
var
   s1, s2 : string;
begin
   if not BassPlayer1.PlayerReady then
   begin
      Application.MessageBox('Cannot open file (BassPlayer is not ready)',
                 'Confirm', MB_OK or MB_ICONINFORMATION);
      exit;
   end;

   OpenDialog1.FileName := '';
   s1 := BassPlayer1.NativeFileExts;
   s2 := BassPlayer1.PluginFileExts;
   OpenDialog1.Filter := 'All supported files |' + s1 + s2 + '|' +
                         'BASS native files (' + s1 + ')|' + s1 + '|' +
                         'Plug-in supported files (' + s2 + ')|' + s2 + '|';

 // Pre-select the opened stream file.
   if BassPlayer1.StreamName <> '' then
      if (not BassPlayer1.IsNetStream) and (not BassPlayer1.IsNetRadio) then
      begin
         OpenDialog1.InitialDir := ExtractFileDir(BassPlayer1.StreamName);
         OpenDialog1.FileName := ExtractFileName(BassPlayer1.StreamName);
      end;

   if OpenDialog1.Execute then
      if not BassPlayer1.FileInfoBox(OpenDialog1.FileName) then
         Application.MessageBox('Unsupported file type.', 'Information',
         MB_OK + MB_ICONINFORMATION);
end;

procedure TForm1.ProcessPluginRequest(Sender: TObject; GenParam : DWORD);
begin
   case TPluginRequest(GenParam) of
      REQ_VOLUMEUP : begin
                           if VolumeKnob.Position < VolumeKnob.Max then
                              if (VolumeKnob.Max - VolumeKnob.Position) > 4 then
                                 VolumeKnob.Position := VolumeKnob.Position + 4
                              else
                                 VolumeKnob.Position := VolumeKnob.Max;
                        end;
      REQ_VOLUMEDOWN : begin
                           if VolumeKnob.Position > 4 then
                              VolumeKnob.Position := VolumeKnob.Position - 4
                           else
                              VolumeKnob.Position := 0;
                        end;
      REQ_FFWD5S :   begin
                         //  if BassPlayer1.Mode = plmPlaying then
                              if (BassPlayer1.Position + 5000) < BassPlayer1.PlayLength then
                                  BassPlayer1.Position := BassPlayer1.Position + 5000;

                        end;
      REQ_REW5S :    begin
                         //  if BassPlayer1.Mode = plmPlaying then
                              if (BassPlayer1.Position - 5000) > 0 then
                                  BassPlayer1.Position := BassPlayer1.Position - 5000;

                        end;
     { REQ_PREV :
      REQ_NEXT : }
      REQ_PLAY :      BassPlayer1.Play;
      REQ_PAUSE :     BassPlayer1.Pause(true);
      REQ_STOP :      BassPlayer1.Stop;
   end;
end;

procedure TForm1.cbSingleChannelModeClick(Sender: TObject);
begin
   BassPlayer1.SingleChannel := cbSingleChannelMode.Checked;
   if cbSingleChannelMode.Checked then
      Application.MessageBox(WarningMsg, 'Warning', MB_OK + MB_ICONEXCLAMATION);
end;

procedure TForm1.btnAddonSetupClick(Sender: TObject);
begin
   AddonConfigForm.ShowModal;
end;

procedure TForm1.GetVisPluginInfo(Sender: TObject; VisWindowAttr : TVisPluginInfo);
begin
   CurVisWindowAttr := VisWindowAttr;
   VisControlForm.SetVisWindowAttr(CurVisWindowAttr);

   if (PageControl1.ActivePage = BasicSheet) and (CurVisWindowAttr.VisType <> OnAssignedWindow) then
      VisSheet.Caption := 'Visualization';

   if CurVisWindowAttr.VisHandle <> 0 then
   begin
      if CurVisWindowAttr.StartType then
         StartedInType := CurVisWindowAttr.VisType;
    //  WaitingPrevVisStop := false;
      ShowBackground;
   end {else
      if WaitingPrevVisStop then
         BassPlayer1.RunVisPlugin(Vis_Plugin_ToRun, SelectedModule)};
end;

procedure TForm1.cbMergeVisWindowClick(Sender: TObject);  // * Modified Ver 1.92
begin
   if cbMergeVisWindow.Checked then
      if IsWindow(VisPanel_Handle) then
         BassPlayer1.VisEmbedHandle := VisPanel_Handle
      else
         exit
   else
      BassPlayer1.VisEmbedHandle := 0;

   if StartedInType = CreatedByPlugin then
      exit;

 // if BassPlayer1.VisEMBEDSwitchMode = WindowMove then     
 // a. Panel mode -> discrete Window mode
 //   If EMBED window (by Gen_VisDrawer.dll or internal code) has not been created at
 //   starting current vis plug-in, then restart vis plug-in, else vis window is moved
 //   to previously created EMBED window by MoveToPrgramEMBED function in VisDrive.pas.
 // b. discrete Window mode -> Panel mode
 //   vis window is moved to Panel regardless of initial type of vis window.
   if CurVisWindowAttr.VisHandle <> 0 then   // Visualization is active ?
     if BassPlayer1.VisEMBEDSwitchMode = WindowMove then
     begin
       if not cbMergeVisWindow.Checked then
         if CurVisWindowAttr.VisType = OnAssignedWindow then
           if (StartedInType <> CreatedByGPP) and (StartedInType <> CreatedByCode) then
           begin
             Vis_Plugin_ToRun := CurVisWindowAttr.PluginPath;
             SelectedModule := CurVisWindowAttr.ModNo;
            { WaitingPrevVisStop := true;
             BassPlayer1.QuitVisPlugin; } // call BassPlayer1.RunVisPlugin indirectly
             BassPlayer1.RunVisPlugin(Vis_Plugin_ToRun, SelectedModule);
           end;
     end else
     begin   // for (BassPlayer1.VisEMBEDSwitchMode = NewStart)
  { if CurVisWindowAttr.VisHandle <> 0 then   // Visualization is active ?
   begin  }
       Vis_Plugin_ToRun := CurVisWindowAttr.PluginPath;
       SelectedModule := CurVisWindowAttr.ModNo;
      { WaitingPrevVisStop := true;
       BassPlayer1.QuitVisPlugin; }  // call BassPlayer1.RunVisPlugin indirectly
       BassPlayer1.RunVisPlugin(Vis_Plugin_ToRun, SelectedModule);
     end;
end;

// Following is test code which clears vis window.
// One of vis plug-in, vis_milk.dll shows a wrinkled image at showing up.
// Following code can be used to clean the plug-in's display surface for such plug-ins.
{procedure TMainForm.Button1Click(Sender: TObject);
var
   MyDC : HDC;
   r, r2 : TRect;
//   MyBitmap : HBITMAP;
   MyBrush : HBRUSH;
   OldObj : HGDIOBJ;
begin
   if ChildHandle <> 0 then
   begin
      MyDC := GetDC(ParentHandle);
      windows.GetClientRect(ParentHandle, r);
    //  MyBitmap := CreateCompatibleBitmap(MyDC, r.Right - r.Left, r.Bottom - r.Top);
      MyBrush := CreateSolidBrush(0);
    //  OldObj := SelectObject(MyDC, MyBitmap);
      OldObj := SelectObject(MyDC, MyBrush);

      r2.Left := CurVisWindowAttr.ClientPosX;
      r2.Top := CurVisWindowAttr.ClientPosY;
      r2.Right := r2.Left + r.Right - r.Left - CurVisWindowAttr.L_R_Margin;
      r2.Bottom := r2.Top + r.Bottom - r.Top - CurVisWindowAttr.T_B_Margin;
      windows.FillRect(MyDC, r2, MyBrush);

      SelectObject(MyDC, OldObj);
    //  DeleteObject(MyBitmap);
      DeleteObject(MyBrush);
      ReleaseDC(ParentHandle, MyDC);
   end;
end; }

procedure TForm1.PageControl1Change(Sender: TObject);
begin
   if (PageControl1.ActivePage = VisSheet) then
      VisSheet.Caption := 'Visualization';
end;

procedure TForm1.FormCanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
begin
 // The vis_avs.dll plug-in alters form size at display mode change (full screen mode -> windowed mode).
 // This code is to prevent such unintentional change of form size.
   Resize:= false;
end;

procedure TForm1.cbEMBEDSwitchModeClick(Sender: TObject);
begin
   if cbEMBEDSwitchMode.Checked then
      BassPlayer1.VisEMBEDSwitchMode := WindowMove
   else
      BassPlayer1.VisEMBEDSwitchMode := NewStart;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  BassPlayer1.Stop;
  Statusbar1.Panels[1].Text := 'Stop.';
  Statusbar1.SetFocus;
end;

end.
