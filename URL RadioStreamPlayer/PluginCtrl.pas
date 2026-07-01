unit PluginCtrl;

interface

uses
   Windows, SysUtils, Messages, Forms, ioplug, Dynamic_BASS, wa_ipc;

const
   MaxPluginNum = 8;                  // the maximum number of plug-ins, simultaneously loadable

   WM_WA_MPEG_EOF = WM_USER + 2;      // message from Winamp input plug-in, sent when stream reaches the end
   WM_StartPlay   = WM_USER + 101;    // message from output plug-in emulator at starting playback
   WM_BASS_StreamCreate = WM_USER + 105; // message from output plug-in emulator at executing BASS_StreamCreate
//   WM_BASS_StreamFree = WM_USER + 106;   // message from output plug-in emulator at executing BASS_StreamFree
   WM_GetToEnd    = WM_USER + 107;    // message to notify that BASS reaches the end of a stream
   WM_NewFFTData  = WM_USER + 108;    // message to notify that new FFT data are ready
   WM_PluginFirst_Changed = WM_USER + 109; // message from PluginConfigForm(=Input plug-in Configuration Form)
   WM_GetMeta     = WM_USER + 110;    // message to notify that new Shoutcast Metadata are ready
   WM_DownLoaded  = WM_USER + 111;    // message to notify that downloading of an internet stream is done.
   WM_GetChannelInfo = WM_USER + 112; // message from Winamp input plug-in at getting stream file's properties (sampling rate, ..)
   WM_GetChannelData = WM_USER + 114; // message for repeated sample data extraction from BASS decoding channel.
   WM_RequestFromVis = WM_USER + 115; // message to notify that a request from Vis plug-in received (* New at Ver 1.43)
   WM_GetLyric = WM_USER + 121;       // message to notify that a lyric event is encountered
   WM_GetHTTPHeaders = WM_USER + 122; // message to notify that BASS got the HTTP header
   FDataReadyMsg = WM_USER + 123;     // message to notify that there are data to be transfered in DataBuffer.

   MaxVolume = 255;          // Winamp input plug-in volume range : 0 ~ 255
   ChannelLimit = 8;           // Allows maximum 8 channels in a stream

 // error types related loading & unloading of Winamp input plug-ins
   ERROR_OMOD_NOTREADY  = 1;   // output plug-in emulator is not ready
   ERROR_LOADED_BEFORE  = 2;   // already loaded before
   ERROR_LOADED_FULL    = 3;   // no space to load
   ERROR_CANNOT_LOAD    = 4;   // uncertain error at loading
   ERROR_INVALID_PLUGIN = 5;   // not a Winamp input plug-in
   ERROR_CANNOT_UNLOAD  = 6;   // uncertain error at unloading
   ERROR_NOT_LOADED     = 7;   // not loaded yet
   ERROR_IN_USE         = 8;   // specified plug-in is in use

   maxVismodNum = 8;
   maxDSPmodNum = 8;

 // constants for the communication between threads related to driving vis plug-in
   DataReady = 10;
   QuitProg = 20;
 //  PlayerStatus = 30;
   MinimizeWindow = 40;
   ChangeEmbedWindow = 45;  // * New at Ver 1.43
   ChangeEMBEDSwitchMode = 46; // * New at Ver 1.43
   RestoreWindow = 50;
   VisModuleLoaded = 55;
   UnloadVisPlugin = 56;    // * New at Ver 1.43
   RequestRestFlag = 57;    // * New at Ver 1.43
   StartVisOut = 60;
   PauseVisOut = 65;
   UseGPPVisDrawer = 66;    // * New at Ver 1.43
   EmbedWindowChanged = 68; // * New at Ver 1.43
   EndVisOut = 70;

   InformPlayerMode = 81;
   InformStreamInfo = 82;
 //  InformSyncWindows = 83;

   CLASSNAME_WINAMP_VIS_DRAWER = 'Vis Drawer';

type
   TChannelType = (Channel_NotOpened, Channel_Stream, Channel_CD, Channel_WMA, Channel_Music,
                   Channel_MIDI, Channel_Plugin);
   TPluginRequest = (REQ_VOLUMEUP, REQ_VOLUMEDOWN, REQ_FFWD5S, REQ_REW5S,
                     REQ_PREV, REQ_NEXT, REQ_PLAY, REQ_PAUSE, REQ_STOP);  // * New at Ver 1.43

   TPlugin = ^TIn_module;
   TPluginInfo = record
      Name : string;
      Version : integer;        // module type (IN_VER)
      Description : string;     // description of module, with version string
      FileExtensions : string;
      Seekable : integer;
      DLLHandle : THandle;
   end;
   TChannelInfo = record
      BitsPerSample : word;     // Bits per sample
      BitRate   : LongInt;
      SampleRate : LongInt;
      Channels : Word;
   end;
   PChannelInfo = ^TChannelInfo;

   TVismod = array[0..maxVismodNum-1] of PWinAmpVisModule;
   TDSPmod = array[0..maxDSPmodNum-1] of PWinAmpDSPModule;
   TVisModuleInfo = record
     latencyMs : Cardinal;
     delayMs : Cardinal;
     spectrumNch : Cardinal;
     waveformNch : Cardinal;
   end;


 // TVisWindowIs defines the type of vis window
   TVisWindowIs = (UnAvailable, CreatedByPlugin, CreatedByGPP,
                                CreatedByCode, OnAssignedWindow); // * Added at 1.43
     // CreatedByPlugin : vis window is created by plug-in in use.
     // CreatedByGPP : vis window is created by Gen_VisDrawer.DLL ( similar shape as Winamp )
     // CreatedByCode : vis window is created by interal code of this unit. ( normal shape,
     //                  seperate window )
     // OnAssignedWindow : vis window is created by interal code of this unit as a child
     //                  window of user specified window(ex. panel of main form).
     //                  I named the display mode by this type of vis window "Panel Mode".

 // TVisEMBEDSwitchMode defines the method of switching EMBED window for running vis plug-in.
   TVisEMBEDSwitchMode = (NewStart, WindowMove);     // * Added at 1.43
     // NewStart : vis plug-in is quitted first, then restarted with newly specified EMBED window.
     // WindowMove : vis window is transfered to newly specified EMBED window.

   TVisPluginInfo = record   // * Modified & Renamed at 1.43
     ThreadId : DWORD;       // Thread identifier
     VisHandle : DWORD;      // Window handle to vis window
     VisType : TVisWindowIs; // Type of vis window
     PluginPath : pchar;     // File path to the vis plug-in
     ModNo   : word;         // Number of the selected module in the vis plug-in
     StartType : boolean;    // True : VisType entry has the starting display mode
   end;

   TPlayerMode = (plmStandby, plmReady, plmStopped, plmPlaying, plmPaused);
   TStreamInfo = record
     FileName : string;
     FileSize : DWORD;        // File size in byte
     SampleRate : DWORD;      // Sampling rate in Hz
     BitRate : DWORD;         // Bit Rate in KBPS
     BitsPerSample : Word;    // Bits per sample
     Duration : DWORD;        // playback duration in mili second
     Channels : Word;         // 1- Mono, 2 - Stereo

     Title : string;
     Artist : string;
     Album : string;
     Year : string;
     Genre : string;
     GenreID : byte;
     Track : byte;
     Comment : string;
  end;

  procedure WinProcessMessages;
  function GetProgDir : string;

// Input & output plug-in related functions
  function  OutputReady : boolean;
  procedure SetReachedEnd;
  function  SelectInputPlugin(PluginNum : integer) : integer;
  function  ActivePlugin : string;
  procedure SetPosChanged;
  function  GetOutputTime(omodTime : boolean) : integer;
  function  GetPluginIndex(PluginName : string) : integer;
  function  GetPlugin(PluginNum : integer) : TPlugin;
  function  GetPluginInfo(PluginNum : integer) : TPluginInfo;
  function  IsWinampPluginLoaded(PluginName : string) : boolean;
  function  LoadWinampPlugin(PluginName : string) : integer;
  function  UnloadWinampPlugin(PluginName : string) : integer;
  function  GetPlayableFiles : string;
  function  GetPluginNumber(ExtCode : string) : integer;
  function  InitPluginCtrl(MainHandle : HWND) : boolean;
  procedure QuitPluginCtrl;

// Following 8 procedures/functions are to control BASS channel.
  procedure SetFlangeParams;
  procedure Flange(handle: HSYNC; channel: DWORD; buffer: Pointer; length, user: DWORD); stdcall;
  function  omodWrite2 : integer;
  function  omodOpen2(Channel : DWORD; samplerate, numchannels, bitspersamp : integer) : DWORD;
  procedure omodClose2;
  procedure ClearBuffer;
  function  DataInBuffer : DWORD;
  function  GetPlayedTime : DWORD;

// Visualization plug-in related functions
  function  VisActive : boolean;
  procedure LoadVisModule(PluginPath : string;
                          var Visheader : PWinampVisHeader;
                          var Vismod : TVismod;
                          var NumVismod : integer;
                          ParentHandle : HWND);
  function  StartVisModule(ModuleNum : word) : integer;
  function  UnloadVisModule : integer;
  procedure SetVisualizationQuitted;
  procedure SetVisOutHandle(VisOutHandle : HWND);

// DSP plug-in related functions
  function  DSPBufferReady : boolean;
  function  DSPActive : boolean;
  procedure LoadDSPModule(PluginPath : string;
                          var DSPheader : PWinampDSPHeader;
                          var DSPmod : TDSPmod;
                          var NumDSPmod : integer{;
                          ParentHandle : HWND});
  function  StartDSPModule(ModuleNum : word;
                          ParentHandle : HWND) : integer;
  function  StopDSPModule : integer;
  function  UnloadDSPModule : integer;
  procedure SetStreamInfo2(Stream : TStreamInfo);
  procedure SetPlayerMode2(Mode : TPlayerMode);

// * Added at Ver 1.43
  procedure SetChannelInfo2(ChannelType : TChannelType; D_ChannelId, P_ChannelId,
                                        SampleRate, Channels : DWORD);
  function GPPActive : boolean;
  function StartGPPModule(ParentHandle : HWND;       // * Modified at Ver 1.43
                          DriveThreadId : DWORD;
                          InitWinPos : TInitWinPos) : HWND;
  procedure SetGPPInactive;

  function GetBufferAddress : pointer;  // * Added at Ver 1.43

implementation


var
   pBuf : PBYTE;
   bufSize : DWORD = 0;
   DSPBuffer : PBYTE;
   DSPBufferSize : DWORD = 0;
   omod : ^TOut_module;
   imod : ^TIn_module;
   omod1 : TOut_module;
   imodNum   : integer;
   omodReady : boolean = false;
   Plugin : array[0..MaxPluginNum-1] of TPlugin;
   PluginInfo : array[0..MaxPluginNum-1] of TPluginInfo;
   hMainHandle : HWND;	 // handle to main message procedure in BASSPlayer.pas

   HBASSStream : HSTREAM = 0;
   ChannelInfo : TChannelInfo;
   PosChanged : boolean;

   PlayThreadId : DWORD = 0;
   CloseRequested : boolean;
   PlayThreadStarted : boolean;
   PlayChannel : HSTREAM = 0;
   DecodeChannel : DWORD;
 //  NeedData : boolean = false;
   totalGet : int64;

   CurVismod  : TVismod;
   VismodNum : integer = 0;
   VismodIndex : integer;
   VisIsActive : boolean = false;
   VisualizationQuitted : boolean = false;
   hVisOutWindow : HWND = 0; // handle to vis window

   CurDSPmod  : TDSPmod;
   DSPmodNum : integer = 0;
   DSPmodIndex : integer;
   DSPIsActive : boolean = false;

   GPPIsActive : boolean = false;

 // variables for Winamp IPC message handling
   hInst : HWND;             // handle to program (hinstance)
   hMainWindow : HWND;       // handle to Main window
   hFakeWindow : HWND = 0;   // handle to fake Winamp window
   ReturnStr : string[254];
   PlayerMode : TPlayerMode;
   StreamInfo : TStreamInfo;

 // * Added for function SetChannelInfo2 & PlaybackPosition
   FChannelType : TChannelType;
   FD_ChannelId : DWORD;
   FP_ChannelId : DWORD;
   FSampleRate : DWORD;
   FChannels : DWORD;

   DataBuffer : array[0..2857] of byte; // * Added at Ver 1.43

function GetBufferAddress : pointer;  // * Added at Ver 1.43
begin
   result := @DataBuffer[0];
end;

procedure WinProcessMessages;
// Allow Windows to process other system messages
var
    ProcMsg  :  TMsg;
begin
    while PeekMessage(ProcMsg, 0, 0, 0, PM_REMOVE) do begin
      if (ProcMsg.Message = WM_QUIT) then Exit;
      TranslateMessage(ProcMsg);
      DispatchMessage(ProcMsg);
    end;
end;

function PlaybackPosition : DWORD;
var
   SongPos : int64;
   FloatPos : FLOAT;
begin
   case FChannelType of
      Channel_NotOpened : SongPos := 0;
      Channel_CD, Channel_WMA, Channel_Stream, Channel_Music, Channel_MIDI : begin
                          if (FD_ChannelId = FP_ChannelId) then  // Single channel mode ?
                             SongPos := BASS_ChannelGetPosition(FD_ChannelId, BASS_POS_BYTE)  // * Changed at 1.9
                          else
                             SongPos := BASS_ChannelGetPosition(FD_ChannelId, BASS_POS_BYTE) - DataInBuffer;   // * Changed at 1.9
                       end;
      Channel_Plugin : begin
                          if PlayerMode = plmReady then
                             result := 0
                      // Output plug-in's time returns actual elapsed time based on the amount
                      // of sample data output, i.e., not the playback position in a stream
                      // beging played. So we may get different result if we run DSP plug-ins which
                      // returns less or more samples than the amount of source samples.
                         { else if UsesOutputPlugin then
                             result := GetOutputTime(true)  // Output plug-in's time  }
                          else
                             result := GetOutputTime(false); // Input plug-in's time
                          exit;
                       end;
      else
         SongPos := 0;
   end;

   if SongPos <= 0 then
   begin
      result := 0;
      exit;
   end;

   FloatPos := BASS_ChannelBytes2Seconds(FD_ChannelId, SongPos);
   result := round(1000 * FloatPos);     // sec -> milli sec
end;

//---------- procedures & functions for Winamp input plug-in --------------------

procedure SetInfo1(bitrate, srate, stereo, synched : integer); cdecl; // if -1, changes ignored? :)
begin
   if (bitrate = -1) and (srate = -1) and (stereo = -1) then
      exit;

   if bitrate <> -1 then
      ChannelInfo.BitRate := bitrate;
   if srate <> -1 then
      ChannelInfo.SampleRate := srate;
   if stereo <> -1 then
      ChannelInfo.Channels := stereo;

   PostMessage(hMainHandle, WM_GetChannelInfo, 0, longint(@ChannelInfo));
end;

function dsp_isactive1 : integer; cdecl;
begin
   if DSPIsActive then
      result := 1
   else
      result := 0;
end;

function dsp_dosamples1(samples : pointer; numsamples, bps, nch, srate : integer) : integer; cdecl;
var
  rtnsamples : integer;
begin
   result := numsamples;

   if DSPIsActive then
   begin
      rtnsamples := CurDSPmod[DSPmodIndex].ModifySamples(CurDSPmod[DSPmodIndex], samples, numsamples, bps, nch, srate);
      if rtnsamples > 0 then
         result := rtnsamples;
   end;
end;

procedure SAVSAInit1(maxlatency_in_ms : integer; srate : integer); cdecl;  // call in omod.Play()
begin
end;

procedure SAVSADeInit1;cdecl;	// call in omod.Stop()
begin
end;

procedure SAAddPCMData1(PCMData: pointer; nch: integer; bps: integer; timestamp: integer); cdecl;
begin
end;

// gets csa (the current type (4=ws,2=osc,1=spec))
function SAGetMode1: integer; cdecl;
begin
   result := 0;
end;

// sets the spec data, filled in by winamp
procedure SAAdd1(data: pointer; timestamp: integer; csa: integer); cdecl;
begin
end;

// sets the vis data directly from PCM data
procedure VSAAddPCMData1(PCMData: pointer; nch: integer; bps: integer; timestamp: integer); cdecl;
begin
end;

// use to figure out what to give to VSAAdd
function VSAGetMode1(var specNch : integer; var waveNch : integer) : integer; cdecl;
begin
   result := 0;
end;

// filled in by winamp, called by plug-in
procedure VSAAdd1(data : pointer; timestamp : integer); cdecl;
begin
end;

procedure VSASetInfo1(srate : integer; nch : integer); cdecl;
begin
end;


//---------- procedures & functions to emulate Winamp output plug-in -----------

const
   PacketSize = 1152;
   WinampBaseSize = 576;  // Winamp input plug-in's base size of data transfering
   DefaultBufSize = 88200; // buffer size to hold 0.5sec playing data for 44100Hz, 16bit, stereo stream

var
   rOffset : dword;
   wOffset : dword;
   SPS : integer;       // samplerate (samples per second)
   BPS : integer;       // bits per sample
   Channels : integer;  // number of channels (MONO = 1, STEREO = 2)
   BASSDataSize : dword;
   isReading : boolean;
   isWriting : boolean;
   posDelta : integer;
   totalWritten : int64;
   InitialBufferFill : boolean;
   ReachedEnd : boolean;

   ResumeMode : boolean;

// calulate free space of buffer (The buffer pointed by pBuf to hold sound data
// is used as ring buffer)
function FreeSpace(BufferSize, ReadOffset, WriteOffset : dword) : dword;
begin
   if ReadOffset > WriteOffset then
      result := ReadOffset - WriteOffset
   else
      result := BufferSize  - WriteOffset + ReadOffset;
end;

// calculate the amount of data in buffer
function DataRemains(BufferSize, ReadOffset, WriteOffset : dword) : dword;
begin
   if ReadOffset > WriteOffset then
      result := BufferSize  + WriteOffset - ReadOffset
   else
      result := WriteOffset - ReadOffset;
end;

// Message handlers are merged into one message handler in BASSPlayer.pas
//  to prevent timing problems.
{ procedure TMsgHandler.ProcMessage(var Msg: TMessage);
begin
   case Msg.Msg of
      WM_StreamOpen : begin
                         hMainHandle := Msg.wParam;  // main window's message handle
                         imodNum := Msg.lParam;      // index number of Winamp input plug-in to use
                      end;
 //     WM_StreamClose : imodNum := -1;   // -1 : No plug-ins is in use
 //     WM_PosChange  : PosChanged := true;
      WM_StartPlay : begin
                        BASS_ChannelPlay(HBASSStream, false);
                        ResumeMode := false;
                        if hMainHandle <> 0 then
                           PostMessage(hMainHandle, WM_StartPlay, Msg.wParam, Msg.lParam);
                     end;
      WM_WA_MPEG_EOF : begin
                          ReachedEnd := true;
                          if hMainHandle <> 0 then
                             PostMessage(hMainHandle, WM_WA_MPEG_EOF, 0, 0);
                       end;
      WM_GetChannelData : omodWrite2;
      WM_QueryEndSession : Msg.Result := 1;    // Allow system termination
   end;
end; }


// Stream writing callback function for BASS
function StreamWriter(handle : HSTREAM;
                      buffer : pointer;
                      length : DWORD;
                      user : DWORD) : DWORD; stdcall;
// The value of 'length'(data transfering size) is as follows for some cases
//  SPS : 48000, BPS : 16, Channels : 2 -> 38400
//  SPS : 44100, BPS : 16, Channels : 2 -> 35280
//  SPS : 22050, BPS : 8,  Channels : 1 -> 4410
var
   p, p2 : pointer;
   dw : dword;
   wCycle : word;
   OutputTime : DWORD;
begin
 // Let BASS wait until sound data is ready.
 // This waiting routine is not necessary in most cases.
 // This routine is added for the Winamp input plug-in "in_midi.dll" (v2.64,
 // written by Mr. Peter Pawlowski) which takes long time to get sound data after
 // changing play position.
   if not ReachedEnd then
      if DataRemains(bufSize, rOffset, wOffset) < length then
      begin
      // Waiting cycle is allowed only when playback position is not at the near end
      // of a stream file.
      // I have found that in_speex.dll v0.7.5a sounds out echo(= repeats playback of
      // internal buffer) at the near end of a stream file. ( I think it is caused by
      // delayed message-out of WM_WA_MPEG_EOF message from in_speex.dll )
         OutputTime := posDelta + round(1000 * totalWritten / (SPS * (BPS SHR 3) * Channels));
         if (StreamInfo.Duration > OutputTime) and   // OutputTime may be greater than StreamInfo.Duration
            ((StreamInfo.Duration - OutputTime) > 2000) then
         begin
            wCycle := 0;
            while DataRemains(bufSize, rOffset, wOffset) < length do
            begin
               WinProcessMessages;
               Sleep(50);
               inc(wCycle);
               if wCycle = 60 then     // 3 sec time out
                  break;
               if ReachedEnd then
                  break;
            end;
            ResumeMode := false;
         end;
      end;

   while isWriting do  // avoid duplicate access on buffer
      begin
         WinProcessMessages;
         sleep(20);
      end;

   isReading := true;  // notify that BASS is reading buffer data
   dw := dword(pBuf) + rOffset;
   p := pointer(dw);   // p : starting address to read

   if rOffset > wOffset then
      if (bufSize - rOffset) > length then
      begin
         Move(p^, buffer^, length);
         inc(rOffset, length);
         result := length;
      end
      else if (bufSize - rOffset) = length then
      begin
         Move(p^, buffer^, bufSize - rOffset);
         rOffset := 0;
         result := length;
      end else // (bufSize - rOffset) < length
      begin
         Move(p^, buffer^, bufSize - rOffset);
         dw := dword(buffer) + bufSize - rOffset;
         p2 := pointer(dw);
         if (length - (bufSize - rOffset)) < wOffset then
         begin
            Move(pBuf^, p2^, length - (bufSize - rOffset));
            rOffset := length - (bufSize - rOffset);
            result := length;
         end else
         begin
            Move(pBuf^, p2^, wOffset);
            rOffset := wOffset;
            result := bufSize - rOffset + wOffset {+ BASS_STREAMPROC_END};
         end;
      end
   else if rOffset < wOffset then
      if (wOffset - rOffset) >= length then
      begin
         Move(p^, buffer^, length);
         inc(rOffset, length);
         result := length;
      end else
      begin
         Move(p^, buffer^, wOffset - rOffset);
      // signify that the end of the stream is reached
         result := wOffset - rOffset {+ BASS_STREAMPROC_END};
         rOffset := wOffset;
      end
   else   // rOffset = wOffset : no data in buffer
      result := BASS_STREAMPROC_END{0};

 // Let BASS wait until sound data is ready for long latency devices such CDROM
 // (for the delay time drive's spindle motor gets normal speed after stopping)
   if ResumeMode then
      if DataRemains(bufSize, rOffset, wOffset) < length then
      begin
         wCycle := 0;
         while not isWriting do  // until data is written
         begin
            WinProcessMessages;
            Sleep(200);
            inc(wCycle);
            if wCycle = 50 then  // 10 sec time out
                break;
         end;
         ResumeMode := false;
      end;

   isReading := false;
end;


procedure omodConfig(hwndParent : hwnd); cdecl; // configuration dialog
begin
   Application.MessageBox('No configuration is needed.', 'Confirm', MB_OK);
end;

procedure omodAbout(hwndParent : hwnd); cdecl;  // about dialog
begin
   Application.MessageBox('This is a Winamp output plug-in emulator using BASS',
                          'Confirm', MB_OK);
end;

procedure omodInit; cdecl;     // called when loaded
begin
   if omodReady then
      exit;

   omodReady := true;
end;

procedure omodQuit; cdecl;    // called when unloaded
begin
   if omodReady then
      omodReady := false; 

end;

function omodOpen(samplerate, numchannels, bitspersamp, bufferlenms,
                                           prebufferms : integer) : integer; cdecl;
// returns >=0 on success, <0 on failure
// called by input plug-in just before starting playback
var
   flags : DWORD;
begin
   if not omodReady then
   begin
      result := -1;
      exit;
   end;

   if bitspersamp = 8 then
      flags := BASS_SAMPLE_8BITS
   else
      flags := 0;

   HBASSStream := BASS_StreamCreate(samplerate,
                                    numchannels,
                                    flags,
                                    @StreamWriter,
                                    nil {pointer to user data});
   if HBASSStream = 0 then   // 0 : error
      result := -1
   else
   begin
      SPS := samplerate;
      BPS := bitspersamp;
      Channels := numchannels;
  // BASSDataSize : The data size of each transfering to BASS (= amount to play 0.2sec period)
      BASSDataSize := (SPS * (BPS div 8) * CHANNELS * 2) div 10;

  // Readjust buffer size if previously allocated amount is out of reasonable range.
      if ((BASSDataSize * 2) > bufSize) or ((BASSDataSize * 4) < bufSize) then
      begin
         if (pBuf <> nil) then
            FreeMem(pBuf);
         pBuf := nil;
         bufSize := 0;
         GetMem(pBuf, BASSDataSize * 3); // Get memory to hold sound data from Winamp input plug-in
         if (pBuf <> nil) then
            bufSize := BASSDataSize * 3
         else begin
            result := -1;
            exit;
         end;
      end;

      posDelta := 0;
      if hMainHandle <> 0 then
         PostMessage(hMainHandle, WM_BASS_StreamCreate, HBASSStream, 0);
      result := 0;
   end;

   rOffset := 0;
   wOffset := 0;
   totalWritten := 0;
   isReading := false;
   isWriting := false;
   PosChanged := false;
   InitialBufferFill := true;
   ReachedEnd := false;
end;

procedure omodClose; cdecl;   // close the ol' output device.
begin
   if BASS_ChannelIsActive(HBASSStream) = BASS_ACTIVE_PLAYING then
      BASS_ChannelStop(HBASSStream);
   BASS_StreamFree(HBASSStream);
  // if not ReachedEnd then
    {  if hMainHandle <> 0 then
         PostMessage(hMainHandle, WM_BASS_StreamFree, 0, 0); }
   HBASSStream := 0;
end;

function omodWrite(buf : pointer; len : integer) : integer; cdecl;
// 0 on success. Len == bytes to write (<= 8192 always).
// 1 returns not able to write (yet). Non-blocking, always.

var
   dw : dword;
   p, p2 : pointer;
begin
   while isReading do
      begin
         WinProcessMessages;
         sleep(20);
      end;

   isWriting := true;  // notify that Winamp input plug-in is writing sound data
   dw := dword(pBuf) + wOffset;
   p := pointer(dw);   // p : starting address to write

   if FreeSpace(bufSize, rOffset, wOffset) > DWord(len) then
   begin
      if rOffset > wOffset then
      begin
         Move(buf^, p^, len);
         inc(wOffset, len);
      end
      else
         if (bufSize - wOffset) > DWord(len) then
         begin
            Move(buf^, p^, len);
            inc(wOffset, len);
         end else
         begin
            Move(buf^, p^, bufSize - wOffset);
            if (bufSize - wOffset) < DWord(len) then
            begin
               dw := dword(buf) + (bufSize - wOffset);
               p2 := pointer(dw);
               Move(p2^, pBuf^, DWord(len) - (bufSize - wOffset));
            end;
            wOffset := DWord(len) - (bufSize - wOffset);
         end;

      inc(totalWritten, len);
      result := 0;
   end else
      result := 1;      // This case may happen at using DSP plug-in

   isWriting := false;

 // Let BASS start playing after initial buffer filling
   if InitialBufferFill then
      if (FreeSpace(bufSize, rOffset, wOffset) <= 8192) or
         (DataRemains(bufSize, rOffset, wOffset) >= (BASSDataSize SHL 1)) then
      begin
         InitialBufferFill := false;
         if HBASSStream <> 0 then
         begin
            ResumeMode := false;
            BASS_ChannelPlay(HBASSStream, true);
            ChannelInfo.BitsPerSample := BPS;
            ChannelInfo.SampleRate := SPS;
            ChannelInfo.Channels := Channels;
            if hMainHandle <> 0 then
               PostMessage(hMainHandle, WM_StartPlay, 0, longint(@ChannelInfo));
         end;
      end;
end;

function omodCanWrite: integer; cdecl;
// returns number of bytes possible to write at a given time.
// Never will decrease unless you call Write (or Close, heh)
begin
   if isReading then
   begin
      result := 0;
      exit;
   end;

 // Subtract 1 from real free space to prevent wOffset become equal to rOffset
 // after writing data.  (it means there is no data in buffer if rOffset equals
 //  to wOffset)
   result := FreeSpace(bufSize, rOffset, wOffset) - 1;
end;

function omodIsPlaying : integer; cdecl;
// non0 if output is still going or if data in buffers waiting to be
// written (i.e. closing while IsPlaying() returns 1 would truncate the song
begin
   if BASS_ChannelIsActive(HBASSStream) = BASS_ACTIVE_PLAYING then
      result := 1
   else
      result := 0;
end;

function omodPause(pause : integer) : integer; cdecl;
// returns previous pause state
begin
   if BASS_ChannelIsActive(HBASSStream) = BASS_ACTIVE_PAUSED then
      result := 1
   else
      result := 0;

   if (pause = 0) and (result = 1) then
   begin
      ResumeMode := true;
      BASS_ChannelPlay(HBASSStream, false);
   end

// This case will happen in the sequence, Pause -> position change -> resume
   else if (pause = 0) then
   begin
      ResumeMode := true;
      BASS_ChannelPlay(HBASSStream, true{flush})
   end else if (pause <> 0) and (result = 0) then
      BASS_ChannelPause(HBASSStream);
end;

procedure omodSetVolume(volume : integer); cdecl; // volume is 0-255
var
   SetVolume : integer;
begin
   if Volume < 0 then
      SetVolume := 0
   else if Volume > MaxVolume then
      SetVolume := MaxVolume
   else
      SetVolume := Volume;

   BASS_SetConfig(BASS_CONFIG_GVOL_STREAM, SetVolume * 39); // * Changed at 1.93
end;

procedure omodSetPan(pan : integer); cdecl;       // pan is -128 to 128
begin
 //  BASS_ChannelSetAttributes(HBASSStream, -1, -1, round(pan * (100 / 128)));
   BASS_ChannelSetAttribute(HBASSStream, BASS_ATTRIB_PAN, pan * (100 / 128)); // * Changed at 1.43
end;

procedure omodFlush(t : integer); cdecl;
// flushes buffers and restarts output at time t (in ms) (used for seeking)
// This procedure is called by input plug-in when position is changed
var
   chnStatus : dword;
   wasPlaying : boolean;
begin
   chnStatus := BASS_ChannelIsActive(HBASSStream);
   if (chnStatus <> BASS_ACTIVE_PAUSED) and
      (chnStatus <> BASS_ACTIVE_STOPPED) then
      if (FreeSpace(bufSize, rOffset, wOffset) > 0) or
         (chnStatus = BASS_ACTIVE_PLAYING) then
      begin
         wasPlaying := true;
      end else
         wasPlaying := false
   else
      wasPlaying := false;

 // Normally this omod.Flush procedure is called by Winamp input plug-in when
 // play position is changed but "in_midi.dll" (v2.64, written by Mr. Peter
 // Pawlowski) calls this procedure when it starts play after pause without
 // position change.
 // The variable PosChanged is used to know if the position has been changed or not.
 // Set PosChanged true in the main program if position is cahnged.
   if chnStatus = BASS_ACTIVE_PAUSED then
      if not PosChanged then  // if position has not been changed then escape
         exit;

   while isReading do
      begin
         WinProcessMessages;
         sleep(20);
      end;

 // Use BASS_ChannelStop regardless the status of BASS_Channel to flush the buffer in BASS
   BASS_ChannelStop(HBASSStream);

   if wasPlaying then
      InitialBufferFill := true;   // Reset to restart after seeking

   posDelta := t;
   rOffset := 0;
   wOffset := 0;
   totalWritten := 0;
   isWriting := false;
   PosChanged := false;
end;

function omodGetOutputTime : integer; cdecl;  // returns played time in MS
begin
   result := posDelta +
   //  round(1000 * BASS_ChannelGetPosition(HBASSStream) / (SPS * (BPS SHR 3) * Channels));
       round(1000 * BASS_ChannelGetPosition((HBASSStream), BASS_POS_BYTE) / (SPS * (BPS SHR 3) * Channels));
     // * Changed at 1.43
end;

function omodGetWrittenTime : integer; cdecl;
// returns time written in MS (used for synching up vis stuff)
begin
   result := posDelta + round(1000 * totalWritten / (SPS * (BPS SHR 3) * Channels));
end;


procedure SetOutputPlugin;
begin
   omod := @omod1;
   omod.version := $10;
   omod.description := 'BASS Winamp output plug-in emulator';
   omod.id := 65536 + 1;
   omod.hMainWindow := Application.Handle;
   omod.hDllInstance := 0;
   omod.Config := omodConfig;
   omod.About := omodAbout;
   omod.Init := omodInit;
   omod.Quit := omodQuit;
   omod.Open := omodOpen;
   omod.Close := omodClose;
   omod.Write := omodWrite;
   omod.CanWrite := omodCanWrite;
   omod.IsPlaying := omodIsPlaying;
   omod.Pause := omodPause;
   omod.SetVolume := omodSetVolume;
   omod.SetPan := omodSetPan;
   omod.Flush := omodFlush;
   omod.GetOutputTime := omodGetOutputTime;
   omod.GetWrittenTime := omodGetWrittenTime;

   omod.Init;
end;

function GetProgDir : string;
begin
   result := ExtractFilePath(ParamStr(0));
end;

procedure SetReachedEnd;
begin
   ReachedEnd := true;
end;


//------------------------ BASS dual channel mode support -----------------------

// Following statements for flanger effect is adopted from DTMain.pas of BASS16
// package

const
   FLABUFLEN = 350;         // buffer length for flanger effect

var
   // Variables for DSP (flanger effect) implementation
   flabuf : array[0..FLABUFLEN-1, 0..2] of SmallInt;  // buffer
   flapos : Integer;         // cur.pos
   flas, flasinc : FLOAT;    // sweep pos/min/max/inc

procedure SetFlangeParams;
begin
   FillChar(flabuf, SizeOf(flabuf), 0);
   flapos := 0;
   flas := FLABUFLEN / 2;
   flasinc := 0.002;
end;

function fmod(a, b: FLOAT): FLOAT;
begin
   Result := a - (b * Trunc(a / b));
end;

function Clip(a: Integer): Integer;
begin
   if a <= -32768 then
      a := -32768
   else if a >= 32767 then
      a := 32767;

   Result := a;
end;

procedure Flange(handle: HSYNC; channel: DWORD; buffer: Pointer; length, user: DWORD); stdcall;
var
  lc, rc: SmallInt;
  p1, p2, s: Integer;
  d: ^DWORD;
  f: FLOAT;
begin
  d := buffer;
  while (length > 0) do
  begin
    lc := LOWORD(d^); rc := HIWORD(d^);
    p1 := (flapos + Trunc(flas)) mod FLABUFLEN;
    p2 := (p1 + 1) mod FLABUFLEN;
    f := fmod(flas, 1.0);
    s := lc + Trunc(((1.0-f) * flabuf[p1, 0]) + (f * flabuf[p2, 0]));
    flabuf[flapos, 0] := lc;
    lc := Clip(s);
    s := rc + Trunc(((1.0-f) * flabuf[p1, 1]) + (f * flabuf[p2, 1]));
    flabuf[flapos, 1] := rc;
    rc := Clip(s);
    d^ := MakeLong(lc, rc);
    Inc(d);
    Inc(flapos);
    if (flapos = FLABUFLEN) then flapos := 0;
    flas := flas + flasinc;
    if (flas < 0) or (flas > FLABUFLEN) then
      flasinc := -flasinc;
    length := length - 4;
  end;
end;

function omodWrite2 : integer;
// 0 on success.
// 1 on failure

var
   dw : dword;
   ReqSpace, RetSamples : dword;
   ReqSize, GetSize, RetSize : dword;
   p, p2 : pointer;
   ChannelStat : dword;
begin
   result := 1;

   if PlayChannel = 0 then
      exit;
   if ReachedEnd then
      exit;

   while isReading do
      begin
         WinProcessMessages;
         sleep(20);
      end;

   isWriting := true;

 // determine transfering size in bytes.
   ReqSize := 576 * Channels;
   if BPS > 8 then
      ReqSize := ReqSize * 2;
   if SPS > 22050 then
      ReqSize := ReqSize * 2;
   if DSPIsActive then
      ReqSpace := ReqSize * 2  // returned samples may be up to twice than original.
   else
      ReqSpace := ReqSize;

   if FreeSpace(bufSize, rOffset, wOffset) < ReqSpace then
   begin
      isWriting := false;
      exit;
   end;

   GetSize := BASS_ChannelGetData(DecodeChannel, DSPBuffer, ReqSize);
   if GetSize > 0 then
   begin
      inc(totalGet, GetSize);

      if DSPIsActive then
      begin
         try
           RetSamples := CurDSPmod[DSPmodIndex].ModifySamples(CurDSPmod[DSPmodIndex],
                                      DSPBuffer, GetSize div DWord((Channels * 2)){Samples},
                                      BPS{bits per sample}, DWord(Channels), SPS);
         except
           Application.MessageBox('DSP error', 'Error', MB_OK);
           RetSamples := 0;
         end;
         RetSize := RetSamples * DWord((BPS div 8)) * DWORD(Channels);
      end else
         RetSize := GetSize;

      dw := dword(pBuf) + wOffset;
      p := pointer(dw);   // p : starting address to write

      if rOffset > wOffset then
      begin
         Move(DSPBuffer^, p^, RetSize);
         inc(wOffset, RetSize);
      end
      else
         if (bufSize - wOffset) > RetSize then
         begin
            Move(DSPBuffer^, p^, RetSize);
            inc(wOffset, RetSize);
         end else
         begin
            Move(DSPBuffer^, p^, bufSize - wOffset);
            if (bufSize - wOffset) < RetSize then
            begin
               dw := dword(DSPBuffer) + (bufSize - wOffset);
               p2 := pointer(dw);
               Move(p2^, pBuf^, RetSize - (bufSize - wOffset));
            end;
            wOffset := RetSize - (bufSize - wOffset);
         end;
   end else
   begin
      ReachedEnd := true;
      isWriting := false;
      exit;
   end;

   if GetSize < ReqSize then
      ReachedEnd := true
   else begin
      if PlayChannel <> 0 then
      begin
         ChannelStat := BASS_ChannelIsActive(PlayChannel);
         if InitialBufferFill or (ChannelStat = BASS_ACTIVE_PLAYING) or
                                         (ChannelStat = BASS_ACTIVE_STALLED) then
            if FreeSpace(bufSize, rOffset, wOffset) > ReqSpace then
               PostThreadMessage(PlayThreadId, WM_GetChannelData, 0, 0);
            //   NeedData := true;
      end;
      result := 0;
   end;

 // Check initial buffer filling is completed
   if InitialBufferFill then
      if (FreeSpace(bufSize, rOffset, wOffset) <= ReqSpace) or
         (DataRemains(bufSize, rOffset, wOffset) >= (BASSDataSize SHL 1)) then
         InitialBufferFill := false;

   isWriting := false;
end;

function GetResampledData(handle: HSTREAM; buf: Pointer; len, user: DWORD): DWORD; stdcall;
var
   dw : DWORD;
   p, p2 : pointer;
begin
   if InitialBufferFill then
   begin
      PostThreadMessage(PlayThreadId, WM_GetChannelData, 0, 0);
      repeat
         WinProcessMessages;
         if ReachedEnd then
            break;
      until (InitialBufferFill = false);
   end;

   while isWriting do  // avoid duplicate access on buffer
      begin
         WinProcessMessages;
         sleep(20);
      end;

   isReading := true;  // notify that BASS is reading buffer data

   dw := dword(pBuf) + rOffset;
   p := pointer(dw);   // p : starting address to read

   if rOffset > wOffset then
      if (bufSize - rOffset) > len then
      begin
         Move(p^, buf^, len);
         inc(rOffset, len);
         result := len;
      end else   // (bufSize - rOffset) <= len
      begin
         Move(p^, buf^, bufSize - rOffset);
         if (bufSize - rOffset) < len then
         begin
            dw := dword(buf) + bufSize - rOffset;
            p2 := pointer(dw);
            if (len - (bufSize - rOffset)) < wOffset then
            begin
               Move(pBuf^, p2^, len - (bufSize - rOffset));
               rOffset := len - (bufSize - rOffset);
               result := len;
            end else
            begin
               Move(pBuf^, p2^, wOffset);
               rOffset := wOffset;
               result := bufSize - rOffset + wOffset;
            end;
         end else  // (bufSize - rOffset) = len
         begin
            rOffset := 0;
            result := len;
         end;
      end
   else if rOffset < wOffset then
      if (wOffset - rOffset) >= len then
      begin
         Move(p^, buf^, len);
         inc(rOffset, len);
         result := len;
      end else
      begin
         Move(p^, buf^, wOffset - rOffset);
         result := wOffset - rOffset;
         rOffset := wOffset;
      end
   else   // rOffset = wOffset
      result := 0;

   if result < len then
      if ReachedEnd then
         result := result + BASS_STREAMPROC_END
      else
         PostThreadMessage(PlayThreadId, WM_GetChannelData, 0, 0)
   else
      PostThreadMessage(PlayThreadId, WM_GetChannelData, 0, 0);

   isReading := false;
end;

procedure ClearBuffer;
begin
   rOffset := 0;
   wOffset := 0;
   totalGet := 0;
 // FillChar(pBuf^, bufSize, 0);
 //  NeedData := false;
   isWriting := false;
   InitialBufferFill := true;
   ReachedEnd := false;
end;

function PlayNewThread(lpParam : pointer) : DWORD; stdcall;
var
   Msg : TMsg;
   MsgReturn : longbool;
begin
   CloseRequested := false;
   PlayThreadStarted := true;
   SetThreadPriority(GetCurrentThread, THREAD_PRIORITY_ABOVE_NORMAL);

   repeat
     { if NeedData then
      begin
         NeedData := false;
         omodWrite2;
      end else
      begin }
         MsgReturn := GetMessage(Msg, 0, 0, 0);
         if ((Msg.message = WM_QUIT) or (Msg.message = WM_CLOSE)) then
             CloseRequested := true
         else if Msg.message = WM_GetChannelData then
            if PlayChannel <> 0 then
               if not isWriting then
                  omodWrite2;

         TranslateMessage(Msg);
         DispatchMessage(Msg);
    //  end;
   until (integer(MsgReturn) <= 0) or (PlayChannel = 0) or CloseRequested;

   Result := 0;    // * Added (by advice of BassFan)
   PlayThreadId := 0;
   ExitThread(0);
end;

function omodOpen2(Channel : DWORD; samplerate, numchannels, bitspersamp : integer) : DWORD;
// returns opened channel handle
var
   flags : DWORD;
   ThreadHandle : HWND;
begin
   if bitspersamp = 8 then
      flags := BASS_SAMPLE_8BITS
   else
      flags := 0;

   PlayChannel := BASS_StreamCreate(samplerate,
                                     numchannels,
                                     flags,
                                     @GetResampledData,
                                     nil {pointer to user data});
   result := PlayChannel;
   if PlayChannel = 0 then   // 0 : error
      exit
   else
   begin
      SPS := samplerate;
      BPS := bitspersamp;
      Channels := numchannels;
  // BASSDataSize : The data size of each transfering to BASS (= amount to play 0.2sec period)
      BASSDataSize := (SPS * (BPS div 8) * CHANNELS * 2) div 10;

  // Readjust buffer size if previously allocated amount is out of reasonable range.
      if ((BASSDataSize * 2) > bufSize) or ((BASSDataSize * 4) < bufSize) then
      begin
         if (pBuf <> nil) then
            FreeMem(pBuf);
         pBuf := nil;
         bufSize := 0;
         GetMem(pBuf, BASSDataSize * 3); // Get memory to hold sound data from Winamp input plug-in
         if (pBuf <> nil) then
            bufSize := BASSDataSize * 3
         else begin
            BASS_StreamFree(PlayChannel);
            result := 0;
            exit;
         end;
      end;

      ClearBuffer;
      DecodeChannel := Channel;
      PlayThreadStarted := false;
      ThreadHandle := CreateThread(nil, 0, @PlayNewThread, nil, 0, PlayThreadId);
      if ThreadHandle <> 0 then
      begin
         CloseHandle(ThreadHandle);
         repeat
            WinProcessMessages;
            Sleep(20);
         until PlayThreadStarted;
      end else
         MessageBox(hMainHandle, 'Unable to create thread !', 'Error!', MB_ICONEXCLAMATION or MB_ICONWARNING);
   end;
end;

procedure omodClose2;
begin
   if PlayChannel <> 0 then
   begin
      BASS_StreamFree(PlayChannel);
      PlayChannel := 0;
   end;

   if (PlayThreadId <> 0) then
   begin
      PostThreadMessage(PlayThreadId, WM_CLOSE, 0, 0);
      repeat
         WinProcessMessages;
         sleep(20);
      until PlayThreadId = 0;
   end;
end;

function DataInBuffer : DWORD;
begin
   result := DataRemains(bufSize, rOffset, wOffset);
end;

// GetPlayedTime will vary if some kind of DSP effect is applied because the amount of
// output sample data may be different from the one of input data.
function  GetPlayedTime : DWORD;  // Played time in mili second since last flush
begin
   result := round(1000 * (totalGet - DataInBuffer) / (SPS * (BPS SHR 3) * Channels));
end;


//---------------------- Winamp In/Out plug-in support -----------------------------

// get the reference number of specified plug-in
//  ( -1 :  specified plug-in is not loaded )
function GetPluginIndex(PluginName : string) : integer;
var
   i : integer;
begin
   result := -1;

   for i := 0 to MaxPluginNum - 1 do
      if Plugin[i] <> nil then
         if Uppercase(PluginInfo[i].Name) = Uppercase(PluginName) then
         begin
            result := i;
            break;
         end;
end;

// get the control pointer of specified plug-in
//  ( nil : specified plug-in is not loaded )
function GetPlugin(PluginNum : integer) : TPlugin;
var
   dw : dword;
begin
   if (PluginNum < 0) or (PluginNum > (MaxPluginNum - 1)) then
      result := nil
   else
   begin
      dw := dword(Plugin[PluginNum]);
      result := pointer(dw);
   end;
end;

// get the information on specified plug-in
function GetPluginInfo(PluginNum : integer) : TPluginInfo;
var
  APluginInfo : TPluginInfo;

 procedure ClearPluginInfo;
 begin
    with APluginInfo do
      begin
         Name := '';
         Version := 0;
         Description := '';
         FileExtensions := '';
         Seekable := 0;
         DLLHandle := 0;
      end;
 end;

begin
   if (PluginNum < 0) or (PluginNum > (MaxPluginNum - 1)) then
      ClearPluginInfo
   else if Plugin[PluginNum] = nil then
      ClearPluginInfo
   else
      with APluginInfo do
      begin
          Name := PluginInfo[PluginNum].Name;
          Version := PluginInfo[PluginNum].Version;
          Description := PluginInfo[PluginNum].Description;
          FileExtensions := PluginInfo[PluginNum].FileExtensions;
          Seekable := PluginInfo[PluginNum].Seekable;
          DLLHandle := PluginInfo[PluginNum].DLLHandle;
      end;

   result := APluginInfo;
end;

// get the played time in ms
function GetOutputTime(omodTime : boolean) : integer;
begin
   if omodTime then
      result := omod.GetOutputTime
   else
      result := Plugin[imodNum].GetOutputTime;
end;

procedure SetPosChanged;
begin
   PosChanged := true;
end;

function SelectInputPlugin(PluginNum : integer) : integer;
begin
   result := -1;

   if (PluginNum < -1) or (PluginNum > (MaxPluginNum - 1)) then
      exit;

   if PluginNum >= 0 then
      if Plugin[PluginNum] = nil then   // no valid Plugin[x]
         exit;

   imodNum := PluginNum;  // set imodNum to -1 if no plug-in is in use.
   result := 0;
end;

function ActivePlugin : string;
begin
   if imodNum = - 1 then
      result := ''
   else
      result := PluginInfo[imodNum].Name;
end;

{procedure SetMainHandle(MainHandle : HWND);
begin
   hMainHandle := MainHandle;
end; }

{function GetMainHandle : HWND;
begin
   result := hMainHandle;
end; }

// check if output plug-in emulator is ready  ( true : ready )
function OutputReady : boolean;
begin
   result := omodReady;
end;

// Check whether the specified Winamp input plug-in is loaded.
function IsWinampPluginLoaded(PluginName : string) : boolean;
var
   i : integer;
begin
   result := false;

   for i := 0 to MaxPluginNum - 1 do
      if Plugin[i] <> nil then     // Plugin[i] = nil -> empty element
         if Uppercase(PluginInfo[i].Name) = Uppercase(PluginName) then
         begin
            result := true;  // The specified plug-in is loaded.
            exit;
         end;
end;

// load a Winamp input plug-in  ( 0 : succeed )
function LoadWinampPlugin(PluginName : string) : integer;
var
   indexToLoad : integer;
   FilePath : string;
   DLLHandle : THandle;
   getInModule2 : function : pointer; stdcall;
   dw : dword;
   i : integer;
begin
   if not omodReady then
   begin
      result := ERROR_OMOD_NOTREADY;  // output plug-in emulator is not ready
      exit;
   end;

   if IsWinampPluginLoaded(PluginName) then
   begin
      result := ERROR_LOADED_BEFORE;  // already loaded plug-in
      exit;
   end;

   indexToLoad := -1;
   for i := 0 to MaxPluginNum - 1 do
      if Plugin[i] = nil then
      begin
         indexToLoad := i;       // found out empty element to use
         break;
      end;
   if indexToLoad = -1 then
   begin
      result := ERROR_LOADED_FULL;     // no empty element to use
      exit;
   end;

   FilePath := GetProgDir + 'Plugins\' + PluginName;
   DLLHandle := LoadLibrary(pchar(FilePath));
   if DLLHandle = 0 then
   begin
     result := ERROR_CANNOT_LOAD;
     exit;
   end;

   getInModule2 := GetProcAddress(DLLHandle, 'winampGetInModule2');
   if @getInModule2 = nil then
   begin
     result := ERROR_INVALID_PLUGIN;
     FreeLibrary(DLLHandle);
     exit;
   end;

   imod := getInModule2;
   imod.hMainWindow := hMainHandle;  // <= handle of internal messsage handler
   imod.hDllInstance := DllHandle;
   imod.outMod := omod;
   imod.init;
   imod.SetInfo := SetInfo1;
   imod.dsp_IsActive := dsp_isactive1;
   imod.dsp_dosamples := dsp_dosamples1;
   imod.SAVSAInit := SAVSAInit1;
   imod.SAVSADeInit := SAVSADeinit1;
   imod.SAAddPCMData := SAAddPCMData1;
   imod.SAGetMode := SAGetMode1;
   imod.SAAdd := SAADD1;
   imod.VSASetInfo := VSASetInfo1;
   imod.VSAAddPCMData := VSAAddPCMData1;
   imod.VSAGetMode := VSAGetMode1;
   imod.VSAAdd := VSAAdd1;
//   imod.About(0);

   dw := dword(imod);
   Plugin[indexToLoad] := pointer(dw);
   PluginInfo[indexToLoad].Name := PluginName;
   PluginInfo[indexToLoad].Version := imod.version;
   PluginInfo[indexToLoad].Description := string(imod.description);
   PluginInfo[indexToLoad].FileExtensions := string(imod.FileExtensions);
   PluginInfo[indexToLoad].Seekable := imod.is_seekable;
   PluginInfo[indexToLoad].DLLHandle := DLLHandle;

   result := 0;
end;

// unload a Winamp input plug-in  ( 0 : succeed )
function UnloadWinampPlugin(PluginName : string) : integer;
var
   indexToUnload : integer;
   returnOK : longbool;
begin
   indexToUnload := GetPluginIndex(PluginName);

   if indexToUnload = -1 then
   begin
      result := ERROR_NOT_LOADED;
      exit;
   end;
   if indexToUnload = imodNum then
   begin
      result := ERROR_IN_USE;
      exit;
   end;

   Plugin[indexToUnload].Quit;
   returnOK := FreeLibrary(PluginInfo[indexToUnload].DLLHandle);
   if returnOK then
   begin
      result := 0;
      Plugin[indexToUnload] := nil;
   end else
      result := ERROR_CANNOT_UNLOAD;
end;

// get the file types (file extensions) which can be played by any loaded plug-ins
function GetPlayableFiles : string;
var
   s : string;
   i : integer;

  function GetFileExt(s : string) : string;
  var
     s1, s2 : string;
     i : integer;
  begin
     s1 := s;
     if length(s1) = 0 then
        s2 := ''
     else begin
        s2 := '*.';
        for i := 1 to length(s1) do
        begin
           s2 := s2 + s1[i];
           if s1[i] = ';' then
              if i < length(s1) then
                 s2 := s2 + '*.';
        end;
     end;

     result := LowerCase(s2);
  end;

begin
   s := '';
   if not omodReady then  // if not performed function InitPluginCtrl yet or failed to
      exit;               //  get buffer memory to support Winamp input plug-ins.

   for i := 0 to MaxPluginNum - 1 do
      if Plugin[i] <> nil then
         s := s + GetFileExt(Plugin[i].FileExtensions) + ';';

   result := s;
end;

// Get the number of plug-in which can play specified file type
function GetPluginNumber(ExtCode : string) : integer;
var
   i : integer;
begin
   result := -1;  // Pre-assume there is no plug-in for specified file type

   for i := 0 to MaxPluginNum - 1 do
      if Plugin[i] <> nil then
         if pos(upperCase(ExtCode), upperCase(Plugin[i].FileExtensions)) <> 0 then
         begin
            result := i;
            break;
         end;
end;


// initialization
// return value : true on success, false on failure
function InitPluginCtrl(MainHandle : HWND) : boolean;
var
   i : integer;
begin
   if omodReady then
   begin
      result := true;
      exit;
   end;

   result := false;

   if MainHandle = 0 then
      exit
   else
      hMainHandle := MainHandle;
   for i := 0 to MaxPluginNum - 1 do
     Plugin[i] := nil;
   imodNum := -1;

   try
     GetMem(pBuf, DefaultBufSize); // Get memory to hold sound data from Winamp input plug-in
     SetOutputPlugin;              // omodReady should be set at executing this procedure
     bufSize := DefaultBufSize;
   except
     exit;
   end;

   if bufSize > 0 then
     try
       GetMem(DSPBuffer, ChannelLimit * PacketSize * 4);
       DSPBufferSize := ChannelLimit * PacketSize * 4
     except
       exit;
     end;

   result := omodReady;
end;

// finalization
procedure QuitPluginCtrl;
var
   i : integer;
begin
   for i := 0 to MaxPluginNum - 1 do
      if (Plugin[i] <> nil) then
      begin                           // Release all Winamp input plug-ins
         Plugin[i].Quit;
         FreeLibrary(PluginInfo[i].DLLHandle);
      end;

   if (omod <> nil) then
      omod.Quit;

   if (pBuf <> nil) then
   begin
      FreeMem(pBuf);
      bufSize := 0;
   end;
   if (DSPBuffer <> nil) then
   begin
      FreeMem(DSPBuffer);
      DSPBufferSize := 0;
   end;

end;


//--------------------- Winamp visualization plug-in support --------------------

function VisActive : boolean;
begin
   result := VisIsActive;
end;

procedure LoadVisModule(PluginPath : string;
                        var Visheader : PWinampVisHeader;
                        var Vismod : TVismod;
                        var NumVismod : integer;
                        ParentHandle : HWND);
var
   i : integer;
begin
   NumVismod := 0;
   VismodNum := 0;
 //  CloseVisDLL;

   if Uppercase(PluginPath) <> Uppercase(GetLoadedVisDLL) then
      if not initVisDll(PluginPath) then
        exit;

  // try
  //   Visheader := getVisHeader;
  // except
     Visheader := getVisHeader(ParentHandle);
  // end; 

   if VisHeader = nil then
     exit;

   for i := 0 to (maxVismodNum - 1) do
   begin
      Vismod[i] := Visheader.getModule(i);
      if Vismod[i] <> nil then
      begin
         Vismod[i]^.hwndParent := ParentHandle;
         Vismod[i]^.hDllInstance := GetVisDLLHandle;
         inc(VismodNum);
      end else
         break;
   end;

   CurVismod := Vismod;
   NumVismod := VismodNum;
   VismodIndex := -1;
end;

function StartVisModule(ModuleNum : word) : integer;
begin
   result := -1;

   if VismodNum = 0 then
      exit;
   if (ModuleNum > VismodNum - 1) then
   begin
      result := -2;
      exit;
   end;
   if VismodIndex = ModuleNum then
   begin
      result := -3;
      exit;
   end;

   if VismodIndex > -1 then
      if CurVismod[VismodIndex] <> nil then
         CurVismod[VismodIndex]^.Quit(CurVismod[VismodIndex]);

   VismodIndex := ModuleNum;
   result := CurVismod[VismodIndex]^.init(CurVismod[VismodIndex]);

   if result = 0 then
      VisIsActive := true;
end;

procedure SetVisOutHandle(VisOutHandle : HWND);
begin
   hVisOutWindow := VisOutHandle;
end;

procedure SetVisualizationQuitted;
begin
   VismodIndex := -1;
   VisualizationQuitted := true;
end;

function UnloadVisModule : integer;
begin
   result := -1;
   if VismodNum = 0 then
      exit;

   VisIsActive := false;
   if IsWindow(hVisOutWindow) then
      if not VisualizationQuitted then
         if CurVismod[VismodIndex] <> nil then
            CurVismod[VismodIndex]^.Quit(CurVismod[VismodIndex]);

   CloseVisDLL;
   VismodNum := 0;
   hVisOutWindow := 0;
   result := 0;
end;



//------------------------- Winamp DSP plug-in support ---------------------------

// Some Winamp DSP plug-in shows errorneous behavior if Winamp IPC messages are not handled.
// I tried to support both DSP plug-in and visaulization plug-in with one Winamp-like window.
// But I could not solve some problems so I decided to use seperate Winamp-like window,
// one for DSP plug-ins and the other for visaulization plug-ins.
// The Winamp-like window for visaulization plug-ins are managed in VisDrive.pas unit.

function WindowProc(hWnd, Msg, wParam, lParam : Longint) : Longint; stdcall;
var
   p : PBYTE;
   TitleP : pchar;
begin
   Result := 0;     // default value

   if Msg = WM_WA_IPC then   // Message is from DSP plug-in ?
   begin
     if lParam = IPC_GETVERSION then
        Result := $2041    // acts as if the main program is Winamp ver 2.41
     else if lParam = IPC_ISPLAYING then
     begin
        if PlayerMode = plmPlaying then
           Result := 1
        else if PlayerMode = plmPaused then
           Result := 3;
     end else if lParam = IPC_GETOUTPUTTIME then
     begin
        if wParam = 0 then      // position in miliseconds
        begin
           if PlayerMode <> plmPlaying then
              Result := -1
           else
              Result := PlaybackPosition;
        end else if wParam = 1 then      // song length in seconds
           Result := StreamInfo.Duration div 1000;
     end else if lParam = IPC_GETLISTLENGTH then
        Result := 1
     else if lParam = IPC_GETLISTPOS then
        Result := 1
     else if lParam = IPC_GETINFO then
     begin
        if wParam = 0 then           // Sample rate
           Result := StreamInfo.SampleRate
        else if wParam = 1 then      // Bit rate
           Result := StreamInfo.Bitrate
        else if wParam = 2 then      // Channels
           Result := StreamInfo.Channels;
     end else if lParam = IPC_GETPLAYLISTFILE then
     begin
        ReturnStr := StreamInfo.FileName + chr(0);
        p := @ReturnStr;
        inc(p, 1);
        Result := Longint(p);
     end else if lParam = IPC_GETPLAYLISTTITLE then
     begin
        ReturnStr := StreamInfo.Title + chr(0);
        p := @ReturnStr;
        inc(p, 1);
        Result := Longint(p);
     end else
        Result := 1;   //  currently not implemented/supported  IPC message

  // Response to message WM_GETTEXT is to hand over the title of playing stream file to
  // Winamp plug-in.
  // You must add ' - Winamp' to the title string because some Winamp plug-ins
  // show erroneous operation if it is missed (ex. vis_Bass-C.dll)
   end else if Msg = WM_GETTEXT then
   begin
      TitleP := pchar(StreamInfo.Title + ' - Winamp');
      Move(TitleP^, (pointer(lParam))^, length(StreamInfo.Title)+10);
      Result := length(StreamInfo.Title)+10;
   end else
      Result := DefWindowProc(hWnd, Msg, wParam, lParam);

end;

// Create a fake window which acts like the one of Winamp's main window
function CreateFakeWindow(ParentWindow : HWND) : HWND;
var
   WinAtom : TAtom;
   wClass : TWNDCLASSEX;
begin
   if hFakeWindow <> 0 then   // Avoid duplicate creation of fake Winamp window
   begin
      result := hFakeWindow;
      exit;
   end;

   hInst := GetModuleHandle(nil); // get the application instance
   hMainWindow := ParentWindow;

   with wClass do
   begin
      cbSize        := sizeof(wClass);
      Style         := CS_PARENTDC {or CS_VREDRAW};
      lpfnWndProc   := @WindowProc;
      cbClsExtra    := 0;
      cbWndExtra    := 0;
      hInstance     := hInst;
      hIcon         := 0{LoadIcon(hInst, 'MAINICON')};;
      hCursor       := LoadCursor(0, IDC_ARROW);
      hbrBackground := COLOR_BTNFACE + 1;
      lpszMenuName  := nil;
      lpszClassName := 'Winamp v2.x';
      hIconSm       := 0;
   end;

  // Once our class is registered we can start making windows with it
   WinAtom := windows.RegisterClassEx(wClass);

   if WinAtom <> 0 then
   begin
      hFakeWindow := CreateWindowEx(0, 'Winamp v2.x', 'Winamp 2.41',
                                    WS_POPUP,      // no-frame, non-visible window
                                    5, 5, 25, 25,  // x, y, width, height
                                    0{hMainWindow}, 0, hInst, nil);
      result := hFakeWindow;
   end else
      result := 0;
end;


function DestroyFakeWindow : boolean;
begin
   if hFakeWindow <> 0 then
   begin
      result := DestroyWindow(hFakeWindow); 	// handle to window to be destroyed
      if result then
      begin
         hFakeWindow := 0;
         windows.UnRegisterClass('Winamp v2.x', hInst);
      end;
   end else
      result := false;
end;

procedure SetStreamInfo2(Stream : TStreamInfo);
begin
   StreamInfo := Stream;
end;

procedure SetPlayerMode2(Mode : TPlayerMode);
begin
   PlayerMode := Mode;
end;

procedure SetChannelInfo2(ChannelType : TChannelType; D_ChannelId, P_ChannelId,
                                        SampleRate, Channels : DWORD);
begin
   FChannelType := ChannelType;
   FD_ChannelId := D_ChannelId;
   FP_ChannelId := P_ChannelId;
   FSampleRate := SampleRate;
   FChannels := Channels;
end;

function DSPBufferReady : boolean;
begin
   result := (DSPBufferSize <> 0);
end;

function DSPActive : boolean;
begin
   result := DSPIsActive;
end;

procedure LoadDSPModule(PluginPath : string;
                        var DSPheader : PWinampDSPHeader;
                        var DSPmod : TDSPmod;
                        var NumDSPmod : integer{;
                        ParentHandle : HWND});
var
   i : integer;
begin
   NumDSPmod := 0;
 //  CloseDSPDLL;

   if Uppercase(PluginPath) <> Uppercase(GetLoadedDSPDLL) then
   begin
      DSPmodIndex := -1;
      DSPmodNum := 0;
      if not initDSPDll(PluginPath) then
         exit;
   end else if DSPmodNum > 0 then  // if already loaded DSP module
   begin
      DSPheader := getDSPHeader;
      DSPmod := CurDSPmod;
      NumDSPmod := DSPmodNum;
      exit;
   end;

   DSPheader := getDSPHeader;
   if DSPHeader = nil then
     exit;

   for i := 0 to (maxDSPmodNum - 1) do
   begin
      DSPmod[i] := DSPheader.getModule(i);
      if DSPmod[i] <> nil then
      begin
      //   DSPmod[i]^.hwndParent := hFakeWindow;
         DSPmod[i]^.hDllInstance := GetDSPDLLHandle;
         inc(DSPmodNum);
      end else
         break;
   end;

   CurDSPmod := DSPmod;
   NumDSPmod := DSPmodNum;
end;


function StartDSPModule(ModuleNum : word;
                        ParentHandle : HWND) : integer;
begin
   result := -1;

   if DSPBufferSize = 0 then
      exit;                                
   if DSPmodNum = 0 then
      exit;
   if ModuleNum > (DSPmodNum - 1) then
      exit;

   if DSPmodIndex = ModuleNum then  // Is alrready running ?
   begin
      result := 0;
      exit;
   end;

   if DSPmodIndex > -1 then
      if CurDSPmod[DSPmodIndex] <> nil then
         CurDSPmod[DSPmodIndex]^.Quit(CurDSPmod[DSPmodIndex]);

   if hFakeWindow = 0 then
      hFakeWindow := CreateFakeWindow(ParentHandle);
   if hFakeWindow = 0 then
      exit;

   CurDSPmod[ModuleNum]^.hwndParent := hFakeWindow;
   DSPIsActive := (CurDSPmod[ModuleNum]^.init(CurDSPmod[ModuleNum]) = 0);
   if DSPIsActive then
   begin
      DSPmodIndex := ModuleNum;     // DSPmodIndex : running module #
      result := 0;
   end;
end;

function StopDSPModule : integer;
begin
   result := -1;
   if DSPmodNum = 0 then
      exit;

   if DSPIsActive then
   begin
      DSPIsActive := false;
      if CurDSPmod[DSPmodIndex] <> nil then
         CurDSPmod[DSPmodIndex]^.Quit(CurDSPmod[DSPmodIndex]);
   end;

   DestroyFakeWindow;
   DSPmodIndex := -1;

   result := 0;
end;

function UnloadDSPModule : integer;
begin
   if StopDSPModule = 0 then
   begin
      CloseDSPDLL;
      DSPmodNum := 0;
      result := 0;
   end else
      result := -1;
end;

//----------------------- Winamp GPP Plug-in support ------------------------
// Use only for the Winamp customized general purpose plug-in, 'Gen_VisDrawer.dll'.

{ var
   MyThreadId : DWORD;
   MyWinHandle : HWND;
   OtherWinHandle : HWND;

function LookAtAllWindows2(Handle: HWnd; Temp: Longint): BOOL; stdcall;
var
   ThreadId : DWORD;
   ProcId : DWORD;
begin
   result := true;

   if IsWindowVisible(Handle) then
   begin
   // get identifier of the thread which created window
      ThreadId := GetWindowThreadProcessId(Handle, @ProcId);
      if ThreadId = MyThreadId then
      begin
         if Handle <> OtherWinHandle then
         begin
            MyWinHandle := Handle;
            result := false;
         end;
      end;
   end;
end;  }

function GPPActive : boolean;                      
begin
   result := GPPIsActive;
end;

function StartGPPModule(ParentHandle : HWND;        // * Modified at Ver 1.43
                        DriveThreadId : DWORD;
                        InitWinPos : TInitWinPos) : HWND;
var
   GPPheader : PWinampGPP;
   InitResult : integer;
   WinHandle : HWND;
   ProcId : DWORD;
   ThreadId : DWORD;
 //  ClassName : array[0..79] of char;
 //  n : integer;
   RepeatCounter : integer;
begin
   result := 0;

   if GPPIsActive then
      exit;

   if GetGPPDLLHandle = 0 then
      exit;

   GPPheader := getGPPHeader;
 //  MyThreadId := DriveThreadId;
 //  OtherWinHandle := FakeWinHandle;

   GPPheader^.hwndParent := ParentHandle;
   GPPheader^.hDLLInstance := GetGPPDLLHandle;
 //  InitResult := GPPheader^.init;      // * Replaced with init2 at Ver 1.43
   InitResult := GPPheader^.init2(InitWinPos);

   if InitResult = 0 then
   begin
   //   MyWinHandle := 0;
      RepeatCounter := 0;

   // Wait until GPP window is created
      repeat
        { EnumWindows(@LookAtAllWindows2, 0);

         if MyWinHandle <> 0 then
         begin
            n := GetClassName(MyWinHandle, ClassName, SizeOf(ClassName) - 1);
            if n > 0 then
            begin
               ClassName[n] := #0;
               if string(ClassName) = CLASSNAME_WINAMP_VIS_DRAWER then
               begin
                  GPPIsActive := true;
                  result := MyWinHandle;
                  continue;
               end;
            end;
         end;  }

         WinHandle := FindWindow(CLASSNAME_WINAMP_VIS_DRAWER, nil);
         if WinHandle <> 0 then                // found the window ?
         begin
            ThreadId := GetWindowThreadProcessId(WinHandle, @ProcId);
            if ThreadId = DriveThreadId then   // created by this thread ?
            begin
               GPPIsActive := true;
               result := WinHandle;
               continue;
            end;
         end;

         sleep(50);
         WinProcessMessages;
         inc(RepeatCounter);
      until (result <> 0) or (RepeatCounter = 100);

   end;
end;

procedure SetGPPInactive;
var
   GPPheader : PWinampGPP;
begin
   GPPIsActive := false;

   if GetGPPDLLHandle = 0 then
      exit;

   GPPheader := getGPPHeader;
   GPPheader^.quit;          // Quit acctivity of Gen_VisDrawer.dll
end;

end.
