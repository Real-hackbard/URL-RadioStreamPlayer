# :computer: URL-RadioStreamPlayer:

</br>

![Compiler](https://github.com/user-attachments/assets/a916143d-3f1b-4e1f-b1e0-1067ef9e0401) <img src="https://github.com/user-attachments/assets/f31ad418-b104-4c87-8a37-876099647af7" />  
![Components](https://github.com/user-attachments/assets/d6a7a7a4-f10e-4df1-9c4f-b4a1a8db7f0e) <img src="https://github.com/user-attachments/assets/a817ec16-763d-499d-92f0-731470136f19" />  
![Discription](https://github.com/user-attachments/assets/4a778202-1072-463a-bfa3-842226e300af) ![URL-RadioStreamPlayer](https://github.com/user-attachments/assets/34f433f9-5443-4540-81c0-44a9ee688666)  
![Last Update](https://github.com/user-attachments/assets/e1d05f21-2a01-4ecf-94f3-b7bdff4d44dd) <img src="https://github.com/user-attachments/assets/50a8b8ef-f73e-48f4-a47b-3bc8ca1b75b6" />  
![License](https://github.com/user-attachments/assets/ff71a38b-8813-4a79-8774-09a2f3893b48) ![Freeware](https://github.com/user-attachments/assets/1fea2bbf-b296-4152-badd-e1cdae115c43)

</br>

Internet radio, also known as online radio, web radio, net radio, streaming radio, e-radio and IP radio, is a [digital audio](https://en.wikipedia.org/wiki/Digital_audio) service transmitted via the Internet. Broadcasting on the Internet is usually referred to as [webcasting](https://en.wikipedia.org/wiki/Webcast) since it is not transmitted broadly through wireless means. It can either be used as a stand-alone device running through the Internet, or as a software running through a single Computer.

Internet radio is generally used to communicate and easily spread messages through the form of talk. It is distributed through a wireless communication network connected to a switch packet network (the internet) via a disclosed source.

Internet radio involves [streaming media](https://en.wikipedia.org/wiki/Streaming_media), presenting listeners with a continuous stream of audio that typically cannot be paused or replayed, much like traditional broadcast media; in this respect, it is distinct from on-demand file serving. Internet radio is also distinct from [podcasting](https://en.wikipedia.org/wiki/Podcast), which involves [downloading](https://en.wikipedia.org/wiki/Download) rather than streaming.

### Update Notes
* Remove Components (```Slider.pas; Knob.pas```)
* Replace slider component with VCL TrackBar
* Load DSP PlugIns
* Load Bass AddOns
* Load InputPlugIns
* Load Vis-PlugIn Control

### Features
* URL-Stream Player
* Encode Streams by PlugIns
* Decode Streams by PlugIns
* Edit Tags ([MPEG](https://en.wikipedia.org/wiki/MP3), [OGG](https://en.wikipedia.org/wiki/Vorbis), [WAVE](https://de.wikipedia.org/wiki/RIFF_WAVE), [WMA](https://en.wikipedia.org/wiki/Windows_Media_Audio))
* Bass Player
* Download Streams via Encoding PlugIns

For more BASS Audio Library files : https://www.un4seen.com/  

</br>

<img src="https://github.com/user-attachments/assets/b60bfea1-2cd0-46f1-9433-717510403345" />

</br>
</br>

# Visualization
Spectrum is a Visualisation showing a rotating 3D Spectrum Analyzer rendered in that moves to the beat of the music creating a wave effect across the spectrum grid.

It also has settings to adjust the spectrum bar height, Its speed, and how the bars are rendered (solid, points, wire frame) and is a nice light weight, low cpu useage visualisation that looks great.

### :wrench: Settings
</br>
<img src="https://github.com/user-attachments/assets/e627a12d-bd2d-4f6b-95ff-21aeb3110d86" />
</br>

### Examples
</br>
<img src="https://github.com/user-attachments/assets/922b42df-8899-4801-8ec8-cd6eb8c6dbb6" />  
<img src="https://github.com/user-attachments/assets/80b1b0c6-63a5-47f4-9213-362d93fae879" />  
<img src="https://github.com/user-attachments/assets/84c76923-dd78-44d4-8ed6-91d2aeca8e05" />  

</br>
</br>

21 visualizations are available.

Classic Spectrum Analyzer  
Copyright © 2007 Mike Lynch  
eMail : mlynch@gmail.com  

Winamp visualization plugin files typically use the .dll (Dynamic Link Library) file extension, as plugins are compiled code files placed in the Winamp/Plugins directory, such as the examples vis_ngm.dll and vis_avs.dll.

After downloading a visualization plugin, you typically copy the .dll file into the Plugins folder in main Directory

```
vis_ngm.dll is an example of a Winamp visualization plugin.
vis_avs.dll is the file for the Advanced Visualization Studio (AVS) plugin. 
```

</br>  

# Tag Edit
A tag editor is an app that can add, edit, or remove embedded [](https://en.wikipedia.org/wiki/Metadata)metadata on multimedia file formats. Content creators, such as musicians, photographers, podcasters, and video producers, may need to properly label and manage their creations, adding such details as title, creator, date of creation, and copyright notice.

You can edit the different tag audio formats directly from the program.

supports ```ID3v1, ID3v2.3, ID3v2.4, Ogg, WMA, Vorbis```. It can rename files based on the tag information, replace characters or words in tags and filenames, import/export tag information, create playlists and more.

It is especially useful for [audio books](https://wiki.mobileread.com/wiki/Audio_books) that need corrections to the title to get them to play in the right order.

</br>

<img src="https://github.com/user-attachments/assets/c40d33e5-1802-4ead-ba74-4ddfb21fcea0" />

</br>


# PlugIns:
Winamp plugins extend its functionality, categorizing into types like Input plugins for different audio formats, Output plugins for directing audio, DSP plugins for sound processing, and General Purpose (Gen_) plugins for user interface or feature enhancements. To find and install plugins, you can visit community sites like [winampplugins.co.uk](https://winampplugins.co.uk/), and to develop your own, you can refer to the Winamp SDK. 

Copy the plug-ins into the "Plugin" folder and modify the code in the OnShow Event from the Form.

```pascal
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

   ScrollBar1.Position := BassPlayer1.Volume;
   CreateBasicImage;
   ShowBackground;
   for i := 1 to NumFFTBands do
      PeakValue[i] := 0;

   // These components have been replaced and can be assigned new values.
   EQGains[0] := TrackBar1.Position - 15;
   EQGains[1] := TrackBar2.Position - 15;
   EQGains[2] := TrackBar3.Position - 15;
   EQGains[3] := TrackBar4.Position - 15;
   EQGains[4] := TrackBar5.Position - 15;
   EQGains[5] := TrackBar6.Position - 15;
   EQGains[6] := TrackBar7.Position - 15;
   EQGains[7] := TrackBar8.Position - 15;
   EQGains[8] := TrackBar9.Position - 15;
   EQGains[9] := TrackBar10.Position - 15;

   TrackBar11.Position := BassPlayer1.EchoLevel;
   TrackBar12.Position := BassPlayer1.ReverbLevel;

   if BassPlayer1.PlayerReady then
   begin
      //LoadWinampPlugin('in_cdda.dll');
      //LoadWinampPlugin('in_asf.dll');
      LoadWinampPlugin(ProgDir + 'Data\Plugins\in_midi.dll');   // I have found that it is unable to load in_midi v3.16.
        // So, I use in_midi v3.07.

      //LoadWinampPlugin('in_wm_old.dll');

      BassPlayer1.PluginFirst := false;

    // Following sentences are for supporting BASSMDID
      if BassPlayer1.BASSMIDIReady then
         if FileExists(ProgDir + 'Data\Plugins\Chorium.SF2') then
         begin
         // You can use any SF2 soundfont other than Chorium.SF2 to activate BASSMIDI.dll.
         // However this demo program requires Chorium.SF2.
            if BassPlayer1.MIDIFontInit(ProgDir + 'Data\Plugins\Chorium.SF2', MIDI_FONTINFO) then
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
      Timer1.Enabled := true;

   Saved8087CW := Default8087CW;
   Set8087CW($133f);  // Disable all fpu exceptions

   ComboBox1.Items.LoadFromFile(ProgDir +  'Data\URL\lst.ini');
   ComboBox1.ItemIndex := 0;
end;
```

..or load them manually from the program into memory.
</br>


# DSP Plugin:
A Winamp DSP (Digital Signal Processing) plugin is a small software component that modifies audio in real-time, allowing for effects and enhancements during playback, such as equalization, reverb, or special audio manipulation. These plugins are created using a software development kit (SDK) for the Winamp player, which supports different plugin types, including DSP/Effect plugins. They are often shared, installed, and used by other programs like [MediaMonkey Wiki](https://www.mediamonkey.com/wiki/Winamp_Plug-ins_(MM4)) and [foobar2000](https://wiki.hydrogenaudio.org/index.php?title=Foobar2000:Components/Winamp_DSP_Bridge_(foo_dsp_winamp)), which can run Winamp's plug-in architecture.

Copy the DSP plug-ins into the "Plugin" folder and modify the code in the OnShow Event from the Form.

### Intigrated example Channel Mixer
The Channel Mixer is a DSP plug-in for Winamp. It will enable you to create new 
stereo mixes using the existing right and left channels as input sources. With 
different combinations, you can create some interesting stereo imagining effects 
like: Average, Invert, Swap Channels, Wide Stereo, Vocal Cut...

</br>

<img src="https://github.com/user-attachments/assets/aa858b92-0f3b-485f-b7bc-bbfe14314e59" />

</br>
</br>


CHANNEL MIXER 1.1  
Copyright © 2002 Dominik Tyniw, OLSZTYN Poland  
eMail : dtyniw@pf.pl  

The Channel Mixer consists of the following files:

    DSP_CHMX.DLL the plug-in DLL file
    DSP_CHMX.HLP Channel Mixer help file
    DSP_CHMX.INI configuration file
    DSP_CHMX.TXT Channel Mixer overview

Copy files listed above to your /DSP/ plug-ins folder.

The Channel Mixer works only with 16-bit stereo input signal.

# Encode Midi:
SoundFont is a brand name that collectively refers to a file format and associated technology that uses [sample-based synthesis](https://en.wikipedia.org/wiki/Sample-based_synthesis) to play MIDI files. It was first used on the Sound Blaster AWE32 sound card for its [General MIDI support](https://en.wikipedia.org/wiki/General_MIDI).

Download the [*.SF2](https://en.wikipedia.org/wiki/SoundFont) files and copy them into the "Plugin" folder. They will be automatically recognized and Encoded in MIDI Format.

The files are too large to upload here.  
Download : [https://musical-artifacts.com/artifacts/1474](https://musical-artifacts.com/artifacts/1474)

# URL-Radio List:
You can edit your own Radio-URL list, which will be automatically detected in the "URL\lst.ini" folder.

### Intigrated Radio links:
http://waw.ic.smcdn.pl/2330-1.mp3
http://hydra.cdnstream.com/1536_128
http://organichiphop.filehostia.com:8504/stream
http://live.powerhitz.com/lit
http://air.radioart.com/fLove_songs.mp3
http://media-ssl.musicradio.com/CapitalMP3
http://ntvrdwmp.radyotvonline.com/;
http://bbkissfm.kissfmradio.cires21.com/bbkissfm.mp3
http://kisskiss.fluidstream.eu/KissKiss.mp3
http://streaming.nrjaudio.fm/oumrha8fnozc
http://streaming.nrjaudio.fm/ou8vs6gk7oiu
http://stream.klassikradio.de/newclassics/mp3-192/
http://stream.epic-classical.com/classical-love-music
http://stream.zeno.fm/6z99pfyw67zuv
http://media-ssl.musicradio.com/ClassicFMMP3
http://stream.rockantenne.de/classic-perlen/stream/mp3
http://stream.rockantenne.de/blues-rock/stream/mp3
http://stream.rockantenne.de/70er-rock/stream/mp3
http://stream.rockantenne.de/80er-rock/stream/mp3
http://stream.rockantenne.de/90er-rock/stream/mp3
http://dispatcher.rndfnk.com/rbb/rbbkultur/live/mp3/high
http://stream.rtlradio.de/greatesthits/mp3-192/
http://crystalout.surfernetwork.com:8001/KRMP_MP3
http://nr9.newradio.it:9565/stream
http://ais-sa2.cdnstream1.com/1674_128
http://hemnos.cdnstream.com/1881_128
http://listen.181fm.com/181-80srnb_128k.mp3
http://stream.radiojar.com/ypargqan2qzuv
http://organichiphop.filehostia.com:8504/stream
http://play.global.audio/radio1128

