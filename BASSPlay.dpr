program BASSPlay;

uses
  Forms,
  BASSPlayer in 'BASSPlayer.pas',
  BassTest in 'BassTest.pas' {Form1},
  DSPPluginCtrl in 'DSPPluginCtrl.pas' {DSPControlForm},
  AddonConfig in 'AddonConfig.pas' {AddonConfigForm},
  VisPluginCtrl in 'VisPluginCtrl.pas' {VisControlForm},
  PluginConfig in 'PluginConfig.pas' {PluginConfigForm},
  TAGEdit in 'TAGEdit.pas' {TAGEditForm},
  MPEGInfoBox in 'MPEGInfoBox.pas' {MPEGFileInfoForm},
  OGGInfoBox in 'OGGInfoBox.pas' {OggVorbisInfoForm},
  WMAInfoBox in 'WMAInfoBox.pas' {WMAInfoForm};

{$R *.res}

begin
  System.IsMultiThread := true;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TDSPControlForm, DSPControlForm);
  Application.CreateForm(TAddonConfigForm, AddonConfigForm);
  Application.CreateForm(TVisControlForm, VisControlForm);
  Application.Run;
end.
