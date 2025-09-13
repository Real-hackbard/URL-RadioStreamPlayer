unit VisDrive;

interface

uses
  Windows, SysUtils, Messages, mmsystem, syncobjs, PluginCtrl, ioplug, Forms, wa_ipc, WasabiAPI;

type
   TChannelAttrb = record
      Title : string;
      FilePath : string;       // File path to the stream beging played ( * Added at Ver 1.2 )
      SampleRate : LongInt;    // Sampling rate in Hz
      BitRate   : LongInt;     // Bit Rate in KBPS
      Duration : DWORD;        // Song duration in mili second
      Channels : Word;         // Number of channels
   end;

  procedure SetBasicParams(HandleMainWin,         // * Added at Ver 1.2
                           MessageHandle,
                           StatusMsg : HWND;
                           ShareMemPointer : pointer;
                           ThreadPriority : integer;
                           LockFlag_ : TCriticalSection);   
  function Start_Vis(VisPlugin : string;
                      ModuleNum : integer;
                      EmbedHandle : HWND;
                   //   SyncMain : boolean;
                      UseGenVisDrawer : boolean;
                      EMBEDSwitch : TVisEMBEDSwitchMode;
                      PlayerModeId : integer;
                      ChannelIs : TChannelAttrb) : integer;
  procedure Stop_Vis;     // Stop vis plug-in.
  procedure Stop_Vis2;    // Stop vis plug-in at program termination.


implementation

  procedure HideVisWindow; forward;
  procedure ShowVisWindow; forward;
  procedure SetStatus(StatusId : DWORD); forward;

const
   CLASSNAME_WINAMP : pchar = 'Winamp v1.x';
   CLASSNAME_EMBED : pchar = 'Winamp Gen';
   TITLE_WINAMP : pchar = 'Winamp 2.40';
   TITLE_EMBED : pchar = 'Embed target';

 // Default edge margins for the vis window created by internal code.
   DefaultMarginWidth = 8;
   DefaultMarginHeight = 26;

   DefaultEMBEDWidth = 320;
   DefaultEMBEDHeight = 240;

 // Default edge margins for the vis window created by 'Gen_VisDrawer.dll'.
 // note) The 'Gen_VisDrawer.dll' is a customized Winamp general purpose plug-in which
 //       is used for the creator of EMBED window.
   EMBED_L_Margin = 11;   // Margin Left side
   EMBED_R_Margin = 8;    // Margin Right side
   EMBED_T_Margin = 20;   // Margin Top side
   EMBED_B_Margin = 14;   // Margin Bottom side
   EMBEDClientPosX = EMBED_L_Margin;
   EMBEDClientPosY = EMBED_T_Margin;

type
   TObsolitedEMBED = record
      WinHandle : HWND;
      EMBEDType : TVisWindowIs;
      WindowProc : Pointer;
      ChildRect : TRect;
   end;

var
   g_hInstance : HWND;
   MainWinHandle : HWND;     // handle to main window
   MainProcId : DWORD;
   MainThreadId : DWORD;
   DriveThreadId : DWORD = 0;
   ThreadHandle : HWND;
   VisPriority : integer;

   FakeWinHandle : HWND = 0;   // handle to fake Winamp window
   ParentWinHandle : HWND;     // Handle to parent window of vis window
   VisDLLHandle : THandle = 0;
   VisWinHandle : HWND = 0;    // handle to vis window
   VisChildHandle : HWND = 0;  // handle to child window of vis window
   VisDataPointer : pointer;

   Vismod : TVismod;
   VismodCounter : integer = 0;
   VismodIndex : integer = -1;

   OrgWindowProc : Pointer;
   MainWindowProc : pointer;

   MsgHandle : HWND;
   DataReadyMsg : HWND;
   EndByProgram : boolean;
   EndBySysMenu : boolean;
   LockFlag : TCriticalSection;

   PlayerMode : TPlayerMode = plmStandby;
   ChannelAttrb : TChannelAttrb;
 //  SyncWithMain : boolean;
   SongPosition : DWORD = 0;
   TitleP : pchar;
   FileP : pchar;

   VisModuleInfo : TVisModuleInfo;
   VisPluginInfo : TVisPluginInfo;
   GoRendering : boolean;
   VisualizationQuitting : boolean;

   getVisHeader2 : function(ParentHandle : HWND) : pointer; cdecl;

   UserEMBED : HWND;
   WindowEmbed : HWND = 0;
   EmbedClientWidth : integer;
   EmbedClientHeight : integer;
   TopBottomMargin : integer;
   LeftRightMargin : integer;

 //  WindowModeRect : TRect;
 //  SavedWindowRect : boolean;

   UseGPPModule : boolean;
   bEmbedClassRegistered : boolean = false;
   bFakeClassRegistered : boolean = false;
   ModeChanging : boolean;   // true = vis plug-in is changing its display mode
   VisPluginUnloading : boolean;
   VisWindowIs : TVisWindowIs;
   InThreadMsgLoop : boolean;
   StartFromReentry : boolean;

   Vis_Plugin : string;
   VismodNo   : integer;
   Reentering : boolean;
   VisualizationPaused : boolean;
   FirstShown : boolean;

   PosSkipCounter : integer;
   WinPos_X, WinPos_Y : integer;

   PostPending : boolean;

 // Set UseFakeWindow "TRUE" if you want to use a dedicated window which processes messages
 // from vis plug-in. (This is only to avoid any unexpected problem by using owner window's
 // message handler.)
 // If UseFakeWindow is set "FALSE" then messages from vis plug-in are processed by the message
 // handler of the owner of TBASSPlayer.
   UseFakeWindow : boolean = false;
   NowUnloadingPlugin : boolean;

   Vis_Width, Vis_Height : integer;
   WasHidden : boolean;

   EMBEDSwitchMode : TVisEMBEDSwitchMode;
   ObsolitedEMBED : TObsolitedEMBED;
   OrgParentWindow : HWND;
   OrgVisWindowIs : TVisWindowIs;
   WasPanelMode : boolean = false;

function SameThread(hOther : HWND) : boolean;
var
   dwOtherThreadId : DWORD;
   dwThisThreadId : DWORD;
   ProcId : DWord;
begin
   dwOtherThreadId := GetWindowThreadProcessId(hOther, @ProcId);
   dwThisThreadId := GetCurrentThreadId;
   result := (dwOtherThreadId = dwThisThreadId);
end;

{function Randomfunc : integer; cdecl;
begin
  Result := Random(32767);

end; }

procedure RestoreParent;
begin
   if VisChildHandle <> 0 then
   begin
      if ISWindow(VisChildHandle) then
      begin
         if OrgParentWindow <> GetParent(VisChildHandle) then
         begin
            SetParent(VisChildHandle, OrgParentWindow);
            VisWindowIs := OrgVisWindowIs;
         end;
      end;
   end;
end;

// This function handles the messages to be sent to vis plug-in.
// The original message handling routine of vis plug-in is performed after processing
// in this function.
function VisWindowProc(WndHandle : HWND; Msg : DWORD; wp, lp : Longint): longInt; stdcall;
var
   KeyMsg : TMsg;
   hChild : HWND;
   WindowRect, r : TRect;
   WndPos : ^TWindowPos;
begin
   if WndHandle <> VisWinHandle then  // Effective at window creation for (VisWindowsIs = CreatedByCode)
   begin
      if VisWindowIs <> CreatedByCode then
         result := CallWindowProc(OrgWindowProc, WndHandle, Msg, wp, lp)
      else
         result := DefWindowProc(WndHandle, Msg, wp, lp);
      exit;
   end;

   if (VisWindowIs = CreatedByCode) and NowUnloadingPlugin then
   begin
      result := DefWindowProc(WndHandle, Msg, wp, lp);  // * Changed (by advice of BassFan)
      exit;
   end;

   if (Msg = WM_SIZE) then
   begin
      hChild := GetWindow(WndHandle, GW_CHILD);
   // Adjust the size of visualization window according to that of EMBED window.
      if (hChild <> 0) then
      begin
      // vis_avs.dll post WM_SIZE message with lp = 0 at coming back (from full screen mode)
      //  to winodwed mode.    (lp : width & height of client area)
      // So, changed not to depend on the value of lp, now the client area is adjusted according
      //  to the size of EMBED window.
         if (VisWindowIs = CreatedByGPP) or (VisWindowIs = CreatedByCode) then
         begin
            GetWindowRect(WndHandle, r);
            Vis_Width := r.Right - r.Left {+ 1};
            Vis_Height := r.Bottom - r.Top {+ 1};
         end;

         if VisWindowIs = CreatedByGPP then
         // use the entire area of EMBED window except frame region
            SetWindowPos(hChild, 0,
                         EMBED_L_Margin, EMBED_T_Margin, // horizontal & vertical position
                         Vis_Width - LeftRightMargin,  // width
                         Vis_Height - TopBottomMargin, // height
                         SWP_NOZORDER)
         else if VisWindowIs = CreatedByCode then
         begin
         // use the entire area of EMBED window except frame region
            SetWindowPos(hChild, 0,
                         0, 0,    // horizontal position, vertical position
                         Vis_Width - LeftRightMargin, // r.Right - r.Left {+ 1}, // width
                         Vis_Height - TopBottomMargin, // r.Bottom - r.Top {+ 1}, // height
                         SWP_NOZORDER);
         end
         else if VisWindowIs = OnAssignedWindow then
         begin
            GetClientRect(UserEMBED, r);
            SetWindowPos(hChild, 0,
                         0, 0,    // horizontal position, vertical position
                         r.Right - r.Left,    // window width = the width of client area
                         r.Bottom - r.top,    // window height = the height of client area
                         SWP_NOZORDER);
            result := 0;
            exit;
         end
      end
   end else if (Msg = WM_WINDOWPOSCHANGING) then
   begin
  // MilkDrop tends to move its visualization window after showing it up.
  // So, we need to put previously specified cordinates several times at receiving
  // WM_WINDOWPOSCHANGING to prevent reposition of visualization window.
      if ((VisWindowIs = CreatedByGPP) or (VisWindowIs = CreatedByCode))
                                                   and (PosSkipCounter > 0) then
      begin
         WndPos := pointer(lp);
         WndPos^.x := WinPos_X;
         WndPos^.y := WinPos_Y;
       //  if (VisWindowIs = CreatedByGPP) then
       //  begin
            WndPos^.cx := EmbedClientWidth + LeftRightMargin;
            WndPos^.cy := EmbedClientHeight + TopBottomMargin;
       //  end;
         dec(PosSkipCounter);
      end
   end
  { else if (Msg = WM_WINDOWPOSCHANGED) then
      GoRendering := true  }
   else if (Msg = WM_NCLBUTTONDOWN) then  // * Added at Ver 1.2
      PosSkipCounter := 0                 //  to release unmovable state
   else if (Msg = WM_DESTROY) then
   begin
      GoRendering := false;

      if VisWindowIs <> UnAvailable then
         if (not EndByProgram) and (not EndBySysMenu) then   
            PostMessage(MsgHandle, DataReadyMsg, PauseVisOut, 0);

      if (not ModeChanging) then
      begin
         if (not EndByProgram) and (not EndBySysMenu) then
            if InThreadMsgLoop then
            begin
            // Inform message handler that visualization is being ended.
               VisualizationQuitting := true;
               PostThreadMessage(DriveThreadId, WM_QUIT, 0, 0);
            end;
      end;

      if (VisWindowIs = CreatedByGPP) or (VisWindowIs = CreatedByCode) then
      begin
      	 hChild := GetWindow(WndHandle, GW_CHILD);
         if hChild <> 0 then
            if SameThread(hChild) then
               DestroyWindow(hChild);

         if (VisWindowIs = CreatedByGPP) then
            SetGPPInactive;
      end;

      FirstShown := true;
      VisWindowIs := UnAvailable;
   end;

 // Relay WM_KEYDOWN and WM_CHAR messages to vis plug-in.
   if (WindowEmbed <> 0) and (VisChildHandle <> 0) then
   begin
      case (Msg) of
         WM_KEYDOWN : begin
                         PostMessage(VisChildHandle, Msg, wp, lp);
                         KeyMsg.hwnd := WndHandle;
                         KeyMsg.message := WM_KEYDOWN;
                         KeyMsg.wParam := wp;
                         KeyMsg.lParam := lp;
                         TranslateMessage(KeyMsg);

                      end;
        { WM_KEYUP   : begin
                         PostMessage(VisChildHandle, Msg, wp, lp);;
                      end; }
         WM_CHAR :    begin
                         PostMessage(VisChildHandle, Msg, wp, lp);;
                      end;
        { WM_GETMINMAXINFO : begin
                         pMinMaxInfo := pointer(lp);
                         pMinMaxInfo^.ptMaxSize.X := Screen.Width + LeftRightMargin;
                         pMinMaxInfo^.ptMaxSize.Y := Screen.Height + TopBottomMargin;
                         result := 0;  // Exit here
                         exit;
                      end; }
      end;
   end;

   if (VisWindowIs <> CreatedByPlugin) then
   begin
      case (Msg) of
        WM_PARENTNOTIFY : begin
                           case LOWORD(wp) of
                              WM_DESTROY : begin  // vis window is being destroyed
                                              hChild := GetWindow(WndHandle, GW_CHILD);
                                              if not SameThread(hChild) then
                                              begin
                                              // Vis plugin
                                                 DestroyWindow(WndHandle);
                                              end;

                                           end;

                           end;
                        end;

        WM_SHOWWINDOW : if (FirstShown and (not PostPending)) or WasHidden then
                        begin
                          hChild := GetWindow(WndHandle, GW_CHILD);
                          if (wp <> 0) then // Shown
                          begin
                            if (not WasHidden) then
                            begin
                          // Update vis window's position & size according to the characteristics of EMBED window.
                              if VisWindowIs = OnAssignedWindow then // The EMBED window (strictly speaking the parent
                              begin                                  // window of EMBED window) is specified by user
                                windows.GetClientRect(WndHandle, r);
                            // Use the entire client area of EMBED window
                                MoveWindow(hChild, r.Left, r.top, r.Right - r.Left, r.Bottom - r.top, true);
                              end
                              else if (VisWindowIs = CreatedByCode) then  // The EMBED window is created by intenal code
                              begin
                                SetWindowPos(WndHandle, 0,
                                             WinPos_X, WinPos_Y,  // horizontal position, vertical position : not used
                                             EmbedClientWidth + LeftRightMargin,   // width
                                             EmbedClientHeight + TopBottomMargin,  // height
                                             {SWP_NOMOVE +} SWP_NOZORDER);
                                PosSkipCounter := 2;
                              end
                              else if (VisWindowIs = CreatedByGPP) then  // The EMBED window is created by Gen_VisDrawer.dll
                              begin
                                SetWindowPos(WndHandle, 0,
                                             0, 0,   // horizontal position, vertical position : not used
                                             EmbedClientWidth + LeftRightMargin,   // width
                                             EmbedClientHeight + TopBottomMargin,  // height
                                             SWP_NOMOVE + SWP_NOZORDER);
                              // The window created by 'Gen_VisDrawer.dll' is a frame-less window.
                              // So, we should consider frame width and frame height at window edge.
                                MoveWindow(hChild, EMBED_L_Margin, EMBED_T_Margin,
                                           EmbedClientWidth,
                                           EmbedClientHeight, TRUE);

                                PosSkipCounter := 2;  // Just for MilkDrop to prevent reposition of vis window.
                              end;

                           // Put extra code if you want to restore vis window to previous size and position at changing
                           // display mode from full screen mode to window mode.
                           //  if SavedWindowRect then
                            {  begin
                                 SetWindowPos(WndHandle, 0,
                                              WindowModeRect.Left, WindowModeRect.Top,
                                              WindowModeRect.Right - WindowModeRect.Left,
                                              WindowModeRect.Bottom - WindowModeRect.Top,
                                              SWP_NOMOVE + SWP_NOZORDER);
                                 MoveWindow(hChild, EMBED_L_Margin, EMBED_T_Margin,
                                            WindowModeRect.Right - WindowModeRect.Left - LeftRightMargin,
                                            WindowModeRect.Bottom - WindowModeRect.Top - TopBottomMargin, TRUE);
                              end;  }

                           //   SavedWindowRect := false;
                              ModeChanging := false;
                              OrgParentWindow := VisWinHandle;
                              OrgVisWindowIs := VisWindowIs;
                              VisChildHandle := hChild;
                              with VisPluginInfo do
                              begin
                                ThreadId := DriveThreadId;
                                VisHandle := VisWinHandle;
                                VisType := VisWindowIs;
                                PluginPath := pchar(Vis_Plugin);
                                ModNo   := VismodNo;
                                StartType := (not Reentering);
                              end;

                           // Inform main thread that vis module has been launched successfully.
                              PostMessage(MsgHandle, DataReadyMsg, StartVisOut, DWORD(@VisPluginInfo));

                              FirstShown := false;
                              Reentering := false;

                            end else // end of "if (not WasHidden) then"
                                WasHidden := false;

                              GoRendering := true;  // Ready to go !
                          end else
                          begin   // for (wp = 0)
                            GoRendering := false;
                            WasHidden := true;
                          end;
                        end else
                          if (wp = 0) then
                          begin
                             GoRendering := false;
                             WasHidden := true;
                          end;

        WM_SYSCOMMAND : begin
                         if ((wp and $FFF0) = SC_CLOSE) then   // Closes the window.
                         begin
                       // Process for the user action of mouse button click on "Close" button area
                       //  on vis window which created by internal code or by Gen_VisDrawer.dll
      	                    hChild := GetWindow(WndHandle, GW_CHILD);
      	                    if SameThread(hChild) then
                            begin
                               PostMessage(MsgHandle, DataReadyMsg, PauseVisOut, 0);
                               EndBySysMenu := true;
                               VisualizationQuitting := true;
                               PostThreadMessage(DriveThreadId, WM_QUIT, 0, 0);
                               result := 0;
                               exit;
                            end;
                         end;
                      end;

     end;  // end of case

   end  // end of "(VisWindowIs <> CreatedByPlugin)"
   else if Msg = WM_SHOWWINDOW then  // for VisWindowIs = CreatedByPlugin
   begin
      if FirstShown or WasHidden then // Shown
      begin
         if (wp <> 0) then
         begin
           FirstShown := false;

           if (not WasHidden) then
           begin
        //  Move vis window to, top to top, right side of main window (snap to main window)
             if IsWindow(MainWinHandle) then
             begin
               GetWindowRect(WndHandle, r);
            // if the width of vis window is less than half of screen width (considering
            // Desktop mode or Full screen mode), put vis window at top-right side of main window
                if (r.Right - r.Left) < (Screen.Width div 2) then
                begin
                  GetWindowRect(MainWinHandle, WindowRect);
                  SetWindowPos(WndHandle, 0,
                               WindowRect.Right, WindowRect.Top,
                               0, 0,        // width, height : not used
                               SWP_NOSIZE + SWP_NOZORDER);
                end;
             end;

          // Inform main thread that the vis module has been launched successfully.
             with VisPluginInfo do
             begin
               ThreadId := DriveThreadId;
               VisHandle := VisWinHandle;
               VisType := VisWindowIs;
               PluginPath := pchar(Vis_Plugin);
               ModNo   := VismodNo;
               StartType := (not Reentering);
             end;

             ModeChanging := false;
             PostMessage(MsgHandle, DataReadyMsg, StartVisOut, DWORD(@VisPluginInfo));

             Reentering := false;

           end else // end of "if (not WasHidden) then"
             WasHidden := false;

           GoRendering := true;
         end else
         begin   // for (wp = 0)
           GoRendering := false;
           WasHidden := true;
         end;
      end else
         if (wp = 0) then
         begin
            GoRendering := false;
            WasHidden := true;
          end;
   end;

 // Call the original Window procedure of vis window
   if VisWindowIs <> CreatedByCode then
      result := CallWindowProc(OrgWindowProc, WndHandle, Msg, wp, lp)
   else
      result := DefWindowProc(WndHandle, Msg, wp, lp);
end;

function CreateChildWindow(EMBEDWindow : HWND) : HWND;
var
   R2 : TRect;
begin
   Result := 0;

   if IsWindow(EMBEDWindow) then
     if IsWindowVisible(EMBEDWindow) then
     begin
       windows.GetClientRect(EMBEDWindow, R2);

     // Create a child window on the window specified by user.
       Result := CreateWindowEx(WS_EX_CLIENTEDGE,      // extended window style
                                     'STATIC',              // pointer to registered class name
                                     TITLE_EMBED,           // pointer to window name
                                     WS_CHILD or            // window style = Child window &
                                     SS_BLACKFRAME,         //                Black window frame
                                     R2.Left,               // horizontal position of window
                                     R2.top,                // vertical position of window
                                     R2.Right - R2.Left,    // window width = the width of client area
                                     R2.Bottom - R2.top,    // window height = the height of client area
                                     EMBEDWindow,           // handle to parent or owner window
                                     0,                     // handle to menu, or child-window identifier
                                     g_hInstance,           // handle to application instance
                                     nil);                  // pointer to window-creation data


     end;
end;

// This function is called by Winamp 5 vis plug-in and creates a window which becomes
// the EMBED window for visualization.
function MyembedWindow(ews : PembedWindowState) : HWND; cdecl;
var
  wc : TWNDCLASS;
  WindowRect, R2 : TRect;
  Win_Width, Win_Height : integer;
  GPPWinHandle : HWND;
  InitWinPos : TInitWinPos;
begin
   WindowEmbed := 0;
   PosSkipCounter := 0;

   if (UserEMBED = 0) then
     if IsWindow(MainWinHandle) then
       begin
         GetWindowRect(MainWinHandle, WindowRect);
         WinPos_X := WindowRect.Right;
         WinPos_Y := WindowRect.Top;
       end else begin
         if ews <> nil then
         begin
            WinPos_X := (Screen.Width - ews^.r.Right - ews^.r.Left) div 2;
            WinPos_Y := (Screen.Height - ews^.r.Bottom - ews^.r.top) div 2;
         end else
         begin
            WinPos_X := (Screen.Width - DefaultEMBEDWidth) div 2;
            WinPos_Y := (Screen.Height - DefaultEMBEDHeight) div 2;
         end;
       end;

   if (UserEMBED <> 0) then   // if it is set to use a specified window as EMBED window
   begin
  // note) EMBED window is destroyed at quitting visualization or at display mode change.
  //      So, we should avoid to use the EMBED window specified by user directly.
  //      Guess, a panel(which was used a EMBED window) on main form has been disappered after
  //      running vis plug-in !
  //      In this program, 'UserEMBED' specified by user is used as the parent of EMBED window.
  //      And the child window created on the surface of 'UserEMBED' is used as EMBED window.
      WindowEmbed := CreateChildWindow(UserEMBED);
      if WindowEmbed <> 0 then    // if succeed
         VisWindowIs := OnAssignedWindow;
   end else
   if UseGPPModule then                    // if set to use Vis_GenDrawer.DLL
   begin
      InitWinPos.pos_x := WinPos_X;
      InitWinPos.pos_y := WinPos_Y;
      if ews <> nil then
      begin
        InitWinPos.width :=  (ews^.r.Right - ews^.r.Left) + EMBED_L_Margin + EMBED_R_Margin;
        InitWinPos.height :=  (ews^.r.Bottom - ews^.r.Top) + EMBED_T_Margin + EMBED_B_Margin;
      end else
      begin
         InitWinPos.width := DefaultEMBEDWidth;
         InitWinPos.height := DefaultEMBEDHeight;
      end;
      InitWinPos.close_action := 2;  // 2 : determined in user application program
      GPPWinHandle := StartGPPModule(MainWinHandle, DriveThreadId, InitWinPos);
      if GPPActive then
      begin
         WindowEmbed := GPPWinHandle;
         VisWindowIs := CreatedByGPP;
      end;
   end;

   if (WindowEmbed = 0) then  // If have not gotten EMBED window yet then create EMBED window by code
   begin
     // Register class
      if (not bEmbedClassRegistered) then
      begin
         fillchar(wc, sizeof(wc), 0);
         with wc do
         begin
           Style         := CS_DBLCLKS or CS_HREDRAW or CS_VREDRAW;
           lpfnWndProc   := @VisWindowProc;
           cbClsExtra    := 0;
           cbWndExtra    := 0;
           hInstance     := g_hInstance;
           hIcon         := LoadIcon(0, IDI_APPLICATION); // 0;
           hCursor       := LoadCursor(0, IDC_ARROW);
           hbrBackground := COLOR_WINDOW + 1;
           lpszMenuName  := nil;
           lpszClassName := CLASSNAME_EMBED;
         end;

         if (windows.RegisterClass(wc) = 0) then
         begin
            result:= 0;
            exit;
         end;

         bEmbedClassRegistered := true;
      end;

      VisWindowIs := CreatedByCode; // Put this code prior to executing CreateWindowEx.
                                    // note) 'VisWindowProc' is called during creation process.
      PostPending := true;          // supress Posting Message which informs EMBED is ready
                                    //    PostMessage(MsgHandle, DataReadyMsg, StartVisOut...
                                    // why -> WM_SHOWWINDOW message is received during creation of
                                    //        EMBED window

     // Create window
      if ews <> nil then
      begin
         Win_Width := (ews^.r.Right - ews^.r.Left) + DefaultMarginWidth;
         Win_Height := (ews^.r.Bottom - ews^.r.Top) + DefaultMarginHeight;
      end else
      begin
         Win_Width := DefaultEMBEDWidth;
         Win_Height := DefaultEMBEDHeight;
      end;

      WindowEmbed := CreateWindowEx(WS_EX_WINDOWEDGE  or   // extended window style
                                    WS_EX_TOOLWINDOW,      //
                                    CLASSNAME_EMBED,       // pointer to registered class name
                                    TITLE_EMBED,           // pointer to window name
                                    WS_OVERLAPPED or       // window style
                                    WS_CLIPCHILDREN or     //
                                    WS_BORDER or           //
                                    WS_CAPTION or          //
                                    WS_SYSMENU or          //
                                    WS_THICKFRAME or       //
                                    WS_MINIMIZEBOX or      //
                                    WS_MAXIMIZEBOX or      //
                                    WS_POPUP,              //
                                    WinPos_X,              // horizontal position of window
                                    WinPos_Y,              // vertical position of window
                                    Win_Width,             // window width
                                    Win_Height - 1,        // window height  (** note)
                                    MainWinHandle,         // handle to parent or owner window
                                    0,                     // handle to menu, or child-window identifier
                                    g_hInstance,           // handle to application instance
                                    nil );                 // pointer to window-creation data

     if WindowEmbed = 0 then    // if failed
         VisWindowIs := UnAvailable;

    // (** note) window height is intensionally set to incorrect value ->
    //       vis_avs.dll shows reduced client area because WM_SIZE message is not posted
    //       after handling of WM_SHOWWINDOW message, if the initial size is given precisely.
   end;

   if WindowEmbed <> 0 then   // EMBED window is successfully created
   begin
      VisWinHandle := WindowEmbed;
      if VisWindowIs <> CreatedByCode then
         OrgWindowProc := Pointer(SetWindowLong(VisWinHandle, GWL_WNDPROC,
                                                       LongInt(@VisWindowProc)));

      if (ews = nil) or (ews^.me = 0) then
      begin
        if ews <> nil then
        begin
         // if ews^.me = 0 then
              ews^.me := WindowEmbed;

           TopBottomMargin := 0;
           LeftRightMargin := 0;
           if VisWindowIs = CreatedByGPP then
           begin
              LeftRightMargin := EMBED_L_Margin + EMBED_R_Margin;
              TopBottomMargin := EMBED_T_Margin + EMBED_B_Margin;
           end else
           begin
              if GetWindowRect(WindowEmbed, WindowRect) then
                 if GetClientRect(WindowEmbed, R2) then
                 begin
                 // Calculate frame margin to determine the size of EMBED window
                    TopBottomMargin :=
                        (WindowRect.Bottom - WindowRect.Top) - (R2.Bottom - R2.Top);
                    LeftRightMargin :=
                        (WindowRect.Right - WindowRect.Left) - (R2.Right - R2.Left);
                 end;
           end;

           if VisWindowIs = OnAssignedWindow then   // if we use a specified window as EMBED window
           begin
              EmbedClientWidth := R2.Right - R2.Left {+ 1};
              EmbedClientHeight := R2.Bottom - R2.Top {+ 1};
           end
           else if (ews^.r.Right > ews^.r.Left) and (ews^.r.Bottom > ews^.r.Top) then
           begin
            // The width and height calculated from ews^.r specifies the size of vis window.
            // EMBED window should have extra margin for frame region such as caption bar, border edge.
            // The size of EMBED window is adjusted in WM_SHOWWINDOW message processing routine later.
              EmbedClientWidth := ews^.r.Right - ews^.r.Left {+ 1};
              EmbedClientHeight := ews^.r.Bottom - ews^.r.Top {+ 1};
           end;
        end;

        result := WindowEmbed;
        exit;
      end;

      SetParent(ews^.me, WindowEmbed);
   end;

   result := WindowEmbed;
end;

procedure GoFullScreen;
{var
  // TaskWindowHandle : HWnd; }
begin
 // Milkdrop destroys the window used for visualization at changing display mode.
 // Because the detection of WM_DESTROY forwarding vis window is regarded as termination
 // of visualization, we should set ModeChanging to true to prevent quitting vis plug-in.
   if not VisPluginUnloading then
      ModeChanging := true;  // * Added at Ver 1.2

 // If current type of vis window is not original type of vis window(ex. a vis plug-in started
 // in non-Panel mode and switched to Panel mode during the vis plug-in was running), then we
 // should restore its type to original type otherwise some problem happens.
   if EMBEDSwitchMode = WindowMove then
     if VisWinHandle <> OrgParentWindow then
     begin
       if ObsolitedEMBED.EMBEDType <> OnAssignedWindow then
       begin
         windows.SetParent(VisChildHandle, OrgParentWindow);
         ObsolitedEMBED.WinHandle := 0;
         WasPanelMode := true;
         SetWindowLong(VisWinHandle, GWL_WNDPROC, LongInt(OrgWindowProc));
      //  DestroyWindow(VisWinHandle);    // DestroyWindow is not effective, why ?
         ShowWindow(VisWinHandle, SW_HIDE);  // So, I should ShowWindow as a alternate

      // Restore to orginal type of vis window
         VisWinHandle := OrgParentWindow;
         VisWindowIs := OrgVisWindowIs;
         OrgWindowProc := ObsolitedEMBED.WindowProc;
       end;
     end;

 //  FullScreenMode := true;
 //  if (VisWinHandle = 0) or (VisChildHandle = 0) then
 //     exit;

 // Save the size and position of vis window for restoring at back to windowed mode.
  { if GetWindowRect(VisWinHandle, WindowModeRect) then
      SavedWindowRect := true;  }

 // Hide task bar
 {  TaskWindowHandle := FindWindow('Shell_TrayWnd', '');
   if TaskWindowHandle<>0 then
      ShowWindow(TaskWindowHandle, SW_HIDE);  }
end;

procedure GoWindowMode;
{var
   TaskWindowHandle : HWnd;  }
begin
   if not VisPluginUnloading then
      ModeChanging := true;

 //  FullScreenMode := false;

 //  if (VisWinHandle = 0) or (VisChildHandle = 0) then
 //     exit;

 // Show task bar
  { TaskWindowHandle := FindWindow('Shell_TrayWnd', '');
   if TaskWindowHandle<>0 then
      ShowWindow(TaskWindowHandle, SW_SHOW);  }
end;


// This FakeWinampProc function processes some essential Winamp IPC messages sent from
// vis plug-in to application.
// See Winamp_IPC.txt for Winamp IPC messages.
function FakeWinampProc(WndHandle: HWND; Msg : DWORD; wParam, lParam : Longint) : Longint; stdcall;
var
   ews : PembedWindowState;
   cds : ^COPYDATASTRUCT;
   iLen : integer;

begin
   Result := 0;  // Put default value

   if Msg = WM_WA_IPC then
   begin
      if LParam = IPC_GETVERSION then
      //   Result := $2040    // acts as if the main program is Winamp ver 2.4
         Result := $5010    // acts as if the main program is Winamp ver 5.1
      else if LParam = IPC_ISPLAYING then
      begin
         if PlayerMode = plmPlaying then
            Result := 1
         else if PlayerMode = plmPaused then
            Result := 3;
      end else if LParam = IPC_GETINFO then
      begin
         if WParam = 0 then           // Sample rate
            Result := ChannelAttrb.SampleRate
         else if WParam = 1 then      // Bit rate
            Result := ChannelAttrb.BitRate
         else if WParam = 2 then      // Channels
            Result := ChannelAttrb.Channels;
      end
      else if LParam = IPC_GETOUTPUTTIME then
      begin
         if WParam = 0 then      // position in miliseconds
            if (PlayerMode = plmPlaying) or (PlayerMode = plmPaused) then
               Result := SongPosition
            else
               Result := -1
         else if WParam = 1 then      // song length in seconds
            Result := ChannelAttrb.Duration div 1000;
      end
      else if LParam = IPC_GETLISTLENGTH then
       // return playlist->GetSize();
         Result := 1
      else if LParam = IPC_GETLISTPOS then
       // return playlist->GetCurIndex();
         Result := 0       // Result := 0  by default
      else if LParam = IPC_GETPLAYLISTTITLE then
      begin
       // WParam : index of playlist entry
         TitleP := pchar(ChannelAttrb.Title);
         Result := integer(TitleP);
      end
      else if LParam = IPC_GETPLAYLISTFILE then
      begin
       // WParam : index of playlist entry
         FileP := pchar(ChannelAttrb.FilePath);
         Result := integer(FileP);
      end
      else if LParam = IPC_GETINIFILE then   
      begin
         FileP := pchar(ExtractFilePath(ParamStr(0)) + 'Plugins\plugin.ini');
         Result := integer(FileP);
      end
     { else if LParam = IPC_GET_RANDFUNC then
         Result := longint(@Randomfunc)
      else if LParam = IPC_JUMPTOTIME then
      begin
      // Returns -1 if not playing, 1 on eof, or 0 if successful
         Result := ;
      end
      else if LParam = IPC_GETWND then
      begin
         if WParam = IPC_CB_WND_MAIN then
            Result := MainWinHandle
         else
            Result := 0;
      end
      else if LParam = IPC_GETINIDIRECTORY then
      begin
          // Add Code later
      end
      else if LParam = IPC_ISWNDVISIBLE then
      begin
         if WParam = IPC_GETWND_PE then
            Result := 1
         else
            Result := 0;
      end
      else if LParam = IPC_GET_GENSKINBITMAP then
      begin
         if WParam = 0 then
         begin
             // Add code later
         end;
      end
      else if WParam = IPC_HOOK_OKTOQUIT then
         Result := 1  }
      else if LParam = IPC_IS_PLAYING_VIDEO then
         Result := 0       // 0 : not playing video
      else if LParam = IPC_IS_FULLSCREEN then
      begin                                     
         if (WParam = 1) or (WParam = 0) then   // Finished display mode change ?
         begin
            GoRendering := true;
            ModeChanging := false;
         end;

      // Following code is for vis_BassVis.dll plug-in at display mode change (Full -> Windowed)
         if WasPanelMode and (WParam = 0) then
         begin
            WasPanelMode := false;
            if VisWindowIs <> OnAssignedWindow then
               PostThreadMessage(DriveThreadId, DataReadyMsg, ChangeEmbedWindow, UserEMBED);
         end
      end
      else if LParam = IPC_GET_EMBEDIF then
      begin
         if WParam <> 0 then
         begin
            ews := pointer(WParam);
            Result := MyembedWindow(ews)
         end else
            Result := longint(@MyembedWindow);
      end
      else if LParam = IPC_SETVISWND then  // This message is posted by Winamp 5 Vis plug-ins only
      begin                                // after creation vis window or at closing vis window.
       //  WParam = 0 : vis window is bound to be closed.
       //   note) Milkdrop 2.0d posts this message only if a AND b,
       //    a. The vis window was created by itself, i.e., for the case VisWindowIs = CreatedByPlugin.
       //    b. It is changing display mode (does not post at quitting visualization)
       //--------------------------------------------------------------------------------------------

       //  Table : Relation between display mode change and "ModeChanging"
       // --------------------------------------------------------------------
       // | VisWindowIs at Windowed mode | D -> W | W -> D | F -> W | W -> F |
       // |------------------------------|--------|--------|--------|--------|
       // |       CreatedByPlugin        |  O (a) |  O (a) |  O (b) | O (b)  |
       // |------------------------------|--------|--------|--------|--------|
       // |   not CreatedByPlugin        |  O (a) |   X    |  O (b) | O (b)  |
       // --------------------------------------------------------------------
       //  D : Desktop mode    W : Windowed mode    F : Full screen mode
       //  O (a) : "ModeChanging" is set true in the IPC_SETVISWND message handling routine
       //  O (b) : "ModeChanging" is set true in the IPC_SET_VIS_FS_FLAG message handling routine
       //     X  : "ModeChanging" is not affected
       //  note) This table is valid only for MilkDrop 2.0d

         if WParam = 0 then
         begin
            GoRendering := false;
            VisChildHandle := 0;
            PostMessage(MsgHandle, DataReadyMsg, PauseVisOut, 0);

            if not VisPluginUnloading then
              if (not ModeChanging) then
                 ModeChanging := true; // Maybe changing display mode (to/from Desktop mode)
         end else
         begin   //  WParam <> 0 : vis window is available  (= after creation of vis window )
          // Following 'if clause' is needed to differentiate initial creation from re-creation
          //  after destruction (at display mode change)of vis window.
          // The case of (VisWindowIs = UnAvailable) happens only if vis window is re-created by
          //  vis plug-in.
          // If vis plug-in in use uses EMBED window then VisWindowIs is set before receiving this
          // IPC_SETVISWND message.
          // If we set Milkdrop 2.0d start in Desktop mode or full ssreen mode then VisWinHandle is
          //  obtained from here.
            if (VisWindowIs = UnAvailable) or   // Windowed mode -> Desktop, Full screen mode
                 ((VisWindowIs = CreatedByPlugin) and StartFromReentry) then   // Initial Strart-up
            begin
               if VisualizationQuitting then
                  VisualizationPaused := true;  // Windowed mode -> Desktop mode
               VisualizationQuitting := false;  // to prevent exiting VisThread loop
               VisWinHandle := WParam;

               if (VisWindowIs = UnAvailable) then
               begin
                  VisWindowIs := CreatedByPlugin;
                  if ModeChanging then  // = the program flow is in thread's message loop
                  begin                 // So, following 2 sentences should be executed here
                     OrgWindowProc := Pointer(SetWindowLong(VisWinHandle, GWL_WNDPROC,
                                                         LongInt(@VisWindowProc)));
                     PostMessage(VisWinHandle, WM_SHOWWINDOW, 1, 0);
                  end;
               end;
            end;

          // Desktop, Full screen mode -> Windowed mode
            if PostPending and ModeChanging then
            begin
              PostPending := false;
              if VisWindowIs = CreatedByCode then
                 PostMessage(VisWinHandle, WM_SHOWWINDOW, 1, 0);
            end;

            ModeChanging := false;
         end;

      end
      else if LParam = IPC_GETTIMEDISPLAYMODE then
	       Result := 0 // == elapsed time
      else if LParam = IPC_SET_VIS_FS_FLAG then  // Now changing display mode
      begin                         // note) not applied from/to desktop mode
         GoRendering := false;
         if WParam = 0 then      // go windowed mode
            GoWindowMode
         else if WParam = 1 then // go full screen mode
            GoFullScreen;
      end
      else if LParam = IPC_GET_API_SERVICE then   // * New at Ver 1.2
         Result := WaAPIServiceEntry;
    //  end else
       //  Result := 0;   // for currently not implemented/supported  IPC message
       // note) Don't return "1", it may cause fatal error. (ex. vis_avs.dll)
   end else if Msg = WM_COPYDATA then
   begin
      if LParam = 0 then
      begin
         if UseFakeWindow then
            Result := DefWindowProc(WndHandle, Msg, wParam, lParam)
         else
            Result := CallWindowProc(MainWindowProc, WndHandle, Msg, wParam, lParam);
      end else
      begin
         cds := pointer(LParam);
         case cds^.dwData of
            IPC_PLAYFILE : begin
                             iLen := cds.cbData;
                             if iLen <> 0 then
                             begin
                            // Following sentences are left without conversion (C++ to pascal)
                              { TCHAR * szKeep = new TCHAR[ iLen + 1 ];
                                ToTchar( szKeep, ( char * )cds->lpData, iLen );
                                szKeep[ iLen ] = TEXT( '\0' );

                                playlist->PushBack( szKeep ); }
                                result := 1;
                             end else
                                result := 0;
                           end;
            else begin
               if UseFakeWindow then
                  Result := DefWindowProc(WndHandle, Msg, wParam, lParam)
               else
                  Result := CallWindowProc(MainWindowProc, WndHandle, Msg, wParam, lParam);
            end;
         end;

      end;

  // Response to message WM_GETTEXT is to hand over the title of playing stream file to
  // vis plug-in.
  // You must add ' - Winamp' to the title string because some vis plug-ins show
  // erroneous operation if it is missed (ex. vis_Bass-C.dll)
   end else if Msg = WM_GETTEXT then
   begin
      TitleP := pchar(ChannelAttrb.Title + ' - Winamp');
      Move(TitleP^, (pointer(lParam))^, length(ChannelAttrb.Title) + 10);
      Result := length(ChannelAttrb.Title) + 10;
   end else if Msg = WM_COMMAND then    // * Added at Ver 1.2
   begin
      if WParam = WINAMP_VOLUMEUP then
         PostMessage(MsgHandle, WM_RequestFromVis, integer(REQ_VOLUMEUP), 0)
      else if WParam = WINAMP_VOLUMEDOWN then
         PostMessage(MsgHandle, WM_RequestFromVis, integer(REQ_VOLUMEDOWN), 0)
      else if (WParam = WINAMP_FFWD5S) or (WParam = WINAMP_BUTTON5_SHIFT) then
      begin
         if PlayerMode = plmPlaying then
            PostMessage(MsgHandle, WM_RequestFromVis, integer(REQ_FFWD5S), 0);
      end
      else if (WParam = WINAMP_REW5S) or (WParam = WINAMP_BUTTON1_SHIFT) then
      begin
         if PlayerMode = plmPlaying then
            PostMessage(MsgHandle, WM_RequestFromVis, integer(REQ_REW5S), 0);
      end
      else if WParam = WINAMP_BUTTON1 then    // Prev
         PostMessage(MsgHandle, WM_RequestFromVis, integer(REQ_PREV), 0)
      else if WParam = WINAMP_BUTTON2 then    // Play
      begin
         if (PlayerMode = plmStopped) or (PlayerMode = plmPaused) then
            PostMessage(MsgHandle, WM_RequestFromVis, integer(REQ_PLAY), 0);
      end
      else if WParam = WINAMP_BUTTON3 then    // Pause
      begin
         if PlayerMode = plmPlaying then
            PostMessage(MsgHandle, WM_RequestFromVis, integer(REQ_PAUSE), 0);
      end
      else if WParam = WINAMP_BUTTON4 then    // Stop
      begin
         if PlayerMode = plmPlaying then
            PostMessage(MsgHandle, WM_RequestFromVis, integer(REQ_STOP), 0);
      end
      else if WParam = WINAMP_BUTTON5 then    // Next
         PostMessage(MsgHandle, WM_RequestFromVis, integer(REQ_NEXT), 0);

      if UseFakeWindow then
         Result := DefWindowProc(WndHandle, Msg, wParam, lParam)
      else
         Result := CallWindowProc(MainWindowProc, WndHandle, Msg, wParam, lParam);
   end else
      if UseFakeWindow then
         Result := DefWindowProc(WndHandle, Msg, wParam, lParam)
      else
         Result := CallWindowProc(MainWindowProc, WndHandle, Msg, wParam, lParam);

end;

// Create a fake window which acts as if it is the one of Winamp's main window
// Following 2 functions are not necessary if the owner of TBassPlayer is set
// as 'fake window'.
function CreateFakeWindow : HWND;
var
   WinAtom : TAtom;
   wc      : TWNDCLASS;
begin
   if FakeWinHandle <> 0 then   // Avoid duplicate creation of fake Winamp window
   begin
      result := FakeWinHandle;
      exit;
   end;

   if not bFakeClassRegistered then
   begin
      with wc do
      begin
         Style         := 0;                // UINT style
         lpfnWndProc   := @FakeWinampProc;  // WNDPROC lpfnWndProc
         cbClsExtra    := 0;                // int cbClsExtra
         cbWndExtra    := 0;                // int cbWndExtra
         hInstance     := g_hInstance;      // HINSTANCE hInstance
         hIcon         := 0;                // LoadIcon(hInst, 'MAINICON');
         hCursor       := LoadCursor(0, IDC_ARROW);
         hbrBackground := COLOR_WINDOW;
         lpszMenuName  := nil;
         lpszClassName := CLASSNAME_WINAMP;
      end;

   // Once our class is registered we can start making windows with it
      WinAtom := windows.RegisterClass(wc);
      if WinAtom <> 0 then
         bFakeClassRegistered := true;
   end;

   if bFakeClassRegistered then
   begin
      result := CreateWindowEx(0, CLASSNAME_WINAMP, TITLE_WINAMP,
                               WS_POPUP,              //
                               5, 5, 35, 35,          // x, y, width, height
                               0, 0, g_hInstance, nil);
    // Move window to the outer region of screen to hide it.
      if result <> 0 then
         MoveWindow(result, -50,    // horizontal position (minus value : to hide window)
                              5,    // vertical position
                             35,    // width
                             35,    // height
                          false);   // repaint flag

   end else
      result := 0;

end;

function DestroyFakeWindow : boolean;
begin
   if FakeWinHandle <> 0 then
   begin
      result := DestroyWindow(FakeWinHandle);
      if result then
      begin
         FakeWinHandle := 0;

    // We may repeatedly create FakeWindow, so leave the window class in registered state.
    // note) All window classes that an application registers are unregistered when it
    //      terminates.
        { if bFakeClassRegistered then
         begin
            windows.UnRegisterClass(CLASSNAME_WINAMP, g_hInstance);
            bFakeClassRegistered := false;
         end; }
      end;
   end else
      result := false;
end;

function UnloadVisModule2 : integer;
var
   i : integer;

begin
   result := -1;
   if VismodCounter = 0 then
      exit;

  { if IsWindow(VisWinHandle) then
      if not VisualizationQuitted then }
         if Vismod[VismodIndex] <> nil then
            Vismod[VismodIndex]^.Quit(Vismod[VismodIndex]);

   getVisHeader2 := nil;
   if VisDLLHandle <> 0 then
   begin
      FreeLibrary(VisDLLHandle);
      VisDLLHandle := 0;
   end;

   for i := 0 to (maxVismodNum - 1) do
      Vismod[i] := nil;

   VismodCounter := 0;
   VismodIndex := -1;
   result := 0;
end;

procedure LoadVisModule2(PluginPath : string;
                         var Vismod : TVismod;
                         var NumVismod : integer;
                         ParentHandle : HWND);
var
   i : integer;
   Visheader : PWinampVisHeader;
begin
   NumVismod := 0;
   getVisHeader2 := nil;

   if VisDLLHandle <> 0 then
      UnloadVisModule2;
   VisDLLHandle := LoadLibrary(pchar(PluginPath));
   if (VisDLLHandle = 0) then
   begin
     MessageBox(MainWinHandle, 'Failed loading vis module', 'Error', MB_OK or MB_ICONERROR);
     exit;
   end;

   getVisHeader2 := GetProcAddress(VisDLLHandle, 'winampVisGetHeader');
   if @getVisHeader2 = nil then
   begin   // Unload if not a valid Vis module
      FreeLibrary(VisDLLHandle);
      VisDLLHandle := 0;
   end;

   Visheader := getVisHeader2(ParentHandle);
   if VisHeader = nil then
      exit;

   for i := 0 to (maxVismodNum - 1) do
   begin
      Vismod[i] := Visheader.getModule(i);
      if Vismod[i] <> nil then
      begin
         Vismod[i]^.hwndParent := ParentHandle;
         Vismod[i]^.hDllInstance := VisDLLHandle;
         inc(NumVismod);
      end else
         break;
   end;
   VismodIndex := -1;
end;


function StartVisModule2(ModuleNum : word) : integer;
begin
   if VismodCounter = 0 then
   begin
      result := -1;
      exit;
   end;
   if (ModuleNum > VismodCounter - 1) then
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
      if Vismod[VismodIndex] <> nil then
         Vismod[VismodIndex]^.Quit(Vismod[VismodIndex]);

   VismodIndex := ModuleNum;
   Vismod[VismodIndex]^.sRate := ChannelAttrb.SampleRate;
   Vismod[VismodIndex]^.nCh := ChannelAttrb.Channels;
   result := Vismod[VismodIndex]^.init(Vismod[VismodIndex]);
   if result <> 0 then
      MessageBox(MainWinHandle, 'Failed initializing vis module', 'Error', MB_OK or MB_ICONERROR);
end;

procedure RenderToVis;
var
   p1 : PDWORD;
   p2 : PBYTE;
   RenderResult : integer;
begin
   if EndByProgram or VisualizationQuitting then
      exit;
      
   if ModeChanging or (not GoRendering) then
      exit;

   p1 := VisDataPointer;
   inc(p1, 70{=280/4}); // Flag information is stored at byte offset 280 ~ 283.
   if p1^ = 0 then      // New data is not given ?
      exit;

   inc(p1, 1);
   Vismod[VismodIndex]^.sRate := p1^;
   inc(p1, 1);
   Vismod[VismodIndex]^.nCh := p1^;
   inc(p1, 1);
   SongPosition := p1^;
   inc(p1, 1);

   p2 := pointer(p1);

   if VisModuleInfo.spectrumNch > 0 then
   begin
      Move(p2^, Vismod[VismodIndex]^.SpectrumData[1, 1], 576);
      inc(p2, 576);

      if VisModuleInfo.spectrumNch > 1 then
      begin
        Move(p2^, Vismod[VismodIndex]^.SpectrumData[2, 1], 576);
         inc(p2, 576);
      end;
   end;

   if VisModuleInfo.waveformNch > 0 then
   begin
      Move(p2^, Vismod[VismodIndex]^.waveformData[1, 1], 576);
      inc(p2, 576);

      if VisModuleInfo.waveformNch > 1 then
         Move(p2^, Vismod[VismodIndex]^.waveformData[2, 1], 576);
   end;

   try
     RenderResult := Vismod[VismodIndex].Render(Vismod[VismodIndex]);
   except
   // Neglect some kind of exceptions
   //  on E: EMathError do RenderResult := 0;
   //  else
   //     Application.HandleException(Self);
   
     RenderResult := 1;
   end;

   if RenderResult <> 0 then
   begin
      Vismod[VismodIndex].Quit(Vismod[VismodIndex]);
      VisualizationQuitting := true;
   end;

   p1 := VisDataPointer;
   inc(p1, 70);

 //  LockFlag.Acquire; // lock out other threads
 //  try
     p1^ := 0;    // Reset flag to notify that rendering is completed.
 //  finally
 //    LockFlag.Release;
 //  end;

end;

function LookAtAllWindows(Handle: HWnd; Temp: Longint): BOOL; stdcall;
var
   ThreadId : DWORD;
   ProcId : DWORD;
begin
   result := true;

   if IsWindowVisible(Handle) then
   begin
   // get identifier of the thread which created window
      ThreadId := GetWindowThreadProcessId(Handle, @ProcId);
      if ThreadId = DriveThreadId then
      begin
         if Handle <> ParentWinHandle then
         begin
            VisWinHandle := Handle;
            result := false;
         end;
      end else
  // I have found that dwVis_hThreadId is different from the creator's for some vis plug-ins
  // Check again with identifier of the processor which created the thread.
      if ProcId = MainProcId then
      begin
         if ThreadId <> MainThreadId then
            if Handle <> ParentWinHandle then
            begin
               VisWinHandle := Handle;
               result := false;
            end;
      end;
   end;
end;

function VisDriveThread(lpParam : pointer) : DWORD; stdcall;
var
   VisModNum : integer;
   MsgReturn : longbool;
   Msg : TMsg;
   RepeatCounter : integer;
   GoFlag : integer;
   ExitLoop : boolean;
   p1 : PDWORD;
   NewEMBEDWindow : HWND;
   r1, r2 : TRect;

label ReentryPoint, ReentryPoint2;

 procedure MoveToUserEMBED(ChildWindow, ParentWindow : HWND);
 begin
    PostMessage(MsgHandle, DataReadyMsg, PauseVisOut, 0);
    GetWindowRect(VisWinHandle, r2);
    GetWindowRect(ChildWindow, r1);  // r1 gets the absolute coordinates in screen,
    r1.Left := r1.Left - r2.Left;    //  i.e., not relative coordinates in Parent Window.
    r1.Right := r1.Right - r2.Left;  //  So, we should convert it to relative coordinates
    r1.Top := r1.Top - r2.Top;       //  in Parent Window.
    r1.Bottom := r1.Bottom - r2.Top;
    ObsolitedEMBED.ChildRect := r1;

    windows.SetParent(ChildWindow, ParentWindow);
    windows.GetClientRect(ParentWindow, r1);
    MoveWindow(ChildWindow, r1.Left, r1.top, r1.Right - r1.Left, r1.Bottom - r1.top, true);
    ObsolitedEMBED.WinHandle := VisWinHandle;
    ObsolitedEMBED.EMBEDType := VisWindowIs;
    ObsolitedEMBED.WindowProc := OrgWindowProc;

    ShowWindow(VisWinHandle, SW_HIDE);
    VisWinHandle := ParentWindow;
    VisWindowIs := OnAssignedWindow;
    WasHidden := true;
    OrgWindowProc := Pointer(SetWindowLong(VisWinHandle, GWL_WNDPROC,
                                                       LongInt(@VisWindowProc)));
    ShowWindow(ParentWindow, SW_SHOW);

    with VisPluginInfo do
    begin
       VisHandle := VisWinHandle;
       VisType := VisWindowIs;
       StartType := false;
    end;

  // Inform main thread that vis module has been moved successfully.
    PostMessage(MsgHandle, DataReadyMsg, EmbedWindowChanged, DWORD(@VisPluginInfo));
 end;

 procedure MoveToPrgramEMBED(ChildWindow, ParentWindow : HWND);
 begin
    PostMessage(MsgHandle, DataReadyMsg, PauseVisOut, 0);
    windows.SetParent(ChildWindow, ParentWindow);
    ObsolitedEMBED.WinHandle := 0;
    SetWindowLong(VisWinHandle, GWL_WNDPROC, LongInt(OrgWindowProc));
    DestroyWindow(VisWinHandle);
    VisWinHandle := ParentWindow;
    VisWindowIs := ObsolitedEMBED.EMBEDType;
    OrgWindowProc := ObsolitedEMBED.WindowProc;

    WasHidden := true;
    ShowWindow(ParentWindow, SW_SHOW);
    r1 := ObsolitedEMBED.ChildRect;
    if VisWindowIs = CreatedByGPP then
       MoveWindow(ChildWindow, r1.Left, r1.Top,
                  r1.Right - r1.Left,
                  r1.Bottom - r1.Top, TRUE)
    else  //  VisWindowIs = CreatedByCode
       MoveWindow(ChildWindow, 0, 0,
                  r1.Right - r1.Left,
                  r1.Bottom - r1.Top, TRUE);

    with VisPluginInfo do
    begin
       VisHandle := VisWinHandle;
       VisType := VisWindowIs;
       StartType := false;
    end;

  // Inform main thread that vis module has been moved successfully.
    PostMessage(MsgHandle, DataReadyMsg, EmbedWindowChanged, DWORD(@VisPluginInfo));
 end;

 procedure PrepareTerminateThread;
 begin
   InThreadMsgLoop := false;
   GoRendering := false;

   VisPluginUnloading := true;
   UnloadVisModule2;
   if UseFakeWindow then
      DestroyFakeWindow;

  // We may repeatedly create EMBED Window, so leave the window class in registered state.
  // note) All window classes that an application registers are unregistered when it
  //      terminates.
  { if bEmbedClassRegistered then
   begin
      windows.UnRegisterClass(CLASSNAME_EMBED, g_hInstance);
      bEmbedClassRegistered := false;
   end; }
   WindowEmbed := 0;
 //  VisWinHandle := 0;  // Execute after ending VisDriveThread (by advice of BassFan)
   VisChildHandle := 0;

 // Relaese subclassing
   if not UseFakeWindow then
      SetWindowLong(MainWinHandle, GWL_WNDPROC, LongInt(MainWindowProc));

   if not EndByProgram then
   begin
      CloseHandle(ThreadHandle);
      ThreadHandle := 0;
      PostMessage(MsgHandle, DataReadyMsg, EndVisOut, 0);
   end;

   DriveThreadId := 0;
 end;

begin
   GoRendering := false;
   EndByProgram := false;
   EndBySysMenu := false;
   InThreadMsgLoop := false;

   g_hInstance := GetModuleHandle(nil); // get the application instance

   if UseFakeWindow then
   begin
     FakeWinHandle := CreateFakeWindow;

     if (FakeWinHandle = 0) then  // if failed to create a fake window(= window to emulate Winamp)
     begin
     // Inform main thread that vis-drive-thread failed to create a fake window.
       PostMessage(MsgHandle, DataReadyMsg, StartVisOut, 0);
       DriveThreadId := 0;
       ExitThread(0);
     end else
       ParentWinHandle := FakeWinHandle;
   end else
      ParentWinHandle := MainWinHandle;

   VisModNum := integer(lpParam);
   SetThreadPriority(GetCurrentThread, VisPriority);

   Reentering := false;
   VisPluginUnloading := false;
   VisualizationQuitting := false;
   VisualizationPaused := false;
   ModeChanging := false;
 //  SavedWindowRect := false;
   FirstShown := true;
   WasHidden := false;

   VisWinHandle := 0;
   VisChildHandle := 0;
   ObsolitedEMBED.WinHandle := 0;
   WindowEmbed := 0;
   VisWindowIs := CreatedByPlugin;  // put default value. may be altered later
   NowUnloadingPlugin := false;

 // Subclass main window's message handler to handle the messages from vis plug-in.
   if not UseFakeWindow then
      MainWindowProc := Pointer(SetWindowLong(MainWinHandle, GWL_WNDPROC,
                                                       LongInt(@FakeWinampProc)));

   LoadVisModule2(Vis_Plugin, Vismod, VismodCounter, ParentWinHandle);
   if ((VismodCounter > 0) and (VisModNum >= 0) and (VisModNum < VismodCounter)) then
   begin
     VisModuleInfo.delayMs := Vismod[VisModNum]^.delayMs;
     VisModuleInfo.latencyMs := Vismod[VisModNum]^.latencyMs;
     VisModuleInfo.spectrumNch := Vismod[VisModNum]^.spectrumNch;
     VisModuleInfo.waveformNch := Vismod[VisModNum]^.waveformNch;
     PostMessage(MsgHandle, DataReadyMsg, VisModuleLoaded, DWORD(@VisModuleInfo));
   end else  // failed to load vis plug-in
   begin
     PostMessage(MsgHandle, DataReadyMsg, StartVisOut, 0);
     PrepareTerminateThread;
     ExitThread(0);
   end;

// ( For MilkDrop )
// There is a case that I cannot detect the occurrence that vis plug-in is changing its
// display mode, before it closes previously created vis window.
// So, I should use extra check code and simple "goto" command in preparation for the case.
// It's a very unskilled way, I know.

 ReentryPoint:

     StartFromReentry := true;   // for start in Desktop mode
     PostPending := false;
     FirstShown := true;
     WasHidden := false;

     if Reentering then          // vis plug-in already started ?
        GoFlag := 0
     else
        GoFlag := StartVisModule2(VisModNum);   // start vis plug-in (returns 0 if no error)

     if GoFlag = 0 then          // vis plug-in started ?
     begin
        RepeatCounter := 0;

        if not Reentering then    // Reentering = A vis window has been already detected.
        repeat                    // Wait until vis window is created
           if WindowEmbed <> 0 then
              VisWinHandle := WindowEmbed
           else
              EnumWindows(@LookAtAllWindows, 0);
       // < For Milkdrop 2.0d >
       // If we set Milkdrop 2.0d to start in Desktop mode, vis window is not detected with EnumWindows.
       // Milkdrop gives us the chance to detect vis window in the message handling routine for Winamp
       // IPC messages from vis plug-in (LParam = IPC_SETVISWND).

           sleep(50);
           WinProcessMessages;
           inc(RepeatCounter);
        until (VisWinHandle <> 0) or (RepeatCounter = 100);

        if VisWinHandle <> 0 then    // vis window detected ?
        begin
           if PostPending then       // PostPending is set true, if VisWindowIs = CreatedByCode,
              PostPending := false;  // to post WM_SHOWWINDOW message after detecting vis window.

           if (VisWindowIs = CreatedByPlugin) then
            // Subclass to handle the messages to be sent to vis plug-in.
              OrgWindowProc := Pointer(SetWindowLong(VisWinHandle, GWL_WNDPROC,
                                                        LongInt(@VisWindowProc)));

       // Post WM_SHOWWINDOW Message to enforce on vis window to adjust it's size and/or position.
           if FirstShown then  // Some plug-ins post WM_SHOWWINDOW message at creation of vis window
            //  PostMessage(VisWinHandle, WM_SHOWWINDOW, 1, 0);
              SendMessage(VisWinHandle, WM_SHOWWINDOW, 1, 0); 
        end;

        StartFromReentry := false;
        VisualizationPaused := false;

        if (VisWinHandle <> 0) then   // if vis window is created.
        begin
           InThreadMsgLoop := true;
         //  Reentering := false;  // This line executed prior to receiving WM_SHOWWINDOW
                                  // message. So put this line in the WM_SHOWWINDOW message
                                  // handling routine
           ExitLoop := false;

        ReentryPoint2:
           repeat
             MsgReturn := GetMessage(Msg, 0, 0, 0);
             if ((Msg.message = WM_QUIT) or (Msg.message = WM_CLOSE)) then
             begin
                if (not NowUnloadingPlugin) then
                   ExitLoop := true
             end else if Msg.message = DataReadyMsg then
             begin
               case Msg.wParam of
                 DataReady : RenderToVis;
                 MinimizeWindow : HideVisWindow;
                 RestoreWindow : ShowVisWindow;
                 InformPlayerMode..InformStreamInfo : SetStatus(Msg.wParam);
                 RequestRestFlag : begin  // * Added at Ver 1.2
                                      p1 := VisDataPointer;
                                      inc(p1, 70);
                                   //   LockFlag.Acquire; // lock out other threads
                                   //   try
                                        p1^ := 0;    // Reset flag (= rendering is completed)
                                   //   finally
                                   //     LockFlag.Release;
                                   //   end;
                                   end;
                 UnloadVisPlugin : begin  // * Added at Ver 1.2
                                     if VisWindowIs <> CreatedByCode then
                                     // Relaese subclassing
                                        SetWindowLong(VisWinHandle, GWL_WNDPROC, LongInt(OrgWindowProc));
                                     UnloadVisModule2;
                                     if (VisWindowIs = CreatedByGPP) then
                                        SetGPPInactive;
                                   end;

                 ChangeEmbedWindow : begin   // * Added at Ver 1.2
                                       UserEMBED := Msg.lParam;

                                       if EMBEDSwitchMode = WindowMove then
                                       begin
                                         if (VisWindowIs <> Unavailable) and (VisWindowIs <> CreatedByPlugin) then
                                         begin
                                           if UserEMBED <> 0 then
                                           begin
                                        // if (VisWindowIs <> OnAssignedWindow) then
                                             if ((VisWindowIs = CreatedByGPP) or (VisWindowIs = CreatedByCode)) then
                                             begin
                                               NewEMBEDWindow := GetWindow(UserEMBED, GW_CHILD);
                                               if NewEMBEDWindow = 0 then
                                                  NewEMBEDWindow := CreateChildWindow(UserEMBED);
                                               if NewEMBEDWindow <> 0 then
                                                 MoveToUserEMBED(VisChildHandle, NewEMBEDWindow);
                                             end;
                                           end else
                                           begin   // for UserEMBED = 0
                                             if ObsolitedEMBED.WinHandle <> 0 then
                                               MoveToPrgramEMBED(VisChildHandle, ObsolitedEMBED.WinHandle);

                                           end;
                                         end;
                                       end;
                                     end;

                 ChangeEMBEDSwitchMode : if Msg.lParam = ord(NewStart) then  // * Added at Ver 1.2
                                            EMBEDSwitchMode := NewStart
                                         else if Msg.lParam = ord(WindowMove) then
                                            EMBEDSwitchMode := WindowMove;

                 UseGPPVisDrawer : if Msg.lParam = 1 then
                                       UseGPPModule := true
                                    else
                                       UseGPPModule := false;
               end;

               Continue;
             end;

             TranslateMessage(Msg);
             DispatchMessage(Msg);
           until (integer(MsgReturn) <= 0) or ExitLoop;

           if NowUnloadingPlugin then
              goto ReentryPoint2;

        end else // vis window is not created
           PostMessage(MsgHandle, DataReadyMsg, StartVisOut, 0);
     end else    // Failed to launch vis module
        PostMessage(MsgHandle, DataReadyMsg, StartVisOut, 0);

 // Following condition is met at changing display mode from windowed mode to desktop mode.
 // ( and the vis window type at windowed mode is not CreatedByPlugin )
 // In this case we should revert program flow to ReentryPoint.
   if (VisWindowIs = CreatedByPlugin) and VisualizationPaused and (not EndByProgram) then
      Reentering := true;

   if Reentering then
      goto ReentryPoint;

   Result := 0;   // * Added (by advice of BassFan)
   PrepareTerminateThread;
   ExitThread(0);
end;


function Start_Vis(VisPlugin : string;
                    ModuleNum : integer;
                    EmbedHandle : HWND;
                 //   SyncMain : boolean;
                    UseGenVisDrawer : boolean;
                    EMBEDSwitch : TVisEMBEDSwitchMode;
                    PlayerModeId : integer; ChannelIs : TChannelAttrb) : integer;
var
   WaitCycle : integer;
begin
   result := 0;

 // The function RunVisPlugin in TBASSPlayer.pas calls its QuitVisPlugin function (which calls
 // Stop_Vis procedure in this Unit) first, if a vis plug-in is running.
   if DriveThreadId <> 0 then   // Check if previously created VisDriveThread is terminated.
   begin
      WaitCycle := 0;

      repeat                // Wait until previously created VisDriveThread is terminated.
         Sleep(30);
         WinProcessMessages;
         inc(WaitCycle);
      until (DriveThreadId = 0) or (WaitCycle = 100);

      if DriveThreadId <> 0 then
      begin
      // This case should not happen, MessageBox code is placed for debugging purpose
         Application.MessageBox('Error : DriveThreadId non-zero', 'Error', MB_OK+MB_ICONERROR);
         exit;
      end;
   end;

   if (VisPlugin <> '') and (ModuleNum >= 0) then
   begin
      Vis_Plugin := VisPlugin;
      VismodNo   := ModuleNum;

      if EmbedHandle <> 0 then
         if IsWindow(EmbedHandle) then
            UserEMBED := EmbedHandle
         else
            UserEMBED := 0
      else
         UserEMBED := 0;

      if (GetGPPDLLHandle <> 0) and UseGenVisDrawer then
         UseGPPModule := true
      else
         UseGPPModule := false;

      EMBEDSwitchMode := EMBEDSwitch;

      MainThreadId := GetWindowThreadProcessId(MainWinHandle, @MainProcId);

    //  SyncWithMain := SyncMain;
      PlayerMode := TPlayerMode(PlayerModeId);
      ChannelAttrb := ChannelIs;

   // "BeginThread" invokes system error when floating point exception is occured by vis module
   //   ThreadHandle := BeginThread(nil, 0, @VisDriveThread, pointer(ModuleNum), 0, VisDriveThreadId);
      ThreadHandle := CreateThread(nil, 0, @VisDriveThread, pointer(VismodNo), 0, DriveThreadId);
     { if ThreadHandle <> 0 then
         CloseHandle(ThreadHandle)  // if we do not use ThreadHandle elsewhere, we can close it.
      else }
      if ThreadHandle <> 0 then
         result := ThreadHandle
      else
         MessageBox(MainWinHandle, 'Error : Unable to create thread !', 'Error!', MB_OK or MB_ICONERROR);
   end;
end;

procedure SetBasicParams(HandleMainWin,
                         MessageHandle,
                         StatusMsg : HWND;
                         ShareMemPointer : pointer;
                         ThreadPriority : integer;
                         LockFlag_ : TCriticalSection);
begin
   MainWinHandle := HandleMainWin;
   MsgHandle := MessageHandle;
   DataReadyMsg := StatusMsg;
   VisDataPointer := ShareMemPointer;
   VisPriority := ThreadPriority;
   LockFlag := LockFlag_;
end;

procedure Stop_Vis;
var
   WaitCycle : integer;
begin
   EndByProgram := true;
   GoRendering := false;
   RestoreParent;
   
   if (DriveThreadId <> 0) then
   begin
   // if vis windows is active then force on VisDriveThread to unload vis plug-in.
   // note) We should unload vis plug-in prior to posting WM_QUIT message to VisDriveThread,
   //       else the WaitForSingleObject API call does not operate normally.

      NowUnloadingPlugin := true;
      if VisWinHandle <> 0 then
         PostThreadMessage(DriveThreadId, DataReadyMsg, UnloadVisPlugin, 0);

   // Wait until VisDriveThread finish unloading vis plug-in
      WaitCycle := 0;
      if VismodCounter <> 0 then
         repeat
            Sleep(50);
            WinProcessMessages;
            inc(WaitCycle);
         until (VismodCounter = 0) or (WaitCycle = 100);

      if VismodCounter <> 0 then
         Application.MessageBox('Error : VismodCounter non-zero', 'Error', MB_OK+MB_ICONERROR);

      NowUnloadingPlugin := false;
   // Force on VisDriveThread to terminate itself
      PostThreadMessage(DriveThreadId, WM_QUIT, 0, 0);

   // Wait until VisDriveThread terminated
      WaitForSingleObject(ThreadHandle, INFINITE);

      if DriveThreadId <> 0 then
         Application.MessageBox('Error : DriveThreadId non-zero', 'Error', MB_OK+MB_ICONERROR);

   end;

   VisWinHandle := 0;
   CloseHandle(ThreadHandle);
   ThreadHandle := 0;

 //  Inform main thread that visualization process terminated
 //  PostMessage(MsgHandle, DataReadyMsg, EndVisOut, 0);
   SendMessage(MsgHandle, DataReadyMsg, EndVisOut, 0);
end;

// Nearly same as Stop_Vis. Why is this needed ?
// I have gotton an error with Stop_vis, if I run demo program as follows.
//  - Run demo program then play a song.
//  - Run vis plug-in.
//  - (while a song is being played and vis plug-in is running) log off or shut down system.
// So I have decided to use Stop_Vis2 instead of Stop_Vis when an instance of TBASSPlayer
//  is destroyed.
procedure Stop_Vis2;
var
   WaitCycle : integer;
begin
   EndByProgram := true;
   GoRendering := false;
   RestoreParent;

   if (DriveThreadId <> 0) then
   begin
   // if vis windows is active then force on Vis Driver Thread to unload vis plug-in.
      NowUnloadingPlugin := true;
      if VisWinHandle <> 0 then
         PostThreadMessage(DriveThreadId, DataReadyMsg, UnloadVisPlugin, 0);

      WaitCycle := 0;
      if VismodCounter <> 0 then
         repeat
            Sleep(30);
            WinProcessMessages;
            inc(WaitCycle);
         until (VismodCounter = 0) or (WaitCycle = 100);
   end;

  // WM_QUIT message is posted by main thread at terminating program
  { if (DriveThreadId <> 0) then
      PostThreadMessage(DriveThreadId, WM_QUIT, 0, 0); }

   CloseHandle(ThreadHandle);
end;

procedure HideVisWindow;
begin
 {  CloseWindow(VisWinHandle);  }
   ShowWindow(VisWinHandle, SW_HIDE);
end;

procedure ShowVisWindow;
begin
  { if IsIconic(VisWinHandle) then
      OpenIcon(VisWinHandle);  }
   ShowWindow(VisWinHandle, SW_SHOW);
end;


procedure SetStatus(StatusId : DWORD);
var
   p1 : PDWORD;
   p2 : PBYTE;
   ModeNum : DWORD;
   TitleBuf : array[0..255] of char;
begin
   p1 := VisDataPointer;

   if StatusId = InformPlayerMode then
   begin
      ModeNum := p1^;
      PlayerMode := TPlayerMode(ModeNum);
   end else if StatusId = InformStreamInfo then
   begin
      inc(p1, 1);
      ChannelAttrb.SampleRate := p1^;
      inc(p1, 1);
      ChannelAttrb.BitRate := p1^;
      inc(p1, 1);
      ChannelAttrb.Channels := p1^;
      inc(p1, 1);
      ChannelAttrb.Duration := p1^;
      inc(p1, 1);
      p2 := pointer(p1);
      move(p2^, TitleBuf, 256);
      ChannelAttrb.Title := StrPas(TitleBuf);
    //--- * Added at Ver 1.2 --------------
      p2 := VisDataPointer;
      inc(p2, 2600);
      move(p2^, TitleBuf, 256);
      ChannelAttrb.FilePath := StrPas(TitleBuf);
    //-------------------------------------------
  { end else if StatusId = InformSyncWindows then
   begin
      inc(p1, 69);
      if p1^ = 0 then
         SyncWithMain := false
      else
         SyncWithMain := true;
   end; }
   end;
end;


end.
