{************************************************}
{   Simple SA2 Player.                           }
{   Copyright (c) 1995, Carl Eric Codere         }
{   Montreal, Canada - December  1995.           }
{   VERSION 1.50                                 }
{************************************************}
{ See license.txt for usage of this source code }

{ DEFINE DEBUG if you wish to trace the program. It will use crt.delay    }
{ instead of the interrupt routine, so no crashing while tracing!         }


{ All information on the SA2 file format was taken from the SADT2 document }
{ which was created by Erik Pojar! . The PIT Interrupt was also taken from }
{ Mark Feldman and his PCGPE!!                                             }


{ +$   12/15/95 Bugfix of notes!!! Notes in this format are based on the  }
{               adlib card and not the midi format. Therefore, I had      }
{               forgotten to add twelve to each note!!                    }
{ $    (12/22/95)                                                         }
{              Songs now restart at correct song position.                }
{              Added Pattern_break affect                                 }
{ $    (12/28/95)                                                         }
{              Songs now use the correct pattern order. They start and    }
{              finish at the correct position.                            }
{              TESTED IN PROTECTED AND REAL MODES                         }
{ $    (04/25/96)                                                         }
{              Restart position should now work correctly, forgot to re-  }
{              init patternnumber to 1 at restart.                        }
Uses Objects, Sound,CRT,DOS;

CONST
 MAXINSTR           = 31; { Maximum number of Instruments }
 SA2VERSION         = 9;  { Version of song supported     }
 sa2InvalidFile     = 1;  { Invalid File format }
 Sa2InvalidVersion  = 2;  { Invalid Song Version }


 Version: String = '1.50';
 { Effect constants }
 POSITION_JUMP = $0B;
 PATTERN_BREAK = $0D;
 SET_VOLUME    = $0C;
 RELEASE_NOTE  = $08;

(* Offset of modulator for each voice *)
TableModulator:Array[0..13] of byte = (
      $00,$01,$02,$08,$09,$0a,
      $10,$11,$12,$01,$11,$4f,
      $00,$0f1);


TYPE
  TChannels = Array[1..16] of Byte;

  PSong = ^Tsong;
  TSong = Array[1..60000] of Byte;

  { One single Track - from byte position 0 to 63 }
  TTrack = Array[1..64,1..3] of Byte;
  TTrackTable = Array[1..255] of TTrack;
  PTrackTable = ^TTrackTable;

  TPatternAndTrack = Array[1..MAXMELODICCHANNELS] of Byte;

  TSa2Instrument = Record
   FeedBackConnection: Byte;
   ModulatorChar: Byte;
   CarrierChar: Byte;
   ModAttackDelay: Byte;
   CarAttackDelay: Byte;
   ModSustainRelease: Byte;
   CarSustainRelease: Byte;
   ModWaveSelect: Byte;
   CarWaveSelect: Byte;
   ModScalingOutput: Byte;
   CarScalingOutput: Byte;
   ArpeggioStart: Byte;
   ArpeggioSpeed: Byte;
   CurrentPos: Byte;
   SpeedCount: Byte;
  end;




  TSa2Header = Record
   Id: Array[1..4] of Char;
   Version: Byte;
   Instruments: Array[1..MAXINSTR] of TSA2Instrument;
   InsNames: Array[1..MAXINSTR] of  Array[0..15] of Char;
   PatternOrder: Array[1..128] of Byte;
   Patterns: Word;
   SongLength: Byte;
   Restart: Byte;
   Tempo: Word;
   ArpeggioLost: Array[1..256] of Byte;
   ArpeggioCommands: Array[1..256] of Byte;
   TrackOrder: Array[1..64] of TPatternAndTrack;
   Channels: Word;
  end;


  PSA2Player = ^TSA2Player;
  TSA2PLayer = Object(TObject)
   Delay: Word;       { Current Delay Count      }
   Song: PSong;       { Pointer to the Song data }
   CurrentPos: Word;  { Current Position in song }
   Adlib: TAdlib;
   Playing: Boolean;  { TRUE if a song is playing }
   CopiedValue: Byte;
   Header: TSA2Header;
   Stream: TBufStream;
   InstrTable: Array[0..255] of TSa2Instrument;
   Channels: TChannels;
   TotalTracks: Word;
   TrackTable: PTrackTable;
   PatternNumber: Word;   { Current playing pattern }
   LineNo: Word;
   CurVoice: Byte;
   CurrentEffect: Byte;
   Parameter: Byte; { Current Effect parameter }
   Count: Byte;     { Current Pattern to play  }
   { Call this to play file -> AfileName: CMF File name to play }
   Constructor Init(AFileName: FNameStr);
   { Verifies if song is correct format  }
   Function VerifySong: Boolean;
   { Sets a voice to a particular instrument }
   Procedure SetVoice(Voice:Byte; Instr: TSA2Instrument);
   { Processes the Musical events }
   Procedure ProcessEvent;
   { Called when there is an invalid file format }
   Procedure Error(Value: Integer); Virtual;
   { Copy instruments into instrument table }
   Procedure CopyInstruments;
   { CALL IT AT THE END!! Remove Interrupt Handler }
   Destructor Done; Virtual;
   { Calculates de Delta-Time Delays }
 { Returns the total number of tracks to load from disk }
    Function GetTotalDifferentTracks: Integer;
    Procedure ProcessEffect(Effect: Byte; AParameter: Byte; Voice: Byte);
    Procedure ProcessLine(TracksToPlay: TPatternAndTrack; I: Word; J: Byte);
    Procedure TheEnd; Virtual; { Called when the song is finished }
 private
  ModulatorOutputLevel: Array[0..10] of Byte; { Used for volume effects }
  CarrierOutputLevel: Array[0..10] of Byte;   { Used for volume effects }
  ErrorValue: Integer;
  { Change both Connector and Carrier Volumes          }
  { 0-127 values are possible for volume, 127 is highest }
  Procedure SetBothVolume(Voice: Byte; AVolume: Byte);
end;

   { Used to find the number of tracks to copy into table }
   PNumberedCollection = ^TNumberedCollection;
   TNumberedCollection = Object(TSortedCollection)
      Function Compare(Key1, Key2: Pointer): Integer; Virtual;
      Procedure FreeItem(Item: Pointer); Virtual;
   end;

PByte = ^Byte;


Procedure TNumberedCollection.FreeItem(Item: Pointer);
Begin
 If Item <> Nil then Dispose(Item);
end;



Function TNumberedCollection.Compare(Key1, Key2: Pointer): Integer;
Begin
  If PByte(Key1)^  = PByte(Key2)^ then
   Compare := 0;
  If PByte(Key1)^ < PByte(Key2)^  then
   Compare := 1
  else
   Compare := -1;
end;

(*************************************************************************)
(*                           MAIN ROUTINES                               *)
(*************************************************************************)

const TIMERINTR = 8;
       PIT_FREQ = $1234DD;

var BIOSTimerHandler : procedure;
    clock_ticks, counter : longint;
    { CMF Object Instance }
    SA2Player: TSA2Player;



procedure SetTimer(TimerHandler : pointer; frequency : word);
begin

  { Do some initialization }
  clock_ticks := 0;
  counter := $1234DD div frequency;

  { Store the current BIOS handler and set up our own }
  GetIntVec(TIMERINTR, @BIOSTimerHandler);
  SetIntVec(TIMERINTR, TimerHandler);

  { Set the PIT channel 0 frequency }
  Port[$43] := $34;
  Port[$40] := Byte(counter mod 256);
  Port[$40] := Byte(counter div 256);
end;


procedure CleanUpTimer;
begin
  { Restore the normal clock frequency }
  Port[$43] := $36;
  Port[$40] := 0;
  Port[$40] := 0;
  { Restore the normal ticker handler }
  SetIntVec(TIMERINTR, @BIOSTimerHandler);
end;


procedure Handler; Interrupt;
begin

  { DO WHATEVER WE WANT TO DO IN HERE }
   SA2Player.ProcessEvent;
  { Adjust the count of clock ticks }
  clock_ticks := clock_ticks + counter;

  { Is it time for the BIOS handler to do it's thang? }
  if clock_ticks >= $10000 then
    begin

      { Yep! So adjust the count and call the BIOS handler }
      clock_ticks := clock_ticks - $10000;
      asm pushf end;
      BIOSTimerHandler;
    end

  { If not then just acknowledge the interrupt }
  else
    Port[$20] := $20;
end;

(*************************************************************************)
(* Create a stream error procedure which will be called on error of the  *)
(* stream. Will Terminate executing program, as well as display info     *)
(* on the type of error encountered.                                     *)
(*************************************************************************)
Procedure StreamErrorProcedure(Var S: TStream); FAR;
Begin
 If S.Status = StError then
 Begin
  WriteLn('ERROR: General Access failure. Halting');
  Halt(1);
 end;
 If S.Status = StInitError then
 Begin
  Write('ERROR: Cannot Init Stream. Halting. ');
  { SPECIFIC TO DOS STREAMS }
  Case S.ErrorInfo of
  2: WriteLn('File not found.');
  3: WriteLn('Path not found.');
  5: Writeln('Access denied.');
  else
    WriteLn;
  end;
  Halt(1);
 end;
 If S.Status = StReadError then
 Begin
  WriteLn('ERROR: Read beyond end of Stream. Halting');
  Halt(1);
 end;
 If S.Status = StWriteError then
 Begin
  WriteLn('ERROR: Cannot expand Stream. Halting');
  Halt(1);
 end;
 If S.Status = StGetError then
 Begin
  WriteLn('ERROR: Get of Unregistered type. Halting');
  Halt(1);
 end;
 If S.Status = StPutError then
 Begin
  WriteLn('ERROR: Put of Unregistered type. Halting');
  Halt(1);
 end;
end;


Procedure TSA2Player.SetBothVolume(Voice: Byte; AVolume: Byte);
(*************************************************************************)
(*  This procedure directly changes the Output level for both            *)
(*  the connector and the Modulator.                                     *)
(*                                                                       *)
(*  THIS IS NOTE A REAL VOLUME CHANGE, BUT MORE OF A SOUND CHANGE!       *)
(*  SINCE BOTH OPERATORS ARE CHANGED.                                    *)
(*                                                                       *)
(*  Avolume -> VALUES FROM 0 to 127 possible. 127 is highest.            *)
(*************************************************************************)
VAR
 Vol: Byte;
Begin
 { We need a value between 0 and 63, so divide by two }
 Vol := AVolume SHR 1;
 ModulatorOutputLevel[Voice] := Vol AND 63;
 CarrierOutputLevel[Voice] := Vol AND 63;
 FastOutput($40 + TableModulator[Voice], BYTE((63-ModulatorOutputLevel[Voice])
   OR Adlib.ModulatorScalingLevel[Voice]));
 FastOutput($43 + Tablemodulator[Voice], BYTE((63-CarrierOutputLevel[Voice]) OR Adlib.CarrierScalingLevel[Voice]));
end;


Procedure TSa2Player.TheEnd;
Begin
 WriteLn('THE END.');
end;

Destructor TSa2Player.Done;
Begin
{$IFNDEF DEBUG}
 CleanUpTimer;
{$ENDIF}
 Adlib.Done;
 If Song <> Nil then Dispose(Song);
end;




Procedure TSa2Player.ProcessEffect(Effect: Byte; AParameter: Byte; Voice: Byte);
(*********************************************************************)
(*  Process the different effects for the tracks.                    *)
(*********************************************************************)
Var
 Temp :BYTE;
Begin
 { If no effect then Exit procedure }
 If Effect = 0 then Exit;
 { If the effect is to change the volume }
 If Effect = SET_VOLUME then
 Begin
 { If volume higher then 63, then max volume = 63 }
 If AParameter > $40 then AParameter := $40;
  { We need a value between 0 and 127 }
  { therefore multiply by 2           }
  Temp := AParameter SHL 1;
  { Set both operator volumes }
  SetBothVolume(Voice, Temp);
 end;
  { Close note }
 If Effect = RELEASE_NOTE then
 Begin
  Adlib.NoteOff(Voice, AParameter);
 end;
end;

Procedure TSa2Player.ProcessLine(TracksToPlay: TPatternAndTrack; I: Word; J: Byte);
Var
 Note: Byte;
 Instrument: Byte;
 Temp: Word;
 k: Integer;
Begin
  { SET THE INSTRUMENT }
  If TracksToPlay[J] <> 0 then
  Begin
    Parameter := TrackTable^[TracksToPlay[J]][i][3];
    Move(TrackTable^[TracksToPlay[J]][i], Temp, Sizeof(Word));
    Note := TrackTable^[TracksToPlay[J]][i][1] SHR 1;

    Temp := Swap(Temp);
    Instrument := Byte((Temp SHR 4) AND $1F);
    { Check if there is an instrument, and if so check not to replace }
    { the same old instrument.                                        }
    If (Instrument <> 0) then
    Begin
      SetVoice(J-1, InstrTable[Instrument]);
    end;
    CurrentEffect := Byte(Temp AND $0F);
    If CurrentEffect <> 0 then ProcessEffect(CurrentEffect, Parameter, J-1);

  { SET THE NOTE AND DO A NOTE ON }
    If Note <> 0 then
    Begin
      Note:=Note+12;
      Adlib.NoteOn(J-1, Note);
    end;
  end
  else
  Begin
    Note := 0;
    Temp := 0;
  end;
end;


Procedure TSa2Player.ProcessEvent;
Var
 TracksToPlay: TPatternAndTrack;
 i: Byte;
Begin
 { If the effect is a pattern_break }
 { we handle it by changing to the  }
 { next pattern immediately.        }
 If (CurrentEffect = PATTERN_BREAK) then
 Begin
  { Parameter = Parameter of effect }
  LineNo := Parameter+1;
  Parameter := 0;
  CurrentEffect := 0;
  Inc(PatternNumber);
  Count := Header.PatternOrder[PatternNumber]+1;
  CurVoice := 1;
 end;
 { Get the next track to play from the trackorder list }
 TracksToPlay := Header.TrackOrder[Count];
 { Processes a complete pattern line }
 { Tracks to play is an array for each channel indicating which }
 { pattern to play for that track                               }
 ProcessLine(TracksToPlay, LineNo,CurVoice);
 { Get to the next voice in this pattern }
 Inc(CurVoice);
 If CurVoice > 9 then
 Begin
  CurVoice := 1;
  Inc(LineNo);
 end;
 { If we have arrived at the end of the pattern, then change to next }
 { pattern.                                                          }
 If LineNo > 64 then
 Begin
  LineNo:=1;
  WriteLn(PatternNumber);
  Inc(PatternNumber);
  Count := Header.PatternOrder[PatternNumber]+1;
  CurVoice := 1;
 end;
 { If we have arrived at the end of the song }
 { Start everything over                     }
 If PatternNumber > Header.SongLength then
 { End of song start over. }
 Begin
  TheEnd;
  For i:=1 to MAXMELODICCHANNELS do
   Adlib.MuteVoice(i-1);
  PatternNumber:=1;
  Count:= Header.Restart+1;
  CurVoice := 1;
  LineNo := 1;
 end;
end;


Function TSa2Player.GetTotalDifferentTracks: Integer;
(*************************************************************************)
(*  This Function returns the number of DIFFERENT tracks to load         *)
(*  from disk.(these are sorted at run-time)                             *)
(*************************************************************************)
Var
 Collection: PNumberedCollection;
 I,J: Byte;
 Value: PByte;
Begin
 Collection := New(PNumberedCollection, Init(35, 10));
 For i:=1 to Header.Patterns do
 Begin
  For J:=1 to MAXMELODICCHANNELS do
    Collection^.Insert(@Header.TrackOrder[i][j]);
 end;
 Value := Collection^.At(0);
 Dispose(Collection, Done);
 GetTotalDifferentTracks := Integer(Value^);
end;


Constructor TSa2Player.Init(AFileName: FNameStr);
Var
 i: Byte;
Const
 HexTable: Array[0..15] of Word =
 ($8000, $4000, $2000, $1000, $800, $400, $200, $100, $80, $40, $20, $10, $08, $04, $02, $01);
Begin
 Inherited Init;
 Count := 1;
 CurVoice := 1;
 LineNo := 1;
 Counter:=1;
 PatternNumber := 1;
 Stream.Init(AFileName, StOpenRead, 1024);
 Stream.Read(Header, SizeOf(Header));
 If (NOT VerifySong) then
    Error(ErrorValue);
 { GET IN USE CHANNELS TABLE }
 { Verify which channels will be used }
 { for the moment, it seems that all  }
 { bit values are always set to one   }
 For i:=0 to 15 do
 Begin
  If (Header.Channels AND HexTable[i]  <> $00) then Channels[i+1] := Byte(TRUE);
 end;
 CopyInstruments;
 { Make a pointer to the song }
 New(Song);
 { Make a pointer to the tracktable }
 New(TrackTable);
 i:=1;
 { Get the total number of tracks (NOT) patterns }
 TotalTracks := GetTotalDifferentTracks;
 Repeat
   { Read in all the diffrent tracks into the tracktable }
   Stream.Read(TrackTable^[i], Sizeof(TTrack));
   inc(i);
 Until i = TotalTracks;
 Adlib.Init;
 Stream.Done;
{$IFNDEF DEBUG}
 { The code is slow so adjust song speed accordingly         }
 { if everything was in ASM we would not have this problem ! }
 SetTimer(Addr(Handler), 80);
{$ENDIF}
end;


Function TSa2Player.VerifySong: Boolean;
Begin
 VerifySong := TRUE;
 If Header.ID <> 'SAdT' then
 Begin
  VerifySong := FALSE;
  ErrorValue := Sa2InvalidFile;
 end;
 If Header.Version <> SA2VERSION then
 Begin
  VerifySong := FALSE;
  ErrorValue := Sa2InvalidVersion;
 end;
end;


Procedure TSa2Player.CopyInstruments;
(*********************************************************************)
(* Copy each of the instruments in the Header into a local instrument*)
(* table.                                                            *)
(*********************************************************************)
Var
 i: Byte;
Begin
 For i:=1 to MAXINSTR do
 Begin
  InstrTable[i] := Header.Instruments[i]
 end;
end;


Procedure TSa2Player.Error(Value: Integer);
(*********************************************************************)
(*  Takes care of halting the program and writing to different       *)
(*  error values found.                                              *)
(*********************************************************************)
Begin
 Case ErrorValue of
 Sa2InvalidFile: WriteLn('ERROR! Invalid File Format');
 Sa2InvalidVersion:
 Begin
    WriteLn('ERROR! Invalid Song Version - Only plays version ',VERSION,' songs.');
    WriteLn(' Go back in your tracker load the file and save it again!');
 end;
 else
  WriteLn('ERROR!');
 end;
 Halt(1);
end;


Procedure TSa2Player.SetVoice(Voice:Byte; Instr: TSA2Instrument);
(*************************************************************************)
(*  TSa2Player.SetVoice(Voice: Byte; Instr: TCMFInstrument)              *)
(*   -> Sets the Instruments passed as a parameter to the specified      *)
(*   music card voice.                                                   *)
(*                                                                       *)
(*************************************************************************)
Begin
  If Adlib.PlayingNote[Voice] <> 0 then Adlib.NoteOff(Voice, 0);
  Adlib.SetWaveSelect(Voice, Modulator, Instr.ModWaveSelect);
  Adlib.SetWaveSelect(Voice, Carrier, Instr.CarWaveSelect);
  Adlib.SetWaveChar(Voice, Modulator, Instr.ModulatorChar);
  Adlib.SetWaveChar(Voice, Carrier, Instr.CarrierChar);
  { Save the scaling output for the second operator }
  { this is the type which controls the volume of   }
  { the voice.                                      }
  Adlib.CarrierScalingLevel[Voice] := Instr.CarScalingOutput AND $C0;
  Adlib.ModulatorScalingLevel[Voice] := Instr.ModScalingOutput AND $C0;
  Adlib.Algorithm[Voice] := Boolean(Instr.FeedBackConnection AND $01);
  Adlib.Volume[Voice] := Instr.CarScalingOutput AND $3f;
  Adlib.Volume[Voice] := Byte(Adlib.Volume[Voice] - $3F);
  Adlib.Volume[Voice] := Byte(0 - Adlib.Volume[Voice]);
  Adlib.AllScalingOutput(Voice, Modulator, Instr.ModScalingoutput);
  Adlib.AllScalingOutput(Voice, Carrier, Instr.CarScalingOutput);
  Adlib.AllAttackDecay(Voice, Modulator, Instr.ModAttackDelay);
  Adlib.AllAttackDecay(Voice, Carrier, Instr.CarAttackDelay);
  Adlib.AllFeedBack(Voice, Instr.FeedBackConnection);
  Adlib.AllSusRelease(Voice, Modulator, Instr.ModSustainRelease);
  Adlib.AllSusRelease(Voice, Carrier,   Instr.CarSustainRelease);
end;




Var
 i: Byte;
Begin
 StreamError := @StreamErrorProcedure;
  { LITTLE TITLE }
   WriteLn('Simple SA2 Player Version '+VERSION+' by Carl Eric Codere');
   WriteLn('---------------------------------------------------------');
   { HELP INOFRMATION }
    If Paramcount = 0 then
    Begin
      WriteLn('Usage: ', ParamStr(0), '  filetoplay.sa2');
      WriteLn('Press any key to Quit music');
      Halt;
    end;
 SA2Player.Init(paramstr(1));
  i := 1;
 Repeat
{$IFDEF DEBUG}
  CRT.Delay(14);
  Sa2Player.ProcessEvent;
{$ENDIF}
 Until Keypressed;
 SA2Player.Done;
end.