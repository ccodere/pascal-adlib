{************************************************}
{   Simple CMF Player.                           }
{   Copyright (c) 1996, Carl Eric Codere         }
{   Montreal, Canada - November  1995.           }
{   VERSION 1.61 - PROTECTED AND REAL MODES      }
{************************************************}
{ See license.txt for usage of this source code }

{ -------------------------- UPDATE INFORMATION ------------------------- }
{ I am sorry if the previous version was a bit (well a lot) crappy        }
{ this version should be better,I tested it extensively, I've fixed most, }
{ if not all percussive mode problems, I solved most of the trouble with  }
{ semi tone modifications when a note is already playing, I also added    }
{ automatic initialisation of percussive instruments and melodic instru-  }
{ ments (see sound.pas for default instruments).                          }

{ BUGS LEFT -Semitone modification with playing notes could be better     }
{            (an asm speed up could be done)                              }
{           -SetWaveselect in Setvoice is incorrect, OR calcnote is       }
{            incorrect. To play all songs correctly but distorted go in   }
{            sound.pas and change Adlib.SetWaveSelect to always output    }
{            a 1. This will be ok, but the song will be distorted!!       }
{            ANY HELP FOR THIS PROBLEM APPRECIATED!                       }
{ DEFINE DEBUG if you wish to trace the program. It will use crt.delay    }
{ instead of the interrupt routine, so no crashing while tracing!         }


(* Conditions for use of this source code:                                *)
(*      - No re-release of this or the modified source code is done.      *)
(*      - Proper credit is given to the author of these routines.         *)
(*      - That I be notified of any bugfixes / major upgrades.            *)
{       - CONTACT ME FOR COMMERCIAL USE OF THIS CODE!                      }

{ BTW I am not responsibe for any damages caused by the use of this source }
{ code... and all the other legal blahblah..                               }

{ All information on the CMF file format was taken from the PCGPE magazine }
{ which was created by Mark Feldman. The PIT Interrupt was also taken from }
{ him. Thanks for that great encyclopedia!                                 }

{$IFDEF WINDOWS}
 ERROR: THIS PROGRAM WILL NOT RUN AS A WINDOWS PROGRAM
{$ENDIF}

{$IFNDEF CPU86}
 ERROR: THIS PROGRAM USES x86 SPECIFIC CODE
{$ENDIF}

{$IFNDEF VER70}
 ERROR: This program uses TP v7.0 syntax
{$ENDIF}

Program CMFPlay;
{$S-}
{$F+}

Uses Sound,Crt,Dos, Objects;
{ BUGFIX -> Scaling level was not saved correctly }
{    $ (11/03/95)                                                          }
{          BUGFIX Change Instrument Number. Forgot to do a NoteOff event   }
{           when changing instruments.                                     }
{    $ (11/03/95)                                                          }
{          BUGFIX Of the Semitone changes when a note is already playing.  }
{           It is still not perfect (updating is too slow), but much       }
{           better then before nonetheless.                                }
{    $ (11/04/95)                                                          }
{          Repeat midi-event $Bx bugfix.                                   }
{    + (11/11/95)                                                          }
{          Converted everything to an Object. Much  simpler and easier     }
{          to use. You simply need to change to the appropriate stream     }
{          TBufStream, by other streams if you wish (especially by EMS/XMS }
{          streams) if you wish to load from memory, just as in games!     }
{    $ (11/23/95)                                                          }
{          Bugfix of music marker event, the delay would be calculated at  }
{          the wrong index.                                                }
{          Bugfix of the calculate delay routine with Percussive mode      }
{          select.                                                         }
{          BUGFIX Of repeat midi event, with special number.               }
{    TESTED UNDER PROTECTED AND REAL MODES                                 }
{    $ (03/18/96)                                                          }
{          Bugfix of controller changes, undefined controller changes are  }
{          now taken care of, i.e currentpos is updated - thanks "reboot"  }
{    $ (03/19/96)                                                          }
{          Bugfix in calling TheEnd procedure, added mutevoice procedure   }
{          in the code, so that the voices all get muted before starting   }
{          again.                                                          }
{   NOTA: (04/24/96) -> Some CMF files still do not work correctly, I will }
{          try to fix this soon. Hmmm.. quite difficult to fix. Will leave }
{          it as is for the moment at least.                               }
CONST
 CMF_VERSION = $013D;
 CMFInvalidFile    = 1;
 CMFInvalidVersion = 2;

Type
 TChannels = Array[0..15] of Byte;
 (* Header of CMF file *)
 TCMFHeader = Record
  ID: Array[0..3] of Char;    (* Should Contain 'CTMF' *)
  Version: Word;              (* LSB = Minor Version, MSB = Major Version *)
  InstrumentOffset: Word;     (* Offset of file into Instrument block     *)
  MusicOffset: Word;          (* Offset of file into music block          *)
  TicksPerQuartet: Word;      (* Clock ticks per quarter note             *)
(* Calculation :=(TicksPerQuartet*Tempo) div 60 sec.                      *)
  TicksPerSecond: Word;       (* Clock Ticks per second                   *)
  TitleOffset: Word;          (* Offset of file to title                  *)
  AuthorOffset: Word;         (* Offset of file to author name            *)
  NotaOffset: Word;           (* Offset of file to notations              *)
  (* For each voice, tells if voice is used or not:                       *)
  (* 0 = Unused voice                                                     *)
  (* 1 = Used voice                                                       *)
  Channels: TChannels;            (* For all 16 midi CHANNELS Offset 20-35*)
  Instruments: Word;          (* Number of instruments used-1        36-37*)
  Tempo: Word;                                          (* Offset    38-39*)
 end;


 (* A CMF File Instrument Record *)
 TCMFInstrument = Record
  ModulatorChar: Byte;      (* Modulator Characteristic *)
  CarrierChar: Byte;        (* Carrier Characteristic   *)
  ModScalingOutput: Byte;
  CarScalingOutput: Byte;
  ModAttackDelay: Byte;
  CarAttackDelay: Byte;
  ModSustainRelease: Byte;
  CarSustainRelease: Byte;
  ModWaveSelect: Byte;
  CarWaveSelect: Byte;
  FeedBackConnection: Byte;
  Reserved: Array[1..5] of Byte;
 end;

  { Maximum Size of a CMF File }
  TSong = Array[1..60000] of Byte;
  PSong = ^TSong;


 PCMFPlayer = ^TCMFPlayer;
 TCMFPlayer = Object(TObject)
   Adlib: TAdlib;                  { Adlib sound card driver   }
   Playing: Boolean;               { TRUE if a song is playing }
   Header: TCMFHeader;             { Music Header              }
   Channels: TChannels;            { Channels in use           }
   { Call this to play file -> AStream: Music Stream to read from }
   Constructor Init(VAR AStream: TStream);
   { Called when there is an invalid file format }
   Procedure Error(Value: Integer); Virtual;
   { CALL IT AT THE END!! Remove Interrupt Handler }
   Destructor Done; Virtual;
   { Called at the End of the song }
   Procedure TheEnd; Virtual;
   { Get Version of Driver }
   Function GetVersion: Word;
   { Stop the old song and start a new song }
   Procedure NewSong(VAR AStream: TStream);
 private
   TimerReplaced: Boolean;
   CopiedValue: Byte;
   CurrentPos: Word;  { Current Position in song }
   Delay: Word;       { Current Delay Count      }
   Song: PSong;       { Pointer to the Song data }
   InstrTable: Array[0..255] of TCMFInstrument;
   ErrorValue: Integer;
   { Verifies if song is correct format  }
   Function VerifySong: Boolean;
   { Processes the Musical events }
   Procedure ProcessEvent;
   { Sets a voice to a particular instrument }
   Procedure SetVoice(Voice:Byte; Instr: TCMFInstrument);
   { Calculates de Delta-Time Delays }
   Procedure CalculateDelay(Var Index: Byte);
 end;





(*************************************************************************)
(*                           MAIN ROUTINES                               *)
(*************************************************************************)

const TIMERINTR = 8;
       PIT_FREQ = $1234DD;

var BIOSTimerHandler : procedure;
    clock_ticks, counter : longint;
    { CMF Object Instance }
    CMFPlayer: TCMFPlayer;



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
   CMFPlayer.ProcessEvent;
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


Function TCMFPlayer.GetVersion: Word;
Begin
 GetVersion := CMF_VERSION;
end;

Constructor TCMFPlayer.Init(VAR AStream: TStream);
(*************************************************************************)
(*  TCMFPlayer.Init                                                      *)
(*   -> Initialises sound card, variables, interrupt handler and         *)
(*   Stream.                                                             *)
(*************************************************************************)
VAR
 i: Byte;
Begin
 Inherited Init;
 i := 0;
 Adlib.Init;
 AStream.Seek(0);
 AStream.Read(Header, SizeOf(Header));
 If (NOT VerifySong) then
    Error(ErrorValue);
 New(Song);
 AStream.Seek(Header.InstrumentOffset);
  { READIN INSTRUMENTS }
  Repeat
   AStream.Read(InstrTable[i], SizeOf(TCMFInstrument));
   inc(i);
  Until i = Header.Instruments;
 Channels := Header.Channels;
 AStream.Seek(Header.MusicOffset);
 AStream.Read(Song^, AStream.GetSize - AStream.GetPos);
 AStream.Done;
{$IFNDEF DEBUG}
 { The code is slow so adjust song speed accordingly         }
 { if everything was in ASM we would not have this problem ! }
 TimerReplaced := TRUE;
 SetTimer(Addr(Handler), Header.TicksperSecond + Header.TicksperSecond div 4);
{$ENDIF}
end;

Procedure TCMFPlayer.NewSong(VAR AStream: TStream);
VAR
 i,j: Byte;
Begin
 If Playing then
 Begin
  For J:=0 to 63 do
  Begin
   For i:=0 to Adlib.voices do
   Begin
     If Adlib.Volume[i] > 0 then Adlib.SetVolume(i, Adlib.Volume[i]-1);
   end;
  end;
 end;
 i:=0;
 { Re-initialise all variables }
 CopiedValue := 0;
 CurrentPos := 0;
 ErrorValue := 0;
 Delay := 0;
 If Song <> Nil then Dispose(Song);
{$IFNDEF DEBUG}
 If TimerReplaced then
 Begin
  CleanUpTimer;
  TimerReplaced := FALSE;
 end;
{$ENDIF}
 Playing := FALSE;
 AStream.Seek(0);
 AStream.Read(Header, SizeOf(Header));
 If (NOT VerifySong) then
    Error(ErrorValue);
 New(Song);
 AStream.Seek(Header.InstrumentOffset);
  { READIN INSTRUMENTS }
  Repeat
   AStream.Read(InstrTable[i], SizeOf(TCMFInstrument));
   inc(i);
  Until i = Header.Instruments;
 Channels := Header.Channels;
 AStream.Seek(Header.MusicOffset);
 AStream.Read(Song^, AStream.GetSize - AStream.GetPos);
 AStream.Done;
{$IFNDEF DEBUG}
 { The code is slow so adjust song speed accordingly         }
 { if everything was in ASM we would not have this problem ! }
 TimerReplaced := TRUE;
 SetTimer(Addr(Handler), Header.TicksperSecond + Header.TicksperSecond div 4);
{$ENDIF}
end;



Procedure TCMFPlayer.SetVoice(Voice:Byte; Instr: TCMFInstrument);
(*************************************************************************)
(*  TCMFPlayer.SetVoice(Voice: Byte; Instr: TCMFInstrument)              *)
(*   -> Sets the Instruments passed as a parameter to the specified      *)
(*   music card voice.                                                   *)
(*                                                                       *)
(*************************************************************************)
Begin
 If ((Voice < 9) AND (NOT Adlib.Percussive))  OR ((Adlib.Percussive)
 { In Melodic mode call this                         }
 { In percussive mode, calls this if Bass drum voice }
 { or if a melodic voice is used.                    }
  AND (Voice < 7)) then
 Begin
  Adlib.SetWaveSelect(Voice, MODULATOR, Instr.ModWaveSelect);
  Adlib.SetWaveSelect(Voice, CARRIER, Instr.CarWaveSelect);
  Adlib.SetWaveChar(Voice, MODULATOR, Instr.ModulatorChar);
  Adlib.SetWaveChar(Voice, CARRIER, Instr.CarrierChar);
  Adlib.AllScalingOutput(Voice, MODULATOR, Instr.ModScalingOutput);
  Adlib.AllScalingOutput(Voice, CARRIER, Instr.CarScalingOutput);
  { Save the scaling output for the second operator }
  { this is the type which controls the volume of   }
  { the voice.                                      }
  Adlib.CarrierScalingLevel[Voice] := Instr.CarScalingOutput AND $C0;
  Adlib.ModulatorScalingLevel[Voice] := Instr.ModScalingOutput AND $C0;
  Adlib.Algorithm[Voice] := Boolean(Instr.FeedBackConnection AND $01);
  Adlib.Volume[Voice] := Instr.CarScalingOutput AND $3f;
  Adlib.Volume[Voice] := Byte(Adlib.Volume[Voice] - $3F);
  Adlib.Volume[Voice] := Byte(0 - Adlib.Volume[Voice]);
  Adlib.AllAttackDecay(Voice, MODULATOR, Instr.ModAttackDelay);
  Adlib.AllAttackDecay(Voice, CARRIER, Instr.CarAttackDelay);
  Adlib.AllFeedBack(Voice, Instr.FeedBackConnection);
  Adlib.AllSusRelease(Voice, MODULATOR, Instr.ModSustainRelease);
  Adlib.AllSusRelease(Voice, CARRIER,   Instr.CarSustainRelease);
 end
 else
  { This is a normal percussive voice }
  { Bug fix: Percussive mode music instruments in CMF file still use }
  { carrier level for normal output level calculation.               }
 Begin
  SndOutput(TablePModulator[Voice] + $20, Instr.ModulatorChar);
  SndOutput(TablePModulator[Voice] + $40, Instr.ModScalingOutput);
  SndOutput(TablePModulator[Voice] + $60, Instr.ModAttackDelay);
  SndOutput(TablePModulator[Voice] + $80, Instr.ModSustainRelease);
  SndOutput(Voice + $C0, Instr.FeedbackConnection);
  SndOutput(TablePModulator[Voice] + $E0, Instr.ModWaveSelect);
  Adlib.CarrierScalingLevel[Voice] := Instr.CarScalingOutput AND $C0;
  Adlib.ModulatorScalingLevel[Voice] := Instr.ModScalingoutput AND $C0;
  Adlib.Algorithm[SnareDrumVoice] := Boolean(Instr.FeedBackConnection AND $01);
  Adlib.Volume[Voice] := Instr.CarScalingOutput AND $3f;
  Adlib.Volume[Voice] := Byte(Adlib.Volume[Voice] - $3F);
  Adlib.Volume[Voice] := Byte(0 - Adlib.Volume[Voice]);
 end;
end;


(*************************************************************************)
(*  TCMFPlayer.VerifySong: Boolean                                       *)
(*    Verifies if the song is a valid format.                            *)
(*************************************************************************)
Function TCMFPlayer.VerifySong: Boolean;
begin
 VerifySong := TRUE;
 If Header.ID <> 'CTMF' then
 Begin
  VerifySong := FALSE;
  ErrorValue := CMFInvalidFile;
 end;
 { Maximum supported revision number is 1.1 }
 If Header.Version > 257 then
 Begin
  VerifySong := FALSE;
  ErrorValue := CMFInvalidVersion;
 end;
end;


Procedure TCMFPlayer.Error(Value: Integer);
(*************************************************************************)
(*  VIRTUAL METHOD ERROR                                                 *)
(*   -> Called when there is an invalid music file format.               *)
(*************************************************************************)
Begin
 Case ErrorValue of
 CMFInvalidFile:    WriteLn('ERROR! Invalid File Format');
 CMFInvalidVersion: WriteLn('ERROR! Invalid CMF Version');
 else
  WriteLn('ERROR!');
 end;
 Halt(1);
end;



Destructor TCMFPlayer.Done;
(*************************************************************************)
(*  TCMFPlayer.Done                                                      *)
(*   -> Closes done the Sound System. Silences all voices and releases   *)
(*      allocated heap memory. REMOVES INTERRUPT HANDLER.                *)
(*************************************************************************)
Begin
{$IFNDEF DEBUG}
 CleanUpTimer;
 TimerReplaced := FALSE;
{$ENDIF}
 Adlib.Done;
 If Song <> Nil then Dispose(Song);
end;


Procedure TCMFPlayer.CalculateDelay(Var Index: Byte);
(*************************************************************************)
(*  TCMFPlayer.CalculateDelay(Var Index: Byte)                           *)
(*   -> Calculates the Delta-time delay.                                 *)
(*      Modifies Delay Global Variable, which contains Delta-Time delay  *)
(*  Index: Byte -> Current Index of Song (Current Position)              *)
(*************************************************************************)
{ Ideas for this procedure                           }
{  Taken from 'Le Grand Livre de la Sound Blaster'   }
{  ISBN: 2-86899-758-9                               }
{ OPTIMISED FOR SPEED USING SIMPLE MATH OPERATORS    }
Var
 LocalIndex: Byte;
 i: Byte;
 byteValues: Array[0..3] of Byte;
 Power: Real;
 ActData: Byte;
Begin
    For i:=0 to 3 do ByteValues[i] := 0;
    LocalIndex := 0;
    ActData := 128;
    WHILE (ActData AND $80) = $80 DO
    BEGIN
     ActData := Song^[CurrentPos+Index+1];
     ByteValues[LocalIndex] := ActData;
     Inc(Index);
     Inc(LocalIndex);
    END;

    Delay := 0;
    FOR i:= 0 to (LocalIndex-1) do
    Begin
{    Power := Exp(i * Ln(128));                                             }
{    Delay := Delay + (Trunc(Power)*(ByteValues[(LocalIndex-1)-i] AND $7F));}
{ REPLACED BY:                                                              }
     Delay := Delay + (ByteValues[i] AND $7F) SHL (7*(LocalIndex-i-1));
    end;
end;

(*************************************************************************)
(*  THIS IS THE MAIN LOOP IN THE PROCESSING OF THE MIDI EVENTS           *)
(*************************************************************************)
Procedure TCMFPlayer.ProcessEvent;
Var
 Volume: Byte;
 Index: Byte;
 Note: byte;
 Voice: Byte;
Begin
  If Delay > 0 then
  Begin
    Dec(Delay);
    Exit;
 end;
 If not Playing then
 Begin
  CurrentPos:=1;
  Playing:=True;
 end;


 Volume :=0;
 Note   :=0;
 Voice  :=0;
 Case Song^[CurrentPos] of
(* Note Number Off *)
 $80,$81,$82,$83,$84,$85,$86,$87,$88,$89,$8A,$8B,$8C,$8D,$8E,$8F:
    Begin
       CopiedValue:=Song^[CurrentPos];
       Voice := Song^[CurrentPos] AND $0F;
       { CMF channels 12 to 15 are the equivalent of voices }
       { 7,8,9,10                                           }
       If Voice > 9 then Voice := Voice - 5;
       Note := Song^[CurrentPos+1];
       Volume:=Song^[CurrentPos+2];

       Index:=2;

       If Volume = 0 then Adlib.SetVolume(Voice, Volume);
       Adlib.NoteOff(Voice, Note);

       CalculateDelay(Index);
       Inc(CurrentPos, Index+1);
     end;

(* Note Number on *)
 $90,$91,$92,$93,$94,$95,$96,$97,$98,$99,$9A,$9B,$9C,$9D,$9E,$9F:
    Begin
       Index:=2;
       CopiedValue:=Song^[CurrentPos];
       Voice  := Song^[CurrentPos] and $0F;
       { CMF channels 12 to 15 are the equivalent of voices }
       { 7,8,9,10                                           }
       If Voice > 9 then Voice := Voice - 5;
       Note   := Song^[CurrentPos+1];
       Volume := Song^[CurrentPos+2];


       If Volume = 0 then
           Adlib.Noteoff(voice, note)
       else
       Begin
          Adlib.SetVolume(Voice, Volume);
          Adlib.NoteOn(Voice, Note);
       end;

         CalculateDelay(Index);
       Inc(CurrentPos, Index+1);
     end;
(* END OF SONG *)
 (* Check if end of song *)
 $FF: Begin
     If (Song^[CurrentPos+1] = $2f) and (Song^[CurrentPos+2] = $00) then
     Begin
       Volume:=Song^[CurrentPos+2];
       Playing:=False;
       TheEnd;
      end;
      end;
(* CONTROLLER CHANGE *)
 $B0,$B1,$B2,$B3,$B4,$B5,$B6,$B7,$B8,$B9,$BA,$BB,$BC,$BD,$BE,$BF:
    Begin
      CopiedValue := Song^[CurrentPos];
      Case Song^[CurrentPos+1] of
      $66: (* Music Marker *)
           Begin
             Index:=2;
             CalculateDelay(Index);
             Inc(CurrentPos, Index+1);
           end;
      $67: (* Specify which mode 1 = PERCUSSIVE *)
           Begin
            Index:=2;
            Adlib.SetPercussive(Boolean(Song^[CurrentPos+2]));
            CalculateDelay(Index);
            Inc(CurrentPos, Index+1);
           end;
      $68: (* All note upward *)
           Begin
            Index:=2;
            Voice := Song^[CurrentPos] AND $0F;
       { CMF channels 12 to 15 are the equivalent of voices }
       { 7,8,9,10                                           }
            If Voice > 9 then Voice := Voice - 5;
            Adlib.SemiToneUp(Voice, Shortint(Song^[CurrentPos+2]));
            CalculateDelay(Index);
            Inc(CurrentPos, Index+1);
           end;
      $69: (* All notes downward *)
           Begin
            Voice := Song^[CurrentPos] AND $0F;
       { CMF channels 12 to 15 are the equivalent of voices }
       { 7,8,9,10                                           }
            If Voice > 9 then Voice := Voice - 5;
            Index:=2;
            CalculateDelay(Index);
            Adlib.SemiToneDown(Voice, Shortint(Song^[Currentpos+2]));
            Inc(CurrentPos, Index+1);
           end;
      else
         Begin
         { Other controller changes - UNDEFINED USE FOR THE MOMENT }
          Index := 2;
          CalculateDelay(Index);
          Inc(CurrentPos, Index+1);
         end;
      end;
    end;
(* Change to musical instrument nn *)
 $C0,$C1,$C2,$c3,$c4,$c5,$c6,$c7,$c8,$C9,$CA,$CB,$CC,$CD,$CE,$CF:
  Begin
     Voice := Song^[CurrentPos] AND $0F;
       { CMF channels 12 to 15 are the equivalent of voices }
       { 7,8,9,10                                           }
     If Voice > 9 then Voice := Voice - 5;
     CopiedValue := Song^[CurrentPos];
     { Do A NoteOff first with any note number }
     Adlib.NoteOff(Voice, $60);
     Adlib.SemiToneCurrent[Voice] := 0;
     SetVoice(Voice, InstrTable[Song^[CurrentPos+1]]);
     Index:=1;
     CalculateDelay(Index);
     Inc(CurrentPos, Index+1);
  end;
 $00: Inc(CurrentPos);
 else
(*************************************************************************)
(*                      A REPEAT MIDI EVENT                              *)
(*************************************************************************)
 Begin
 Case CopiedValue of
(* Note Number Off *)
 $80,$81,$82,$83,$84,$85,$86,$87,$88,$89,$8A,$8B,$8C,$8D,$8E,$8F:
    Begin
       Voice := CopiedValue AND $0F;
       { CMF channels 12 to 15 are the equivalent of voices }
       { 7,8,9,10                                           }
       If Voice > 9 then Voice := Voice - 5;
       Note := Song^[CurrentPos];
       Volume:=Song^[CurrentPos+1];
       Index:=1;


       Adlib.SetVolume(Voice, Volume);
       Adlib.NoteOff(Voice, Note);

       CalculateDelay(Index);
       Inc(CurrentPos, Index+1);
     end;

(* Note Number on *)
 (* Open Voice 1,2,3,4,5,6,7,8 or 9 *)
 $90,$91,$92,$93,$94,$95,$96,$97,$98,$99,$9A,$9B,$9C,$9D,$9E,$9F:
    Begin
       Index:=1;
       Voice :=CopiedValue and $0F;
       { CMF channels 12 to 15 are the equivalent of voices }
       { 7,8,9,10                                           }
       If Voice > 9 then Voice := Voice - 5;
       Volume:=Song^[CurrentPos+1];
       Note := Song^[CurrentPos];

      If Volume = 0 then
         Adlib.Noteoff(Voice, note)
      else
      Begin
        Adlib.NoteOn(Voice, Note);
        Adlib.SetVolume(Voice, volume);
      end;
        CalculateDelay(Index);
       Inc(CurrentPos, Index+1);
     end;
(* CONTROLLER CHANGE *)
 $B0,$B1,$B2,$B3,$B4,$B5,$B6,$B7,$B8,$B9,$BA,$BB,$BC,$BD,$BE,$BF:
    Begin
      Case Song^[CurrentPos] of
      $66: (* Music Marker *)
           Begin
            Index:=1;
            Calculatedelay(Index);
            Inc(CurrentPos, Index+1);
           end;
      $67: (* Specify which mode *)
           Begin
            Index:=1;
            Adlib.SetPercussive(Boolean(Song^[CurrentPos+1]));
            CalculateDelay(Index);
            Inc(CurrentPos, Index+1);
           end;
      $68:
           Begin
            Voice := CopiedValue AND $0F;
       { CMF channels 12 to 15 are the equivalent of voices }
       { 7,8,9,10                                           }
            If Voice > 9 then Voice := Voice -5;
            Index:=1;
           Adlib.SemiToneUp(Voice, Shortint(Song^[CurrentPos+1]));
           CalculateDelay(Index);
           Inc(CurrentPos, Index+1);
           end;
      $69:
           Begin
            Index:=1;
            Voice := CopiedValue AND $0F;
            If Voice > 9 then Voice := Voice -5;
            Adlib.SemiToneDown(Voice, Shortint(Song^[Currentpos+1]));
            CalculateDelay(Index);
           Inc(CurrentPos, Index+1);
           end;
      else
         Begin
         { Other controller changes - UNDEFINED USE FOR THE MOMENT }
          Index := 1;
          CalculateDelay(Index);
          Inc(CurrentPos, Index+1);
         end;
      end;
    end;
(* Change to musical instrument nn *)
 $C0,$C1,$C2,$c3,$c4,$c5,$c6,$c7,$c8,$C9,$CA,$CB,$CC,$CD,$CE,$CF:
  Begin
     Voice := CopiedValue AND $0F;
       { CMF channels 12 to 15 are the equivalent of voices }
       { 7,8,9,10                                           }
     If Voice > 9 then Voice := Voice - 5;
     Adlib.NoteOff(Voice, $60);
     Adlib.SemiToneCurrent[Voice] := 0;
     SetVoice(Voice, InstrTable[Song^[CurrentPos]]);
      Index:=1;
      CalculateDelay(Index);
      Inc(CurrentPos, Index+1);
  end;
 else
  WriteLn('MIDI FORMAT ERROR At Position:',CurrentPos);
 end;
 end;
end;
end;


Procedure TCMFPlayer.TheEnd;
Var
 i: Byte;
Begin
 { Mute all voices }
 For i:=0 to Adlib.VOICES do
  Adlib.MuteVoice(i);
 WriteLn('THE END. START AGAIN!');
end;


Var
 ch: Char;
 InLoop: Longint;
 i: Byte;
 Stream: PStream;
Begin
  { Register the Stream Error procedure }
  StreamError:= @StreamErrorProcedure;
  { LITTLE TITLE }
   WriteLn('Simple CMF Player Version ',Hi(CMFPlayer.GetVersion),'.',
    Lo(CMFPlayer.GetVersion),' by Carl Eric Codere');
   WriteLn('---------------------------------------------------------');
   { HELP INOFRMATION }
    If Paramcount = 0 then
    Begin
      WriteLn('Usage: ', ParamStr(0), '  filetoplay.cmf');
      WriteLn('Press any key to Quit music');
      Halt;
    end;
 { Create a new buffered stream }
 Stream := New(PBufStream, Init(ParamStr(1), StOpenRead, 2048));
 { Init and play CMF file }
 CMFPlayer.Init(Stream^);
 { INDICATE WHICH VOICES/CHANNELS WILL BE USED }
 For i:=0 to 15 do
 Begin
  If CMFPlayer.Header.Channels[i] = 1 then
   WriteLn('Channel ',i,' used.');
 end;
 Repeat
 {$IFDEF DEBUG}
   Inc(Inloop);
   CMFPlayer.ProcessEvent;
   CRT.Delay(10);
{$ENDIF}
 Until Keypressed;
 { Restore interrupt vector }
 CMFPlayer.Done;
end.


