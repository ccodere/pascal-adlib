{************************************************}
{   Simple SCR Player.                           }
{   Copyright (c) 1995, Carl Eric Codere         }
{   Montreal, Canada - Febuary 1996.             }
{   VERSION 1.50                                 }
{************************************************}
{ See license.txt for usage of this source code }
{ THIS CODE IS UNFINISHED AND DOES NOT WORK!    }
 
{ DEFINE DEBUG if you wish to trace the program. It will use crt.delay    }
{ instead of the interrupt routine, so no crashing while tracing!         }


Uses Sound,Objects,DOS,CRT;




TYPE

 TChannels = Array[0..15] of Byte;
  { Maximum Size of a CMF File }
  TSong = Array[1..60000] of Byte;
  PSong = ^TSong;


 { SCR Instruments }
 TSCRInstrument = record
   MOD_KSL:Byte;
   MOD_FMULT:Byte;
   MOD_ATTACK:Byte;
   MOD_SUSTAIN:Byte;
   MOD_SS:Byte;
   MOD_DECAY:Byte;
   MOD_Release:Byte;
   MOD_OUTPUTLEVEL:Byte;
   MOD_AmplitudeVibrato:Byte;
   MOD_frequencyVibrato:Byte;
   MOD_envelopeScaling:Byte;
   CAR_KSL,CAR_fMult,CAR_FB,CAR_attack,CAR_sustain,CAR_ss:Byte;
   CAR_decay,CAR_release,CAR_outputLevel:Byte;
   CAR_amplitudeVibrato,CAR_frequencyVibrato,CAR_envelopeScaling:Byte;
   FM:Boolean;
   MOD_waveForm,CAR_waveForm,feedBack,PADDING:Byte;
 end;

 TSCRHeader = Record
  Percussive: Boolean;
  Instruments: Byte;   { Number of Instruments }
 end;




 PSCRPlayer = ^TSCRPlayer;
 TSCRPlayer = Object(TObject)
   Delay: Word;       { Current Delay Count      }
   Song: PSong;       { Pointer to the Song data }
   CurrentPos: Word;  { Current Position in song }
   Adlib: TAdlib;
   Playing: Boolean;  { TRUE if a song is playing }
   CopiedValue: Byte;
   Header: TSCRHeader;
   Stream: TBufStream;
   Channels: TChannels;            { Channels in use }
   InstrTable: Array[0..255] of TSCRInstrument;
   NoteTimer:Array[0..11] of Byte;
   { Call this to play file -> AfileName: CMF File name to play }
   Constructor Init(AFileName: FNameStr);
   { Verifies if song is correct format  }
{   Function VerifySong: Boolean;}
   { Sets a voice to a particular instrument }
   Procedure SetVoice(Voice:Byte; Instr: TSCRInstrument);
   { Processes the Musical events }
   Procedure ProcessEvent;
   { Called when there is an invalid file format }
{   Procedure Error(Value: Integer); Virtual; }
   { CALL IT AT THE END!! Remove Interrupt Handler }
   Destructor Done; Virtual;
   { Called at the End of the song }
   Procedure TheEnd; Virtual;
 private
   ErrorValue: Integer;
 end;


Procedure TSCRPlayer.ProcessEvent;
Var
 i,j,k:Integer;
Begin
 If (NOT Playing) then Exit;

 for i:=0 to (11-1) do
 Begin
  If NoteTimer[i] > 0 then
  Begin
   Dec(Notetimer[i]);
   If (Notetimer[i]  <= 0) then Adlib.NoteOff(i,0);
  end;

 end;
 If Delay <> 0 then Dec(Delay);
 If Delay <= 0 then
 Begin
 (* If it is not a pauseTime in the song. THEN *)
 (* Depending on the next Data of the song, do action *)
 (* If Byte = 0 then PLAY NOTE                        *)
 (* If Byte = 1 then CHANGE INSTRUMENT                *)
 (* If Byte = 2 then CREATE A PAUSE                   *)
 (* If Byte = 3 then END OF SONG                      *)
   Delay:=0;
   While Delay = 0 do
   Begin
    Case Song^[CurrentPos]  (* Case of Event Value *) of
    0: Begin  (* Play A note *)
             i:=Song^[CurrentPos+1];
             j:=Song^[CurrentPos+2];
       k:=Song^[CurrentPos+3];
             Adlib.NoteOn(i,j);
       noteTimer[i]:=k;
       CurrentPos:=CurrentPos+5;
      end;
    2: Begin  (* Play instrument *)
    i:=Song^[CurrentPos+1];
    j:=Song^[CurrentPos+2];
        (* Get the instrument Number and its Data *)
        (* Offset 2 * Instrument Number * j       *)
         SetVoice(i,InstrTable[j]);
   CurrentPos:=CurrentPos+3;
      end;
     3: (* pause *)
        (* Ticks are in words, get full word *)
        Begin
         Delay:=Song^[CurrentPos+1]*256+Song^[CurrentPos];
   CurrentPos:=CurrentPos+3;
        end;
   4: (* end of song *)
            TheEnd;
      else
      Begin
       WriteLn('ERROR!')
      end;
     end;
     end;
 end;
end;


(*************************************************************************)
(*                           MAIN ROUTINES                               *)
(*************************************************************************)

const TIMERINTR = 8;
       PIT_FREQ = $1234DD;

var BIOSTimerHandler : procedure;
    clock_ticks, counter : longint;
    SCRPlayer: TSCRPlayer;



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
   SCRPlayer.ProcessEvent;
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



Constructor TSCRPlayer.Init(AFileName: FNameStr);
 VAR
 i: Byte;
Begin
 { Initialise all local variables }
 Inherited Init;
 i := 0;
 CurrentPos:=1;
 { Initialise sound card          }
 Adlib.Init;
 { Get file }
 Stream.Init(AFileName, StOpenRead, 1024);
 { Read file into memory }
 Stream.Read(Header, SizeOf(Header));
 Adlib.SetPercussive(Header.Percussive);
 New(Song);
  { READIN INSTRUMENTS }
  Repeat
   Stream.Read(InstrTable[i], SizeOf(TSCRInstrument));
   inc(i);
  Until i = Header.Instruments;
 Stream.Read(Song^, Stream.GetSize - Stream.GetPos);
 Stream.Done;
 Playing := TRUE;
{$IFNDEF DEBUG}
 SetTimer(Addr(Handler), 18);
{$ENDIF}
end;



Procedure TSCRPlayer.SetVoice(Voice:Byte; Instr: TSCRInstrument);
(* Load a voice with a particular instrument *)
Var
 i: Boolean;
Begin
 If (Voice < 9) AND (NOT Adlib.Percussive)  OR ((Adlib.Percussive)
 { In Melodic mode call this                         }
 { In percussive mode, calls this if Bass drum modes }
 { or if a melodic voice is used.                    }
 AND (Voice < 7)) then
 Begin
  Adlib.SetWaveChar(Voice, MODULATOR,(Instr.MOD_amplitudeVibrato shl 7) +
       (Instr.MOD_frequencyVibrato shl 6) +
       (Instr.MOD_ss shl 5)               +
       (Instr.MOD_envelopeScaling shl 4)  +
       (Instr.MOD_fMult));
  Adlib.AllScalingOutput(Voice, MODULATOR,
       (Instr.MOD_KSL shl 6) +
       (Instr.MOD_outputLevel));
  Adlib.AllAttackDecay(Voice, MODULATOR,
       (Instr.MOD_attack shl 4) + (Instr.MOD_decay));
  Adlib.AllSusRelease(Voice, MODULATOR,
             (Instr.MOD_sustain shl 4) + (Instr.MOD_release));
  Adlib.SetWaveSelect(Voice, MODULATOR, Instr.MOD_waveform);
  Adlib.SetWaveChar(Voice, CARRIER,
       (Instr.CAR_amplitudeVibrato shl 7) +
       (Instr.CAR_frequencyVibrato shl 6) +
       (Instr.CAR_ss shl 5)               +
       (Instr.CAR_envelopeScaling shl 4)  +
       (Instr.CAR_fMult));
  Adlib.AllScalingOutput(Voice, CARRIER,
       (Instr.CAR_KSL shl 6) +
       (Instr.CAR_outputLevel));
  Adlib.AllAttackDecay(Voice, CARRIER,
       (Instr.CAR_attack shl 4) + (Instr.CAR_decay));
  Adlib.AllSusRelease(Voice, CARRIER,
       (Instr.CAR_sustain shl 4) + (Instr.CAR_release));
  Adlib.SetWaveSelect(Voice, CARRIER, Instr.CAR_waveForm);

  Adlib.CarrierScalingLevel[Voice] := Instr.Car_KSL AND $C0;
  Adlib.ModulatorScalingLevel[Voice] := Instr.Mod_KSL AND $C0;
  Adlib.Volume[Voice] := Instr.Car_outputLevel;
  Adlib.Volume[Voice] := Byte(Adlib.Volume[Voice] - $3F);
  Adlib.Volume[Voice] := Byte(0 - Adlib.Volume[Voice]);

  { Opposite of the normal convention }

  If Instr.FM then
  Begin
     i :=FALSE;
     Adlib.Algorithm[Voice] := i;
  end
  else
  Begin
     i :=TRUE;
     Adlib.Algorithm[Voice] := i
  end;
   Adlib.AllFeedBack(Voice, Instr.Feedback SHL 1+Byte(i));
 end
 else
  Begin
  SndOutput(TablePModulator[Voice] + $20,
         (Instr.MOD_amplitudeVibrato shl 7) +
       (Instr.MOD_frequencyVibrato shl 6) +
       (Instr.MOD_ss shl 5)               +
       (Instr.MOD_envelopeScaling shl 4)  +
       (Instr.MOD_fMult));

   SndOutput(TablePModulator[Voice] + $40,
         (Instr.MOD_KSL shl 6) +
       (Instr.MOD_outputLevel));

   SndOutput(TablePModulator[Voice] + $60,
       (Instr.MOD_attack shl 4) + (Instr.MOD_decay));

   SndOutput(TablePModulator[Voice] + $80,
       (Instr.MOD_sustain shl 4) + (Instr.MOD_release));

   SndOutput(TablePModulator[Voice] + $E0, Instr.Mod_Waveform);
    end;
end;


Destructor TSCRPlayer.Done;
(*************************************************************************)
(*  TCMFPlayer.Done                                                      *)
(*   -> Closes done the Sound System. Silences all voices and releases   *)
(*      allocated heap memory. REMOVES INTERRUPT HANDLER.                *)
(*************************************************************************)
Begin
{$IFNDEF DEBUG}
 CleanUpTimer;
{$ENDIF}
 Adlib.Done;
 If Song <> Nil then Dispose(Song);
end;

Procedure TSCRPlayer.TheEnd;
Begin
 CurrentPos:=0;
 WriteLn('THE END');
end;

Begin
 StreamError:= @StreamErrorProcedure;
 SCRPlayer.Init(paramStr(1));
 Repeat
 {$IFDEF DEBUG}
  SCRPlayer.ProcessEvent;
 {$ENDIF}
 Until Keypressed;
 SCRPlayer.Done;
end.
