{***************************************}
{  Adlib Low Level Unit                 }
{  Copyright (c) 1995, Carl Eric Codere }
{  Montreal, Canada - September 1995    }
{***************************************}
{ See license.txt for usage of this source code }

{ INFORMATION:                                                             }
{   FIRST AND FOREMOST -> Jeffrey S. Lee for his documentation on the      }
{      OPL2 Chipset found in the Adlib Sound Card.                         }
{   UNDISCLOSED ADLIB CODE -> some BASIC Code written by Adlib, which      }
{      actually contained some bugs.                                       }
{   SBFMDRV Tracing to find out how to calculate the SemiTones and Note    }
{      modifications.                                                      }
{   and last but not least: The person who gave me the original idea of    }
{      starting this project: Ezra Dreisbach with his C SCR FM Player.     }





{  Updates/Bugfixes:                                                       }
{      (09/12/95)                                                          }
{   $      Removed some bugs in array definitions, forgot to load Self     }
{          pointer for Object fields in certain routines.                  }
{   $  (09/15/95)                                                          }
{          Forgot Object Inherited Init -> I.e Data WAS NOT initialized to }
{          zero.                                                           }
{   +  (10/15/95)                                                          }
{          Added Initialisation of Melodic voices pianos.                  }
{          Added volume control for percussive instruments.                }
{   $  (11/03/95)                                                          }
{          Bugfix of Note table                                            }
{   ? (11/23/95) HELP!!! There is a bug in one of the test songs. The      }
{                distortion parameter does not seem to work!!!             }
{                In other words when I use the setwaveselect routine with  }
{                an input of zero when setting an instrument, nothing      }
{                happens (it seems!!?), either that or the calcnote is     }
{                wrong somewhere! Help!! (test song was enemywin.cmf)      }                                   
{                It works correctly with the SBFM Driver!                  }
{   + (11/23/95) Removed a few lines of code                               }
{   $ (11/23/95) BassDrumVoice is never FM synthesis, it seems, therefore  }
{                only one operator has to be changed to change the volume  }
{   +$ (12/19/95) Of course the bug is still there! But i replaced most    }
{                SndOutput routines by Fastoutput, so a little  bit less   }
{                static. (still too much for my taste though).             }
{   $ (06/12/96)                                                           } 
{                Fixed a small bug in the SetVolume routine, one of the    }  
{                registers was ORed two times with different values.       }
{                 - Thank you very much for noticing this bug Kevin Horton }
{                   (khorton@tech.iupui.edu)                               }

{$S-}
Unit Sound;



{$IFNDEF CPU86}
 ERROR: THIS PROGRAM USES x86 SPECIFIC CODE
{$ENDIF}

{$IFNDEF VER70}
 ERROR: This program uses TP v7.0 syntax
{$ENDIF}

(* Declaration for this module's routines *)
INTERFACE

Uses Objects,Crt;

{-----------------------------------------------------------------}

CONST
  { Do not forget that the voices start from voice ZERO ! }
  MODULATOR = 0;
  CARRIER =   1;
(* Percussive voice numbers *)
  BassDrumVoice  = 6;          { Bass Drum Voice number   }
  SnareDrumVoice = 7;          { Snare Drum Voice number  }
  TomTomVoice    = 8;          { Tom-Tom Voice Number     }
  CymbalVoice    = 9;          { Cymbal Voice number      }
  HiHatVoice     = 10;         { Hihat Voice number       }


 { IMPORTANT WHILE CHECKING MIDI NOTES ! }
 MidiMiddleC = 60;      { MIDI Specification Middle-C                      }
 CardMiddleC = 48;      { Adlib Sound Card Register Middle C               }

 { Maximum melodic channels starting from 1     }
 MAXMELODICCHANNELS      = 09;  { 1..9  }
 { Maximum Percussive channels starting from 1  }
 MAXPERCUSSIVECHANNELS   = 11;  { 1..11 }


(* Offset of modulator for each voice *)
TableModulator:Array[0..13] of byte = (
      $00,$01,$02,$08,$09,$0a,
      $10,$11,$12,$01,$11,$4f,
      $00,$0f1);

(* Offset of operator for percussive mode     *)
(* Offset memory boundary cross in ASM Source *)
TablePModulator:Array[0..10] of Byte = (
       $f2,$53,$74,$00,$00,$08,
       $10,$14,$12,$15,$11);

(* Offset of Modulator in each voice *)
SlotMVoice: Array[0..8,0..1] of Byte = (

      ($00, $03),
      ($01, $04),
      ($02, $05),
      ($08, $0B),
      ($09, $0C),
      ($0a, $0D),
      ($10, $13),
      ($11, $14),
      ($12, $15));

  { Percussion Rhythm Enable bits }
  PercBits:Array[0..4] of Byte =
    ($10,$08,$04,$02,$01);


PercussiveOffset:Array[0..10] of Byte = (
        $11,$10,$08,$04,$02,$01,
        $06,$07,$08,$08,$07)
        ; { Offset voice when in percussive voice. }

{----------------------------------------------------------------- }



Type
 TBit = 0..1;


PAdlib = ^TAdlib;
TAdlib = Object(TObject)
 Public
  { This determines if both operators produce sound directly         }
  { if so, then modyfing the volume must be done with both operators }
  { TRUE = Both Modulator and Carrier must be changed to change      }
  { the volume. (TRUE = FM MODE, FALSE = AM MODE )                   }
  Algorithm: Array[0..10] of Boolean;

  Percussive: Boolean;              (* Percussion Mode parameter *)
  Voices: Integer;                  (* maximum number of available voices *)
  Volume: Array[0..10] of Byte; { Maximum volume for that voice READ ONLY!}
  ModulatorScalingLevel:Array[0..10] of Byte;
  CarrierScalingLevel:Array[0..10] of Byte;
  Output : Array[0..15] of Word;            { Direct A0B0 Register outputs }
  SemiToneCurrent: Array[0..10] of Integer; { Current semi tones modify    }
  BdRegister: Byte;                         { Current Value in BDRegister  }
  PlayingNote: Array[0..10] of Byte;        { Current playing notes        }

  { Initialise sound card in melodic mode with pianos }
  Constructor Init;
  { Put all adlib registers back to zero              }
  Destructor Done; Virtual;
  { Called when no adlib card is found                }
  Procedure CardNotInstalled; Virtual;
  { Silence a voice with the specified note           }
  Procedure NoteOff(Voice:Byte; Note:Byte);
  { Play a voice with the specified note              }
  Procedure NoteOn(Voice: Byte; Note: Byte);
  { Change the volume of a voice (0 (lowest) < Volume < 127 }
  Procedure SetVolume(Voice: Byte; AVolume: Byte);
  { Modify the notes by 1-127 semi-tones                    }
  Procedure SemiToneup(Voice: Byte; Value: Shortint);
  { Modify the notes by 1-127 semi-tones                    }
  Procedure SemiToneDown(Voice: Byte; Value: Shortint);
  { Silences a note by changing attack rate }
  Procedure MuteVoice(Voice: Byte);
(* Selects either Melodic or Percussive mode *)
(* TRUE = Percussive Mode                    *)
  Procedure SetPercussive(Mode: Boolean);






 {----------------------------------------------------------------}
 (* *  Writes to Registers 20h-35h                             * *)
 (* *    AMP. MODULATION / VIBRATO / EG TYPE / MODULATION      * *)
 (* *     MULTIPLIER                                           * *)
 (* *    SetWaveChar -> Writes Values directly to Register     * *)
 {----------------------------------------------------------------}
  Procedure SetWaveChar(Voice: Byte; ModCar: TBit; Value: Byte);

 {----------------------------------------------------------------}
 (* *  Writes to Register 40h-55h                              * *)
 (* *    SCALING LEVEL  / OUTPUT LEVEL                         * *)
 (* *  AllScalingOutput -> Writes Byte directly to register    * *)
 {----------------------------------------------------------------}
  Procedure AllScalingOutput(Voice: Byte; ModCar: TBit; Value: Byte);

 {----------------------------------------------------------------}
 (* *  Writes to Registers 60h-75h                             * *)
 (* *    ATTACK RATE / DECAY RATE                              * *)
 (* *  AllAttackDecay -> Writes Value directly to Register     * *)
 {----------------------------------------------------------------}
  Procedure AllAttackDecay(Voice: Byte; ModCar: TBit; Value: Byte);

 {----------------------------------------------------------------}
 (* *  Writes to Registers 80h-95h                             * *)
 (* *     SUSTAIN RATE/ RELEASE RATE                           * *)
 (* *   AllSusRelease -> Outputs Byte directly to Register     * *)
 {----------------------------------------------------------------}
  Procedure AllSusRelease(Voice: Byte; ModCar: TBit; Value: Byte);

 {----------------------------------------------------------------}
 (* *  Writes To Register BDh                                  * *)
 (* *  AMPLITUDE MODULATION DEPTH / VIBRATO DEPTH / RHYTHM     * *)
 (* *                                                          * *)
 (* *  SetAM  -> Toggles the AM Depth On/Off Only              * *)
 (* *  SetVib -> Toggles the Vibrato Depth On/Off Only         * *)
 (* *                                                          * *)
 {----------------------------------------------------------------}
  Procedure SetAM(AM: Boolean);
  Procedure SetVibrato(Vib: Boolean);

 {----------------------------------------------------------------}
 (* *  Writes To Registers C0h-C8h                             * *)
 (* *   AllFeedBack -> Outputs Byte directly to Register       * *)
 {----------------------------------------------------------------}
  Procedure AllFeedback(Voice: Byte; Value: Byte);

 {----------------------------------------------------------------}
 (* * Writes To Port E0h-F5h -> Selects Waveform to use        * *)
 (* *    WAVEFORM SELECT                                       * *)
 (* *   Only value in this register                            * *)
 {----------------------------------------------------------------}
  Procedure SetWaveSelect(Voice:Byte;ModCar: TBit; Value: Byte);
Private
  { Calculates the note to play }
  Function CalcNote(Voice:byte; Note: Byte): Word;
  { Sets the default rhythm instruments }
  Procedure SetRhythmInstruments;
  { Sets up the default melodic instruments for all voices }
  Procedure SetMelodicInstruments;
end; { TAdlib Object }



   {-----------------------------------------------------------------}

(*************************************************************************)
(*  FUNCTION BOARDINSTALLED: BOOLEAN                                     *)
(*                                                                       *)
(*  Returns TRUE if an Adlib Compatible Sound Card is installed.         *)
(*************************************************************************)
FUNCTION BoardInstalled: Boolean;

(*************************************************************************)
(*  PROCEDURE SndOutput                                                  *)
(*   Writes to the specified Adlib Register a byte value.                *)
(*                                                                       *)
(*   in ->  Reg          Adlib register to write to                      *)
(*   in ->  Value        The value that the register will change to      *)
(*************************************************************************)
PROCEDURE SndOutput(Reg: Byte; Value: Byte);

(*************************************************************************)
(* PROCEDURE FastOutput                                                  *)
(*  Writes to the specified Adlib register, only if the registers does   *)
(*  not contain the wanted value. (Look-up table checking).              *)
(*                                                                       *)
(*   Reg -> Adlib Register to Write to.                                  *)
(*   Value -> Value to write to the register                             *)
(*************************************************************************)
PROCEDURE FastOutput(Reg: Byte; Value: Byte);





IMPLEMENTATION

(*************************************************************************)
(*                      PRIVATE DECLARATIONS                             *)
(*************************************************************************)
CONST
  { *** compare this note-tab to own one!!! *** }
  NoteTab : array[ 0 .. 126 ] of byte = (
    $00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0A,$0B,
    $00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0A,$0B,
    $10,$11,$12,$13,$14,$15,$16,$17,$18,$19,$1A,$1B,
    $20,$21,$22,$23,$24,$25,$26,$27,$28,$29,$2A,$2B,
    $30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3A,$3B,
    $40,$41,$42,$43,$44,$45,$46,$47,$48,$49,$4A,$4B,
    $50,$51,$52,$53,$54,$55,$56,$57,$58,$59,$5A,$5B,
    $60,$61,$62,$63,$64,$65,$66,$67,$69,$6A,$6B,
    $70,$71,$72,$73,$74,$75,$76,$77,$78,$79,$7A,$7B,
    $70,$71,$72,$73,$74,$75,$76,$77,$78,$79,$7A,$7B,
    $7B,$7B,$7B,$7B,$7B,$7B,$7B,$7B );

{ NoteTab:Array[0..126] of Byte = (
      $00,$01,$02,$03,$04,$05,
      $06,$07,$08,$09,$0a,$0b,

      $00,$01,$02,$03,$04,$05,
      $06,$07,$08,$09,$0a,$0b,
      $10,$11,$12,$13,$14,$15,
      $16,$17,$18,$19,$1a,$1b,
      Ord(' '),Ord('!'),Ord('"'),
      Ord('#'),Ord('$'),Ord('%'),
      Ord('&'), $27,     Ord('('),
      Ord(')'),Ord('*'),Ord('+'),
      Ord('0'),Ord('1'),Ord('2'),
      Ord('3'),Ord('4'),Ord('5'),
      Ord('6'),Ord('7'),Ord('8'),
      Ord('9'),Ord(':'),Ord(';'),
      Ord('@'),Ord('A'),Ord('B'),
      $43,$44,$45,$46,$47,$48,
      $49,$4a,$4b,$50,$51,$52,
      $53,$54,$55,$56,$57,$58,
      Ord('Y'),Ord('Z'),Ord('['),
      Ord('`'),Ord('a'),Ord('b'),
      Ord('c'),Ord('d'),Ord('e'),
      Ord('f'),Ord('g'),Ord('i'),
      Ord('j'),Ord('k'),Ord('p'),
      Ord('q'),Ord('r'),Ord('s'),
      Ord('t'),Ord('u'),
      Ord('v'),Ord('w'),Ord('x'),
      Ord('y'),Ord('z'),Ord('{'),
      Ord('p'),Ord('q'),Ord('r'),
      Ord('s'),Ord('t'),Ord('u'),
      Ord('v'),Ord('w'),Ord('x'),
      Ord('y'),Ord('z'),Ord('{'),
      Ord('{'),Ord('{'),Ord('{'),
      Ord('{'),Ord('{'),Ord('{'),
      Ord('{'),Ord('{')
      ); }
FreqTable:Array[0..767] of Byte = (
      Ord('W'),
      $01,$58,$01,$58,$01,$59,
      $01,$59,$01,$5a,$01,$5b,
      $01,$5b,$01,$5c,$01,$5d,
      $01,$5d,$01,$5e,$01,$5e,
      $01,$5f,$01,$60,$01,$60,
      $01,$61,$01,$62,$01,$62,
      $01,$63,$01,$64,$01,$64,
      $01,$65,$01,$65,$01,$66,
      $01,$67,$01,$67,$01,$68,
      $01,$69,$01,$69,$01,$6a,
      $01,$6b,$01,$6b,$01,$6c,
      $01,$6d,$01,$6d,$01,$6e,
      $01,$6f,$01,$6f,$01,$70,
      $01,$71,$01,$71,$01,$72,
      $01,$73,$01,$73,$01,$74,
      $01,$75,$01,$75,$01,$76,
      $01,$77,$01,$77,$01,$78,
      $01,$79,$01,$79,$01,$7a,
      $01,$7b,$01,$7b,$01,$7c,
      $01,$7d,$01,$7d,$01,$7e,
      $01,$7f,$01,$80,$01,$80,
      $01,$81,$01,$82,$01,$82,
      $01,$83,$01,$84,$01,$84,
      $01,$85,$01,$86,$01,$87,
      $01,$87,$01,$88,$01,$89,
      $01,$89,$01,$8a,$01,$8b,
      $01,$8b,$01,$8c,$01,$8d,
      $01,$8e,$01,$8e,$01,$8f,
      $01,$90,$01,$91,$01,$91,
      $01,$92,$01,$93,$01,$93,
      $01,$94,$01,$95,$01,$96,
      $01,$96,$01,$97,$01,$98,
      $01,$99,$01,$99,$01,$9a,
      $01,$9b,$01,$9c,$01,$9c,
      $01,$9d,$01,$9e,$01,$9e,
      $01,$09f,$01,$0a0,$01,$0a1,
      $01,$0a1,$01,$0a2,$01,$0a3,
      $01,$0a4,$01,$0a5,$01,$0a5,
      $01,$0a6,$01,$0a7,$01,$0a8,
      $01,$0a8,$01,$0a9,$01,$0aa,
      $01,$0ab,$01,$0ab,$01,$0ac,
      $01,$0ad,$01,$0ae,$01,$0ae,
      $01,$0af,$01,$0b0,$01,$0b1,
      $01,$0b2,$01,$0b2,$01,$0b3,
      $01,$0b4,$01,$0b5,$01,$0b6,
      $01,$0b6,$01,$0b7,$01,$0b8,
      $01,$0b9,$01,$0ba,$01,$0ba,
      $01,$0bb,$01,$0bc,$01,$0bd,
      $01,$0be,$01,$0be,$01,$0bf,
      $01,$0c0,$01,$0c1,$01,$0c2,
      $01,$0c2,$01,$0c3,$01,$0c4,
      $01,$0c5,$01,$0c6,$01,$0c6,
      $01,$0c7,$01,$0c8,$01,$0c9,
      $01,$0ca,$01,$0cb,$01,$0cb,
      $01,$0cc,$01,$0cd,$01,$0ce,
      $01,$0cf,$01,$0d0,$01,$0d0,
      $01,$0d1,$01,$0d2,$01,$0d3,
      $01,$0d4,$01,$0d5,$01,$0d5,
      $01,$0d6,$01,$0d7,$01,$0d8,
      $01,$0d9,$01,$0da,$01,$0db,
      $01,$0db,$01,$0dc,$01,$0dd,
      $01,$0de,$01,$0df,$01,$0e0,
      $01,$0e1,$01,$0e1,$01,$0e2,
      $01,$0e3,$01,$0e4,$01,$0e5,
      $01,$0e6,$01,$0e7,$01,$0e8,
      $01,$0e8,$01,$0e9,$01,$0ea,
      $01,$0eb,$01,$0ec,$01,$0ed,
      $01,$0ee,$01,$0ef,$01,$0f0,
      $01,$0f0,$01,$0f1,$01,$0f2,
      $01,$0f3,$01,$0f4,$01,$0f5,
      $01,$0f6,$01,$0f7,$01,$0f8,
      $01,$0f9,$01,$0fa,$01,$0fa,
      $01,$0fb,$01,$0fc,$01,$0fd,
      $01,$0fe,$01,$0ff,$01,$00,
      $02,$01,$02,$02,$02,$03,
      $02,$04,$02,$05,$02,$06,
      $02,$06,$02,$07,$02,$08,
      $02,$09,$02,$0a,$02,$0b,
      $02,$0c,$02,$0d,$02,$0e,
      $02,$0f,$02,$10,$02,$11,
      $02,$12,$02,$13,$02,$14,
      $02,$15,$02,$16,$02,$17,
      $02,$18,$02,$19,$02,$1a,
      $02,$1a,$02,$1b,$02,$1c,
      $02,$1d,$02,$1e,$02,$1f,
      $02,$20,$02,$21,$02,$22,
      $02,$23,$02,$24,$02,$25,
      $02,$26,$02,$27,$02,$28,
      $02,$29,$02,$2a,$02,$2b,
      $02,$2c,$02,$2d,$02,$2e,
      $02,$2f,$02,$30,$02,$31,
      $02,$32,$02,$33,$02,$34,
      $02,$35,$02,$36,$02,$37,
      $02,$38,$02,$39,$02,$3b,
      $02,$3c,$02,$3d,$02,$3e,
      $02,$3f,$02,$40,$02,$41,
      $02,$42,$02,$43,$02,$44,
      $02,$45,$02,$46,$02,$47,
      $02,$48,$02,$49,$02,$4a,
      $02,$4b,$02,$4c,$02,$4d,
      $02,$4e,$02,$4f,$02,$51,
      $02,$52,$02,$53,$02,$54,
      $02,$55,$02,$56,$02,$57,
      $02,$58,$02,$59,$02,$5a,
      $02,$5b,$02,$5c,$02,$5e,
      $02,$5f,$02,$60,$02,$61,
      $02,$62,$02,$63,$02,$64,
      $02,$65,$02,$66,$02,$67,
      $02,$69,$02,$6a,$02,$6b,
      $02,$6c,$02,$6d,$02,$6e,
      $02,$6f,$02,$70,$02,$72,
      $02,$73,$02,$74,$02,$75,
      $02,$76,$02,$77,$02,$78,
      $02,$79,$02,$7b,$02,$7c,
      $02,$7d,$02,$7e,$02,$7f,
      $02,$80,$02,$82,$02,$83,
      $02,$84,$02,$85,$02,$86,
      $02,$87,$02,$89,$02,$8a,
      $02,$8b,$02,$8c,$02,$8d,
      $02,$8e,$02,$90,$02,$91,
      $02,$92,$02,$93,$02,$94,
      $02,$96,$02,$97,$02,$98,
      $02,$99,$02,$9a,$02,$9c,
      $02,$9d,$02,$9e,$02,$09f,
      $02,$0a0,$02,$0a2,$02,$0a3,
      $02,$0a4,$02,$0a5,$02,$0a6,
      $02,$0a8,$02,$0a9,$02,$0aa,
      $02,$0ab,$02,$0ad,$02
      );

VAR
 { Adlib Register set Look up table }
 RegisterBypass: Array[0..255] of Byte;


{ Percussive voices instruments }
TYPE
 TInstrument = Record
  WaveChar: Byte;       { Wave Characteristic         }
  ScalingOutput: Byte;  { Scaling Level / Ouput level }
  AttackDecay: Byte;    { Attack / Decay Rate         }
  SustainRelease: Byte; { Sustain / Release Rate      }
  Feedback: Byte;       { Feedback                    }
  SetWaveSelect: Byte;  { Type of Wave to use         }
 end;

Const
  { ALL MELODIC VOICES ARE INITIALIZED TO THIS }
  ElectricPiano:array[0..1] of TInstrument = (
  (WaveChar: 65; ScalingOutput: 127; AttackDecay: 242; SustainRelease: 81;
   FeedBack: 7; SetWaveSelect: 1),
  (WaveChar: 19; ScalingOutput: 0; AttackDecay: 242; SustainRelease: 241;
   FeedBack: 0; SetWaveSelect: 0));

  { PERCUSSIVE MODE VOICES }

  BassDrum:array[0..1] of TInstrument = (
  (WaveChar: 0; ScalingOutput: 11; AttackDecay: 168; SustainRelease: 76;
   FeedBack: 1; SetWaveSelect: 1),
  (WaveChar: 0; ScalingOutput: 0; AttackDecay: 214; SustainRelease: 79;
   FeedBack: 1; SetWaveSelect: 0));

  InstrArray:Array[7..10] of TInstrument =
  (* SnareDrum Instrument *)
  ((WaveChar:12; ScalingOutput: 0; AttackDecay: 248; SustainRelease: 181;
   FeedBack: 0; SetWaveSelect: 0),
  (* TomTom Voice instrument *)
  (WaveChar: 4; ScalingOutput: 0; AttackDecay: 247; SustainRelease: 181;
   FeedBack: 0; SetWaveSelect: 0),
  (* Cymbal Voice Instrument *)
  (WaveChar: 1; ScalingOutput: 0; AttackDecay: 245; SustainRelease: 181;
   FeedBack: 0; SetWaveSelect: 0),
  (* hihat Voice Instrument *)
  (WaveChar: 1; ScalingOutput: 0; AttackDecay: 247; SustainRelease: 181;
   FeedBack: 0; SetWaveSelect: 0));



   {-----------------------------------------------------------------}

Function BoardInstalled:Boolean;
(* Returns TRUE if an adlib card was detected *)
(* otherwise returns false.                   *)
Var
 Stat1, Stat2: Integer;
Begin
 SndOutput($04,$60);      (* Reset both timers       *)
 SndOutput($04,$80);      (* Enable Timer interrupts *)
 Stat1:=Port[$388];       (* Read Status Register    *)
 SndOutput($02,$FF);
 SndOutput($04,$21);      (* Start Timer 21h         *)
 Delay(100);              (* Wait 80 ms              *)
 Stat2:=Port[$388];       (* Read Status Register    *)
 SndOutput($04,$60);      (* Reset both timers       *)
 SndOutput($04,$80);      (* Enable timer interrupts *)
 Stat1:=stat1 and $E0;
 Stat2:=Stat2 and $E0;
 If (stat1 = $00) and (stat2 = $C0) then
   BoardInstalled:=True;
end;




Procedure SndOutput(reg: Byte; Value: Byte);
 (****************************************************************)
 (* * PROCEDURE SndOutPut(reg: Integer; Value: Byte);          * *)
 (* *  Writes to a specified Adlib register.                   * *)
 (* *  reg -> register number to write to                      * *)
 (* *  value -> value to write to register                     * *)
 (****************************************************************)
Begin
   RegisterBypass[Reg] := Value;
   Asm
    mov  dx, 388h                  { Adlib index port }
    mov  al, [Reg]

    out  dx,al                     { Set the index }

    { Wait For hardware to respond }
    in al, dx; in al, dx; in al, dx
    in al, dx; in al, dx; in al, dx

    inc  dx                        { Adlib register port }
    mov  al, [Value]
    out  dx, al                    { Set the register value }

    dec  dx                        { Adlib index port }

    { Wait For hardware to respond }
    in al, dx; in al, dx; in al, dx; in al, dx; in al, dx
    in al, dx; in al, dx; in al, dx; in al, dx; in al, dx
    in al, dx; in al, dx; in al, dx; in al, dx; in al, dx
    in al, dx; in al, dx; in al, dx; in al, dx; in al, dx
    in al, dx; in al, dx; in al, dx; in al, dx; in al, dx
    in al, dx; in al, dx; in al, dx; in al, dx; in al, dx
    in al, dx; in al, dx; in al, dx; in al, dx; in al, dx
  end;
end;





Procedure FastOutput(Reg: Byte; Value: Byte);
Begin
 If RegisterByPass[Reg] = Value then
  Exit
 else
   RegisterByPass[Reg] := Value;
 ASM
    mov  dx, 388h                  { Adlib index port }
    mov  al, [Reg]

    out  dx,al                     { Set the index }

    { Wait For hardware to respond }
    in al, dx; in al, dx; in al, dx
    in al, dx; in al, dx; in al, dx


    inc  dx                        { Adlib register port }
    mov  al, [Value]
    out  dx, al                    { Set the register value }


    { Wait For hardware to respond }
    in al, dx; in al, dx; in al, dx; in al, dx; in al, dx
    in al, dx; in al, dx; in al, dx; in al, dx; in al, dx
    in al, dx; in al, dx; in al, dx; in al, dx; in al, dx
    in al, dx; in al, dx; in al, dx; in al, dx; in al, dx
    in al, dx; in al, dx; in al, dx; in al, dx; in al, dx
    in al, dx; in al, dx; in al, dx; in al, dx; in al, dx
    in al, dx; in al, dx; in al, dx; in al, dx; in al, dx
 end;
end;


   {-----------------------------------------------------------------}



Constructor TAdlib.Init;
Var
 i:Byte;
Begin
   Inherited Init;
   (* If an Adlib compatible sound card is NOT installed *)
   If Not BoardInstalled then
     CardNotInstalled;
   (* Reset the Sound Card *)
   FOR i := 1 TO 245 do
      SndOutput(i, $00);

  { Let US decide which waveforms we wish to have }
   SndOutput($01, $20);
  { Put Key-Scaling on }
   SndOutput($08, $40);

   { Select SINUS Waves for Output }
   For i:=0 to 8 do
   Begin
    SndOutPut($E0 + SlotMVoice[i][Modulator], $00);
    SndOutput($E0 + SlotMVoice[i][Carrier], $00);
   end;
   { Also sets the correct instruments }
   SetPercussive(FALSE);
END;




Procedure TAdlib.CardNotInstalled;
Begin
 WriteLn('Adlib card not found. Halting program');
 WriteLn('Carte Adlib non trouv‚e. Arrˆt du programme.');
 Halt(1);
end;




{ Start a note playing.                         }
{-----------------------------------------------------------
  Routine to start a note playing.

  0 <= voice <= 8 in melodic mode,
  0 <= voice <= 10 in percussive mode;
  0 <= pitch <= 127, 60 == MID_C ( the card can play between 12 and 107 )
-----------------------------------------------------------
}

Procedure TAdlib.NoteOn(Voice: Byte; Note: Byte);
Begin

 { Do a note off if note is already playing }
{ If PlayingNote[Voice] <> 0 then
 Begin
  SndOutput($A0 + Voice, Lo(Output[Voice]));
  SndOutput($B0 + Voice, Hi(Output[Voice]));
 end; }

 If Note > 107 then
  Note := 107;
 If Note < 12 then
  Note := 12;
 PlayingNote[Voice] := Note;

 { Melodic voice }
 If ((Percussive) and (Voice < BassDrumVoice)) OR (NOT Percussive) then
 Begin
   Output[Voice] := CalcNote(Voice, Note);
   FastOutput($A0 + Voice, Lo(Output[Voice]));
   { Enable key on by adding bitwise bit 5 }
   FastOutput($B0 + Voice, Hi(Output[Voice]) OR $20);
 end
 else
 Begin
 { Percussive Voice }
   BDRegister := BDRegister OR PercBits[Voice-6];
   Output[Voice] := CalcNote(Voice, Note);
   SndOutput($A0 + PercussiveOffset[Voice], Lo(Output[Voice]));
   SndOutput($B0 + PercussiveOffset[Voice], Hi(Output[Voice]));

   { Enable Key on in Rhythm Node }
   SndOutput($BD, BDRegister);
 end;
END;



Procedure TAdlib.NoteOff(Voice:Byte; Note:Byte);
Begin
 If PlayingNote[Voice] = 0 then Exit;
 { Melodic Voice }
 If ((Percussive) and (Voice < BassDrumVoice)) OR (NOT Percussive) then
 Begin
   PlayingNote[Voice] := 0;
   OutPut[Voice] := CalcNote(Voice, Note);
   { We just need to rewrite to $B0 register }
   FastOutput($A0 + Voice, Lo(Output[Voice]));
   SndOutput($B0 + Voice, Hi(Output[Voice]));
 end
 else
 { Percussive Voice }
 Begin
   BDRegister := BDRegister AND (NOT Percbits[Voice - 6]);
   PlayingNote[Voice] := 0;
   { Disable thee key-on for that percussive voice }
   SndOutPut($BD, BDRegister);
 end;
end;




{ Put the chip in melodic mode (mode == 0), or in percussive mode (mode <> 0).}
{ If the melodic mode is chosen, all voices are set to electric-piano, else   }
{ the first 5 are set to electric-piano, and the percussion voices to their   }
{ default timbres.                                                            }
Procedure TAdlib.SetPercussive(Mode: Boolean);
BEGIN
 If NOT Mode then
 Begin
   Voices := $08;
   BdRegister := $C0;
   SndOutput($Bd, BDRegister);
   Percussive := FALSE;
   SetMelodicInstruments;
 end
 else
 Begin
   Voices := $05;
   BDRegister := $E0;
   SndOutput($Bd, BDRegister);
   Percussive := TRUE;
   SetRhythmInstruments;
 end;
END;


Procedure TAdlib.SetRhythmInstruments;
Var
 i: Byte;
Begin
 { BASS-DRUM Voice has two operators }
 CarrierScalingLevel[BassDrumVoice] := BassDrum[Carrier].ScalingOutput
     AND $C0;
 ModulatorScalingLevel[BassDrumVoice] := BassDrum[Modulator].ScalingOutput
     AND $C0;
 Algorithm[BassDrumVoice] := Boolean(BassDrum[Modulator].FeedBack AND $01);
 Volume[BassDrumVoice] := BassDrum[Carrier].ScalingOutput AND $3f;
 Volume[BassDrumVoice] := Byte(Volume[BassDrumVoice] - $3F);
 Volume[BassDrumVoice] := Byte(0 - Volume[BassDrumVoice]);
 SndOutput(TablePModulator[BassDrumVoice] + $20,
  BassDrum[Modulator].WaveChar);
 SndOutput(TablePModulator[BassDrumVoice] + $40,
  BassDrum[Modulator].ScalingOutput);
 SndOutput(TablePModulator[BassDrumVoice] + $60,
  BassDrum[Modulator].AttackDecay);
 SndOutput(TablePModulator[BassDrumVoice] + $80,
  BassDrum[Modulator].SustainRelease);
 SndOutput( BassDrumVoice                 + $C0,
  BassDrum[Modulator].Feedback);
 SndOutput(TablePModulator[BassDrumVoice] + $E0,
  BassDrum[Modulator].SetWaveSelect);

 SndOutput(TablePModulator[BassDrumVoice] + $23,
  BassDrum[Carrier].WaveChar);
 SndOutput(TablePModulator[BassDrumVoice] + $43,
  BassDrum[Carrier].ScalingOutput);
 SndOutput(TablePModulator[BassDrumVoice] + $63,
  BassDrum[Carrier].AttackDecay);
 SndOutput(TablePModulator[BassDrumVoice] + $83,
  BassDrum[Carrier].SustainRelease);
 SndOutput(TablePModulator[BassDrumVoice] + $E3,
  BassDrum[Carrier].SetWaveSelect);


 { The other percussive voices only have one operator }
 For i:=SNAREDRUMVOICE to HIHATVOICE do
 Begin
  CarrierScalingLevel[i] := InstrArray[i].ScalingOutput AND $C0;
  ModulatorScalingLevel[i] := InstrArray[i].ScalingOutput AND $C0;
  Algorithm[i] := Boolean(InstrArray[i].FeedBack AND $01);
  Volume[i] := InstrArray[i].ScalingOutput AND $3f;
  Volume[i] := Byte(Volume[i] - $3F);
  Volume[i] := Byte(0 - Volume[i]);

  SndOutput(TablePModulator[i] + $20, InstrArray[i].WaveChar);
  SndOutput(TablePModulator[i] + $40, InstrArray[i].ScalingOutput);
  SndOutput(TablePModulator[i] + $60, InstrArray[i].AttackDecay);
  SndOutput(TablePModulator[i] + $80, InstrArray[i].SustainRelease);
  SndOutput(i + $C0, InstrArray[i].Feedback);
  SndOutput(TablePModulator[i] + $E0, InstrArray[i].SetWaveSelect);
 end;
end;





Procedure TAdlib.SetMelodicInstruments;
Var
 Voice: Byte;
 Index: Byte;
 Select: Byte;
Begin
 (* Optimized greatly for size this routine, everything is now in a *)
 (* tight loop.                                                     *)
 For Voice:=0 to VOICES do
 Begin
  For Select := MODULATOR to CARRIER do
  Begin
     If Select = 0 then Index := 0 else Index := 3;
     SndOutput(TableModulator[Voice] + $20+Index,
   ElectricPiano[Select].WaveChar);

     SndOutput(TableModulator[Voice] + $40+Index,
  ElectricPiano[Select].ScalingOutput);

     SndOutput(TableModulator[Voice] + $60+Index,
  ElectricPiano[Select].AttackDecay);

     SndOutput(TableModulator[Voice] + $80+Index,
  ElectricPiano[Select].SustainRelease);

     SndOutput(TableModulator[Voice] + $E0,
  ElectricPiano[Select].SetWaveSelect);
   end;
  SndOutput($C0 + Voice, ElectricPiano[Modulator].Feedback);
 { Put the volume in the volume registers so we can thereafter change them }
  CarrierScalingLevel[Voice] := ElectricPiano[Carrier].ScalingOutput AND $C0;
  ModulatorScalingLevel[Voice] := ElectricPiano[Modulator].ScalingOutput
       AND $C0;
  Algorithm[Voice] := Boolean(ElectricPiano[Modulator].FeedBack AND $01);
  Volume[Voice] := ElectricPiano[Carrier].ScalingOutput AND $3f;
  Volume[Voice] := Byte(Volume[Voice] - $3F);
  Volume[Voice] := Byte(0 - Volume[Voice]);
 end;
end;





Function Tadlib.CalcNote(Voice:byte; Note: Byte): Word; Assembler;
(* Calculate the note to output to A0 and B0 registers *)
(* Taken directly from the SBlaster FM Driver          *)
(* Possible value is from 0 to 127                     *)
ASM
    Xor     Ax, Ax
    Xor     Bx, Bx
    LES     SI, [Self]

    Mov     Bl, [Voice]
    Mov     Al,Byte Ptr [Note]
    Cbw
    Xor     Di, Di
    Add     Di,Ax
    Jns     @b06c12
    Sub     Di,Di
@b06c12:        And     Di,7fh
    Mov     Al,Byte Ptr NoteTab[Di]
    Mov     Dl,Al
    And     Dl,70h
    Shr     Dl,1
    Shr     Dl,1
    And     Al,0fh
    Cbw
    Xchg    Al,Ah
    Shr     Ax,1
    Shr     Ax,1
    Shr     Ax,1
    Shl     Bx,1
    Add     Ax,Word ptr ES:[SI].SemiToneCurrent[Bx]
    Jns     @b06c43
    Add     Ax,0180h
    Sub     Dl,04
    Jns     @b06c58
    Sub     Dl,Dl
    Sub     Ax,Ax
    Jmp     @b06c58
@b06c43:        Cmp     Ax,0180h
    Jb      @b06c58
    Sub     Ax,0180h
    Add     Dl,04
    Cmp     Dl,1ch
    Jnb     @b06c58
    Mov     Ax,017fh
    Mov     Dl,1ch
@b06c58:        Shl     Ax,1
    Mov     Di,Ax
    Mov     Ax,Word Ptr FreqTable[Di]
    Or      Ah,Dl
end;


Procedure TAdlib.SetVolume(Voice: Byte; AVolume: Byte);
(*************************************************************************)
(*  SETS THE REAL VOLUME OF A VOICE, DEPENDING ON THE CHOSEN             *)
(*  ALGORITHM, WILL WRITE EITHER BOTH OR ONLY ONE OPERATOR.              *)
(*                                                                       *)
(*  AVolume -> Desired Volume (0-127 where 127 is highest)               *)
(*************************************************************************)
{ INFORMATION: From Adlib docs, if the algorithm does     support }
{ additive synthesis (i.e Alg = 0), the volume is modified by the }
{ carrier output level only.                                      }
{ On the other hand, if the Alg = 1, each operator produces a     }
{ sound directly, in that case, BOTH operators must be used to    }
{ change the output level.                                        }
Var
 ValueOne: Byte;  { Modulator modification }
 ValueTwo: Byte;  { Carrier Modification   }
Begin
 ASM
  Les     Si, [Self]
  Xor     Ax, Ax
  Xor     Bx, Bx
  Xor     Cx, Cx
  Mov     Bl, [Voice]
  Mov     Cl, [Avolume]
  Or      Cl,80h
  Mov     Al,Byte ptr ES:[SI].Volume[Bx]
  Mul     Cl
  Mov     Al,3fh
  Sub     Al,Ah
  ; { Change the volume level.}
  { Bugfix - save volume in CL }
  Mov     CL, AL
  Mov     AH,Byte Ptr Es:[Si].CarrierScalingLevel[Bx]
  Or      AL, AH
  mov     [ValueOne], Al
  Mov     AH,Byte Ptr Es:[Si].ModulatorScalingLevel[Bx]
        { We or with CL instead of AL }
  Or      CL, AH
  mov     [ValueTwo], CL
 end;
  { If Melodic mode, or if the voice number is a melodic voice }
  { then output volume level normally.                         }
  If (Not Percussive) OR ((Voice < BassDrumVoice) AND Percussive) then
  Begin
     SndOutput(TableModulator[Voice] + $43, ValueOne);
     { If the sound is produced directly by both operators }
     If Algorithm[Voice] then
  SndOutput(TableModulator[Voice] + $40, ValueTwo);
  end
  else
  Begin
   { This Voice has two operators in Percussive mode, write to }
   { normal operator.                                          }
   If Voice = BassDrumVoice then
   Begin
     SndOutput(TablePModulator[Voice] + $43, ValueOne);
   end
   else
     { These percussive voices only have one operator each, write }
     { the volume level directly.                                 }
     SndOutput(TablePModulator[Voice] + $40, ValueOne);
  end;
end;


Procedure TAdlib.SemiToneup(Voice: Byte; Value: Shortint);
{ Number of Semi-tones to increase the notes by }
Begin
  ASM
      Les     Di, [Self]
      Mov     Ah, [Value]
      Mov     Al,Ah
      Cbw
      Sar     Ax,1
      Sar     Ax,1
      Xor     Bh, Bh
      Mov     Bl,[Voice]
      Shl     Bx,1
      Mov     Word Ptr ES:[DI].SemiToneCurrent[Bx],Ax
 end;
 { Retrigger note if it is already playing }
 If (PlayingNote[Voice] <> 0) AND (NOT Percussive) then
 Begin
  Output[Voice] := CalcNote(Voice, PlayingNote[Voice]);
  SndOutput($A0 + Voice, Lo(Output[Voice]));
   { Enable key on by adding bitwise bit 5 }
  SndOutput($B0 + Voice, hi(Output[Voice]) OR $20);
 end;
end;


Procedure TAdlib.SemiToneDown(Voice: Byte; Value: Shortint);
{ Number of Semi-tones to decrease the notes by }
Begin
    ASM
      Les     Di, [Self]
      Mov     Ah, [Value]
      Neg     Ah
      Mov     Al,Ah
      Cbw
      Sar     Ax,1
      Sar     Ax,1
      Xor     Bh, Bh
      Mov     Bl,[Voice]
      Shl     Bx,1
      Mov     Word Ptr ES:[DI].SemiToneCurrent[Bx],Ax
   end;
 { Retrigger note if it is already playing }
 If (PlayingNote[Voice] <> 0) AND (NOT Percussive) then
 Begin
  Output[Voice] := CalcNote(Voice, PlayingNote[Voice]);
  SndOutput($A0 + Voice, Lo(Output[Voice]));
   { Enable key on by adding bitwise bit 5 }
  SndOutput($B0 + Voice, hi(Output[Voice]) OR $20);
 end;
end;







 (****************************************************************)
(* $BD REGISTER FUNCTIONS *)

{
'Write to the register at &HBD which controls AM Depth, VIB depth, rhythm
'(melo/perc mode) and note on/off for the percussive voices.
'}
(* SETS BIT 7 - AM depth *)
Procedure TAdlib.SetAM(AM: Boolean);
Begin
 If AM then
   BDRegister := BDRegister OR $80
 else
   BDRegister := BDRegister AND $7F;
 SndOutPut($BD, BdRegister);
end;

Procedure TAdlib.SetVibrato(Vib: Boolean);
Begin
 If Vib then
 (* Enable bit 6 *)
  BDRegister := BDRegister OR $40
 else
  BDRegister := BDRegister AND $BF;
(* Clear Cport.Depth bit 6 *)
 SndOutPut($BD, BDRegister);
end;




 (****************************************************************)
(* REGISTERS $60-$75 ->  Attack Rate/ Decay Rate Change                         *)


Procedure TAdlib.AllAttackDecay(Voice: Byte; ModCar: TBit; Value: Byte);
Begin
 SndOutput($60+SlotMVoice[Voice][ModCar], Value);
end;
 (****************************************************************)


 (****************************************************************)
(* AM / Vibrato / EG (Sustain) / Keyboard Scaling Rate (KSR) / Mod. multiplicator *)
Procedure TAdlib.SetWaveChar(Voice: Byte; ModCar: TBit; Value: Byte);
Begin
 SndOutPut($20+SlotMVoice[Voice][ModCar], Value);
end;





Procedure TAdlib.AllScalingOutput(Voice: Byte; ModCar: TBit; Value: Byte);
Begin
 SndOutput($40+ SlotMVoice[Voice][ModCar], Value);
end;




 (****************************************************************)
(*  BYTES $C0-$C8 = Feedback/ Algorithm                                         *)

{'Write to the register at &HC0 which controls FEED-BACK and FM (connection).
'Applicable only to operator 0.
'}

Procedure TAdlib.AllFeedback(Voice: Byte; Value: Byte);
Begin
 SndOutput($C0 + Voice, Value);
end;
 (****************************************************************)

 (****************************************************************)
(* BYTES $80-95 SUSTAIN/ RELEASE RATES                                          *)

{'Write to the register at &H80 which controls the sustain level and
'release rate.
'}

Procedure TAdlib.AllSusRelease(Voice: Byte; ModCar: TBit; Value: Byte);
Begin
 SndOutput($80+SlotMVoice[Voice][ModCar], Value);
end;
 (****************************************************************)


 (****************************************************************)
{'Write to the register at &HE0 which controls the type of wave-form which
'will be used for this slot.
'}
Procedure TAdlib.SetWaveSelect(Voice:Byte;ModCar: TBit; Value: Byte);
Begin
  { BUGFIX -> Check for zero value first! }
  { ????????????????????????????????????? }
  SndOutput($E0 + SlotMvoice[Voice][ModCar], Value);
END;


Procedure TAdlib.MuteVoice(Voice: Byte);
Begin
 SndOutput($60+SlotMVoice[Voice][CARRIER], $FF);
 SndOutput($60+SlotMVoice[Voice][MODULATOR], $FF);
 SndOutput($80+SlotMVoice[Voice][CARRIER], $FF);
 Sndoutput($80+SlotMVoice[Voice][MODULATOR],$FF);
 NoteOff(Voice, PlayingNote[Voice]);
 SetVolume(Voice, 0);
end;



Destructor TAdlib.Done;
Var
 i: Word;
Begin
   (* Reset\Silence the Sound Card *)
   { This could be replaced by a gradual scale down }
   FOR i := $01 TO $F5 do
      SndOutput(i, 0);
   Inherited Done;
end;



end.
