(* ::Package:: *)

(* ========================================================================= *)
(*  The helpers shared by the test files: the counters, check, checkZero,    *)
(*  section, and the checks of messages. Read once, by RunTests.wl or by     *)
(*  EffectiveActions.wl or Superfields.wl run alone; the files read by       *)
(*  RunTests.wl clear every other Global symbol (harnessNames are kept).     *)
(* ========================================================================= *)

$IterationLimit = 10^8;
passed = 0; failed = 0; failures = {};
startTime = AbsoluteTime[];

check[name_String, value_, expected_] := If[value === expected,
   passed++; Print["PASS ", name],
   failed++; AppendTo[failures, name]; Print["FAIL ", name, "\n     got:      ", InputForm[value], "\n     expected: ", InputForm[expected]]];
checkZero[name_String, expr_] := check[name, SimplifyDiagrams[expr], 0];
section[s_String] := Print["\n---- ", s, "   (", Round[AbsoluteTime[] - startTime], " s)"];

(* messages expected by a check are caught, not printed (HoldAll: the message name must not evaluate to its text) *)
SetAttributes[{messageGivenQ, messageAndValue, checkMessage, quietCheck}, HoldAll];
(* whether the message m is given *)
messageGivenQ[expr_, m_] := Quiet[Check[expr; False, True, m], m];
(* whether the message m is given, and the value *)
messageAndValue[e_, m_] := Module[{given = False, value}, Quiet[Check[value = e, given = True, m], m]; {given, value}];
(* the message m is given, and the value is the expected one *)
checkMessage[e_, name_String, m_, expected_] := check[name, messageAndValue[e, m], {True, expected}];
(* Check[e, failexpr] for any message, without printing it *)
quietCheck[e_, failexpr_] := Quiet[Check[e, failexpr]];

harnessNames = {"passed", "failed", "failures", "startTime", "check", "checkZero", "section", "messageGivenQ",
   "messageAndValue", "checkMessage", "quietCheck", "harnessNames", "testDirectory"};
