module FsAutoComplete.CodeFix.AdjustConstant

open FsAutoComplete.CodeFix.Types
open Ionide.LanguageServerProtocol.Types

[<RequireQualifiedAccess>]
type CharFormat =
  /// `ç`
  | Char
  /// `\231`
  | Decimal
  /// `\xE7`
  | Hexadecimal
  /// `\u00E7`
  | Utf16Hexadecimal
  /// `\U000000E7`
  | Utf32Hexadecimal

[<RequireQualifiedAccess>]
type Base =
  /// No prefix
  | Decimal
  /// `0x`
  | Hexadecimal
  /// `0o`
  | Octal
  /// `0b`
  | Binary

module Title =
  val removeDigitSeparators: string
  val replaceWith: (string -> string)

  module Int =
    module Convert =
      val toDecimal: string
      val toHexadecimal: string
      val toOctal: string
      val toBinary: string

      module SpecialCase =
        val extractMinusFromNegativeConstant: string
        val integrateExplicitMinus: string
        val useImplicitPlusInPositiveConstantWithMinusSign: string
        val removeExplicitMinusWithMinValue: string

    module Separate =
      val decimal3: string
      val hexadecimal4: string
      val hexadecimal2: string
      val octal3: string
      val binary4: string
      val binary8: string

  module Float =
    module Separate =
      val all3: string

  module Char =
    module Convert =
      val toChar: (string -> string)
      val toDecimal: (string -> string)
      val toHexadecimal: (string -> string)
      val toUtf16Hexadecimal: (string -> string)
      val toUtf32Hexadecimal: (string -> string)

val fix:
  getParseResultsForFile: GetParseResultsForFile ->
  codeActionParams: CodeActionParams ->
    Async<Result<Fix list, string>>
