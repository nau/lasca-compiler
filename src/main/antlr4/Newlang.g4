/*
 [The "BSD licence"]
 Copyright (c) 2014 Leonardo Lucena
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:
 1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.
 3. The name of the author may not be used to endorse or promote products
    derived from this software without specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
/*
   Derived from http://www.scala-lang.org/files/archive/spec/2.11/13-syntax-summary.html
 */

grammar Newlang;


compilationUnit
   : 'def' 'test' EOF
   ;

// Lexer

BooleanLiteral
   : 'true' | 'false'
   ;


CharacterLiteral
   : '\'' (PrintableChar | CharEscapeSeq) '\''
   ;


StringLiteral
   : '"' StringElement* '"' | '"""' MultiLineChars '"""'
   ;


IntegerLiteral
   : (DecimalNumeral | HexNumeral) ('L' | 'l')
   ;


FloatingPointLiteral
   : Digit + '.' Digit + ExponentPart? FloatType? | '.' Digit + ExponentPart? FloatType? | Digit ExponentPart FloatType? | Digit + ExponentPart? FloatType
   ;


Id
   : Plainid | '`' StringLiteral '`'
   ;


Varid
   : Lower Idrest
   ;


WS
   : [ \r\n\t] -> skip
   ;


Semi
   : ';'
   ;


Paren
   : '(' | ')' | '[' | ']' | '{' | '}'
   ;


Delim
   : '`' | '\'' | '"' | '.' | ';' | ','
   ;


Comment
   : '/*' .*? '*/' | '//' .*?
   ;

// fragments

fragment UnicodeEscape
   : '\\' 'u' 'u'? HexDigit HexDigit HexDigit HexDigit
   ;


fragment WhiteSpace
   : '\u0020' | '\u0009' | '\u000D' | '\u000A'
   ;


fragment Opchar
   : PrintableChar
   ;


fragment Op
   : Opchar +
   ;


fragment Plainid
   : Upper Idrest | Varid | Op
   ;


fragment Idrest
   : (Letter | Digit)* ('_' Op)?
   ;


fragment StringElement
   : '\u0020' | '\u0021' | '\u0023' .. '\u007F' | CharEscapeSeq
   ;


fragment MultiLineChars
   : ('"'? '"'? .*?)* '"'*
   ;


fragment HexDigit
   : '0' .. '9' | 'A' .. 'F' | 'a' .. 'f'
   ;


fragment FloatType
   : 'F' | 'f' | 'D' | 'd'
   ;


fragment Upper
   : 'A' .. 'Z' | '$' | '_'
   ;

// and Unicode category Lu

fragment Lower
   : 'a' .. 'z'
   ;

// and Unicode category Ll

fragment Letter
   : Upper | Lower
   ;

// and Unicode categories Lo, Lt, Nl

fragment ExponentPart
   : ('E' | 'e') ('+' | '-')? Digit +
   ;


fragment PrintableChar
   : '\u0020' .. '\u007F'
   ;


fragment CharEscapeSeq
   : '\\' ('b' | 't' | 'n' | 'f' | 'r' | '"' | '\'' | '\\')
   ;


fragment DecimalNumeral
   : '0' | NonZeroDigit Digit*
   ;


fragment HexNumeral
   : '0' 'x' HexDigit HexDigit +
   ;


fragment Digit
   : '0' | NonZeroDigit
   ;


fragment NonZeroDigit
   : '1' .. '9'
   ;
