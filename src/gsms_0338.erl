%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%    GSM 03.38 codec
%%% @end
%%% Created : 16 Oct 2012 by Tony Rogvall <tony@rogvall.se>

-module(gsms_0338).

-export([decode/1, 
	 decode_char/1,
	 decode_xchar/1]).

decode(Bin) when is_binary(Bin) ->
    decode_list(binary_to_list(Bin));
decode(Cs) when is_list(Cs) ->
    decode_list(Cs).

decode_list([C|Cs]) ->
    case decode_char(C) of
	extension -> decode_xlist(Cs);
	C1 -> [C1|decode_list(Cs)]
    end;
decode_list([]) ->
    [].

decode_xlist([C|Cs]) ->
    C1 = decode_xchar(C),
    [C1|decode_list(Cs)];
decode_xlist([]) -> %% not correct but...
    [16#A0].

    
decode_char(C) ->
    case C band 16#7f of
	16#00 -> 16#0040;	%%	COMMERCIAL AT
	%%16#00  16#0000  %%  NULL
	16#01 -> 16#00A3; %%  POUND SIGN
	16#02 -> 16#0024; %%  DOLLAR SIGN
	16#03 -> 16#00A5; %%  YEN SIGN
	16#04 -> 16#00E8; %%  LATIN SMALL LETTER E WITH GRAVE
	16#05 -> 16#00E9; %%  LATIN SMALL LETTER E WITH ACUTE
	16#06 -> 16#00F9; %%  LATIN SMALL LETTER U WITH GRAVE
	16#07 -> 16#00EC; %%  LATIN SMALL LETTER I WITH GRAVE
	16#08 -> 16#00F2; %%  LATIN SMALL LETTER O WITH GRAVE
	16#09 -> 16#00E7; %%  LATIN SMALL LETTER C WITH CEDILLA
	%%16#09  16#00C7  %%  LATIN CAPITAL LETTER C WITH CEDILLA
	16#0A -> 16#000A; %%  LINE FEED
	16#0B -> 16#00D8; %%  LATIN CAPITAL LETTER O WITH STROKE
	16#0C -> 16#00F8; %%  LATIN SMALL LETTER O WITH STROKE
	16#0D -> 16#000D; %%  CARRIAGE RETURN
	16#0E -> 16#00C5; %%  LATIN CAPITAL LETTER A WITH RING ABOVE
	16#0F -> 16#00E5; %%  LATIN SMALL LETTER A WITH RING ABOVE
	16#10 -> 16#0394; %%  GREEK CAPITAL LETTER DELTA
	16#11 -> 16#005F; %%  LOW LINE
	16#12 -> 16#03A6; %%  GREEK CAPITAL LETTER PHI
	16#13 -> 16#0393; %%  GREEK CAPITAL LETTER GAMMA
	16#14 -> 16#039B; %%  GREEK CAPITAL LETTER LAMDA
	16#15 -> 16#03A9; %%  GREEK CAPITAL LETTER OMEGA
	16#16 -> 16#03A0; %%  GREEK CAPITAL LETTER PI
	16#17 -> 16#03A8; %%  GREEK CAPITAL LETTER PSI
	16#18 -> 16#03A3; %%  GREEK CAPITAL LETTER SIGMA
	16#19 -> 16#0398; %%  GREEK CAPITAL LETTER THETA
	16#1A -> 16#039E; %%  GREEK CAPITAL LETTER XI
	16#1B -> extension; %%  ESCAPE TO EXTENSION TABLE
	16#1C -> 16#00C6; %%  LATIN CAPITAL LETTER AE
	16#1D -> 16#00E6; %%  LATIN SMALL LETTER AE
	16#1E -> 16#00DF; %%  LATIN SMALL LETTER SHARP S (German)
	16#1F -> 16#00C9; %%  LATIN CAPITAL LETTER E WITH ACUTE
	16#20 -> 16#0020; %%  SPACE
	16#21 -> 16#0021; %%  EXCLAMATION MARK
	16#22 -> 16#0022; %%  QUOTATION MARK
	16#23 -> 16#0023; %%  NUMBER SIGN
	16#24 -> 16#00A4; %%  CURRENCY SIGN
	16#25 -> 16#0025; %%  PERCENT SIGN
	16#26 -> 16#0026; %%  AMPERSAND
	16#27 -> 16#0027; %%  APOSTROPHE
	16#28 -> 16#0028; %%  LEFT PARENTHESIS
	16#29 -> 16#0029; %%  RIGHT PARENTHESIS
	16#2A -> 16#002A; %%  ASTERISK
	16#2B -> 16#002B; %%  PLUS SIGN
	16#2C -> 16#002C; %%  COMMA
	16#2D -> 16#002D; %%  HYPHEN-MINUS
	16#2E -> 16#002E; %%  FULL STOP
	16#2F -> 16#002F; %%  SOLIDUS
	16#30 -> 16#0030; %%  DIGIT ZERO
	16#31 -> 16#0031; %%  DIGIT ONE
	16#32 -> 16#0032; %%  DIGIT TWO
	16#33 -> 16#0033; %%  DIGIT THREE
	16#34 -> 16#0034; %%  DIGIT FOUR
	16#35 -> 16#0035; %%  DIGIT FIVE
	16#36 -> 16#0036; %%  DIGIT SIX
	16#37 -> 16#0037; %%  DIGIT SEVEN
	16#38 -> 16#0038; %%  DIGIT EIGHT
	16#39 -> 16#0039; %%  DIGIT NINE
	16#3A -> 16#003A; %%  COLON
	16#3B -> 16#003B; %%  SEMICOLON
	16#3C -> 16#003C; %%  LESS-THAN SIGN
	16#3D -> 16#003D; %%  EQUALS SIGN
	16#3E -> 16#003E; %%  GREATER-THAN SIGN
	16#3F -> 16#003F; %%  QUESTION MARK
	16#40 -> 16#00A1; %%  INVERTED EXCLAMATION MARK
	16#41 -> 16#0041; %%  LATIN CAPITAL LETTER A
	%%16#41  16#0391  %%  GREEK CAPITAL LETTER ALPHA
	16#42 -> 16#0042; %%  LATIN CAPITAL LETTER B
	%%16#42  16#0392  %%  GREEK CAPITAL LETTER BETA
	16#43 -> 16#0043; %%  LATIN CAPITAL LETTER C
	16#44 -> 16#0044; %%  LATIN CAPITAL LETTER D
	16#45 -> 16#0045; %%  LATIN CAPITAL LETTER E
	%%16#45  16#0395  %%  GREEK CAPITAL LETTER EPSILON
	16#46 -> 16#0046; %%  LATIN CAPITAL LETTER F
	16#47 -> 16#0047; %%  LATIN CAPITAL LETTER G
	16#48 -> 16#0048; %%  LATIN CAPITAL LETTER H
	%%16#48  16#0397  %%  GREEK CAPITAL LETTER ETA
	16#49 -> 16#0049; %%  LATIN CAPITAL LETTER I
	%%16#49  16#0399  %%  GREEK CAPITAL LETTER IOTA
	16#4A -> 16#004A; %%  LATIN CAPITAL LETTER J
	16#4B -> 16#004B; %%  LATIN CAPITAL LETTER K
	%%16#4B  16#039A  %%  GREEK CAPITAL LETTER KAPPA
	16#4C -> 16#004C; %%  LATIN CAPITAL LETTER L
	16#4D -> 16#004D; %%  LATIN CAPITAL LETTER M
	%%16#4D  16#039C  %%  GREEK CAPITAL LETTER MU
	16#4E -> 16#004E; %%  LATIN CAPITAL LETTER N
	%%16#4E  16#039D  %%  GREEK CAPITAL LETTER NU
	16#4F -> 16#004F; %%  LATIN CAPITAL LETTER O
	%%16#4F  16#039F  %%  GREEK CAPITAL LETTER OMICRON
	16#50 -> 16#0050; %%  LATIN CAPITAL LETTER P
	%%16#50  16#03A1  %%  GREEK CAPITAL LETTER RHO
	16#51 -> 16#0051; %%  LATIN CAPITAL LETTER Q
	16#52 -> 16#0052; %%  LATIN CAPITAL LETTER R
	16#53 -> 16#0053; %%  LATIN CAPITAL LETTER S
	16#54 -> 16#0054; %%  LATIN CAPITAL LETTER T
	%%16#54  16#03A4  %%  GREEK CAPITAL LETTER TAU
	16#55 -> 16#0055; %%  LATIN CAPITAL LETTER U
	16#56 -> 16#0056; %%  LATIN CAPITAL LETTER V
	16#57 -> 16#0057; %%  LATIN CAPITAL LETTER W
	16#58 -> 16#0058; %%  LATIN CAPITAL LETTER X
	%%16#58  16#03A7  %%  GREEK CAPITAL LETTER CHI
	16#59 -> 16#0059; %%  LATIN CAPITAL LETTER Y
	%%16#59  16#03A5  %%  GREEK CAPITAL LETTER UPSILON
	16#5A -> 16#005A; %%  LATIN CAPITAL LETTER Z
	%%16#5A  16#0396  %%  GREEK CAPITAL LETTER ZETA
	16#5B -> 16#00C4; %%  LATIN CAPITAL LETTER A WITH DIAERESIS
	16#5C -> 16#00D6; %%  LATIN CAPITAL LETTER O WITH DIAERESIS
	16#5D -> 16#00D1; %%  LATIN CAPITAL LETTER N WITH TILDE
	16#5E -> 16#00DC; %%  LATIN CAPITAL LETTER U WITH DIAERESIS
	16#5F -> 16#00A7; %%  SECTION SIGN
	16#60 -> 16#00BF; %%  INVERTED QUESTION MARK
	16#61 -> 16#0061; %%  LATIN SMALL LETTER A
	16#62 -> 16#0062; %%  LATIN SMALL LETTER B
	16#63 -> 16#0063; %%  LATIN SMALL LETTER C
	16#64 -> 16#0064; %%  LATIN SMALL LETTER D
	16#65 -> 16#0065; %%  LATIN SMALL LETTER E
	16#66 -> 16#0066; %%  LATIN SMALL LETTER F
	16#67 -> 16#0067; %%  LATIN SMALL LETTER G
	16#68 -> 16#0068; %%  LATIN SMALL LETTER H
	16#69 -> 16#0069; %%  LATIN SMALL LETTER I
	16#6A -> 16#006A; %%  LATIN SMALL LETTER J
	16#6B -> 16#006B; %%  LATIN SMALL LETTER K
	16#6C -> 16#006C; %%  LATIN SMALL LETTER L
	16#6D -> 16#006D; %%  LATIN SMALL LETTER M
	16#6E -> 16#006E; %%  LATIN SMALL LETTER N
	16#6F -> 16#006F; %%  LATIN SMALL LETTER O
	16#70 -> 16#0070; %%  LATIN SMALL LETTER P
	16#71 -> 16#0071; %%  LATIN SMALL LETTER Q
	16#72 -> 16#0072; %%  LATIN SMALL LETTER R
	16#73 -> 16#0073; %%  LATIN SMALL LETTER S
	16#74 -> 16#0074; %%  LATIN SMALL LETTER T
	16#75 -> 16#0075; %%  LATIN SMALL LETTER U
	16#76 -> 16#0076; %%  LATIN SMALL LETTER V
	16#77 -> 16#0077; %%  LATIN SMALL LETTER W
	16#78 -> 16#0078; %%  LATIN SMALL LETTER X
	16#79 -> 16#0079; %%  LATIN SMALL LETTER Y
	16#7A -> 16#007A; %%  LATIN SMALL LETTER Z
	16#7B -> 16#00E4; %%  LATIN SMALL LETTER A WITH DIAERESIS
	16#7C -> 16#00F6; %%  LATIN SMALL LETTER O WITH DIAERESIS
	16#7D -> 16#00F1; %%  LATIN SMALL LETTER N WITH TILDE
	16#7E -> 16#00FC; %%  LATIN SMALL LETTER U WITH DIAERESIS
	16#7F -> 16#00E0  %%  LATIN SMALL LETTER A WITH GRAVE
    end.


%% 16#1B
decode_xchar(C) ->
    case C of
	16#0A -> 16#000C;  %%  FORM FEED
	16#14 -> 16#005E;  %%  CIRCUMFLEX ACCENT
	16#28 -> 16#007B;  %%  LEFT CURLY BRACKET
	16#29 -> 16#007D;  %%  RIGHT CURLY BRACKET
	16#2F -> 16#005C;  %%  REVERSE SOLIDUS
	16#3C -> 16#005B;  %%  LEFT SQUARE BRACKET
	16#3D -> 16#007E;  %%  TILDE
	16#3E -> 16#005D;  %%  RIGHT SQUARE BRACKET
	16#40 -> 16#007C;  %%  VERTICAL LINE
	16#65 -> 16#20AC;  %%  EURO SIGN    
	_ -> 16#A0
    end.
