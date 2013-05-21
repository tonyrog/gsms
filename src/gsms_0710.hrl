%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2013, Tony Rogvall
%%% @doc
%%%    GSM 07.10 definitions
%%% @end
%%% Created : 21 May 2013 by Tony Rogvall <tony@rogvall.se>

-ifndef(__GSM_0710_HRL__).
-define(__GSM_0710_HRL__, true).

%%
%% - determine if AT+CMUX is supported
%% - start control channel 
%% - add sub channels
%% - add pseduo terminal proxy to support pppd manager
%%
%%  or ignore the above and run the gsm mux kernel module
%%
%%  http://lhwdb.org/doc/serial/n_gsm.txt
%%
-define(BASIC,    2#11111001).
-define(ADVANCED, 2#01111110).

-define(NEXTENDED,          2#00000001).
-define(COMMAND,            2#00000010).
-define(RESPONSE,           2#00000000).

-define(ADDRESS_0, 2#00000001).
-define(ADDRESS_1, 2#00000101).
-define(ADDRESS_2, 2#00001001).
-define(ADDRESS_3, 2#00001101).

-define(CONTROL_F,      2#00000000).
-define(CONTROL_P,      2#00010000).
-define(CONTROL_SABM,   2#00101111).
-define(CONTROL_UA,     2#01100011).
-define(CONTROL_DM,     2#00001111).
-define(CONTROL_DISC,   2#01000011).
-define(CONTROL_UIH,    2#11101111).
-define(CONTROL_UI,     2#00000011).

%% EA   EXTENSION BIT,  1 = not extended 0= more octets
%% C/R  COMMAND=1, RESPONSE=0
%% <<T:6, C/R:1, EA:1>>

%% TYPE FIELD

%% 5.4.6.3.1 Parameter negotation PN (length=8)
-define(TYPE_PN,   2#10000001).

%% 5.4.6.3.2. Power Saving Control (PSC) 
%% <<?PSC_COMMAND:8, 0:8>>  == AT+CFUN=0  (standard)
%% <<?PSC_COMMAND:8, 1:8, PData:8>> (non-standard ?)
%%
-define(TYPE_PSC,  2#01000001).

%%
%% 5.4.6.3.3 Mulitplexer close down CLD
%%
-define(TYPE_CLD,  2#11000001).

%%
%% 5.4.6.3.4 Test command (ping?)
%%
-define(TYPE_TEST,  2#00100001).

%%
%% 5.4.6.3.5 Flow Control On Command (FCon)
%%
-define(TYPE_FCON,  2#00100001).

%%
%% 5.4.6.3.6 Flow Control Off Command (FCoff)
%%
-define(TYPE_FCOFF,  2#01100001).

%%
%% 5.4.6.3.7 Modem Status Command (length=2) 
%% <<?MSC_COMMAND:8, 2:8, DLCI:8, V24:8>>
-define(TYPE_MSC,  2#11100001).

%%
%% 5.4.6.3.8  Non Supported Command Response (NSC)
%% <<?NSC_RESPONSE:8, 1:8, NSC:8>> 
%%
-define(TYPE_NSC,  2#11001000).

%%
%% 5.4.6.3.9 Remote Port Negotiation Command (RPN)
%% length 1 or 8
-define(TYPE_RPN,  2#10010001).

%%
%% 5.4.6.3.10 Remote Line Status Command (RLS)
%%
-define(TYPE_RLS,  2#01010001).

%%
%% 5.4.6.3.11 Servive Negotiation Command (SNC)
%%
-define(TYPE_SNC,  2#11010001).

-endif.


