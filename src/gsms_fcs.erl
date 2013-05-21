%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2013, Tony Rogvall
%%% @doc
%%%    CRC-8 GSM.0710 
%%% @end
%%% Created : 21 May 2013 by Tony Rogvall <tony@rogvall.se>

-module(gsms_fcs).

-export([table/0, init/1, final/2, update/3, update_byte/3]).
-export([crc/1, is_valid/2]).

%%
%% Use gsms_uart to enter AT+CMUX=0, then what?
%% I guess we should implment the above packet type in gsms_uart
%% and allow receiving and sending such frames through gsms_uart
%%
%% Then allocate [1-3] gsms_0710_channel processes, the processes will 
%% behave like a gsms_uart process but use a real gsms_uart 
%% process in MUX mode.
%%

-record(crc_table,
	{
	  poly,    %% list of power polynomial exponents X^5 + X^3 + 1 = [5,3,0]
	  init,    %% initial value
	  final,   %% final value
	  reflect, %% reflected calculation
	  mask,    %% value mask for easy access
	  table    %% table of of crc constants
	 }).

table() ->
%% Name               : CRC-8 GSM 07.10
%% Initializing value : 16#FF
%%   Finalizing value : 16#FF
%%   Polynomial value : 16#07
%%       Mirror value : 16#E0
%%   Polynom          : x^8 + x^2 + x + 1
%%
#crc_table {
    poly = [8,2,1,0],
    init = 16#FF,
   final = 16#FF,
 reflect = true,
    mask = 16#FF,
   table = { 16#00,16#91,16#E3,16#72,16#07,16#96,16#E4,16#75,16#0E,16#9F,16#ED,
             16#7C,16#09,16#98,16#EA,16#7B,16#1C,16#8D,16#FF,16#6E,16#1B,16#8A,
             16#F8,16#69,16#12,16#83,16#F1,16#60,16#15,16#84,16#F6,16#67,16#38,
             16#A9,16#DB,16#4A,16#3F,16#AE,16#DC,16#4D,16#36,16#A7,16#D5,16#44,
             16#31,16#A0,16#D2,16#43,16#24,16#B5,16#C7,16#56,16#23,16#B2,16#C0,
             16#51,16#2A,16#BB,16#C9,16#58,16#2D,16#BC,16#CE,16#5F,16#70,16#E1,
             16#93,16#02,16#77,16#E6,16#94,16#05,16#7E,16#EF,16#9D,16#0C,16#79,
             16#E8,16#9A,16#0B,16#6C,16#FD,16#8F,16#1E,16#6B,16#FA,16#88,16#19,
             16#62,16#F3,16#81,16#10,16#65,16#F4,16#86,16#17,16#48,16#D9,16#AB,
             16#3A,16#4F,16#DE,16#AC,16#3D,16#46,16#D7,16#A5,16#34,16#41,16#D0,
             16#A2,16#33,16#54,16#C5,16#B7,16#26,16#53,16#C2,16#B0,16#21,16#5A,
             16#CB,16#B9,16#28,16#5D,16#CC,16#BE,16#2F,16#E0,16#71,16#03,16#92,
             16#E7,16#76,16#04,16#95,16#EE,16#7F,16#0D,16#9C,16#E9,16#78,16#0A,
             16#9B,16#FC,16#6D,16#1F,16#8E,16#FB,16#6A,16#18,16#89,16#F2,16#63,
             16#11,16#80,16#F5,16#64,16#16,16#87,16#D8,16#49,16#3B,16#AA,16#DF,
             16#4E,16#3C,16#AD,16#D6,16#47,16#35,16#A4,16#D1,16#40,16#32,16#A3,
             16#C4,16#55,16#27,16#B6,16#C3,16#52,16#20,16#B1,16#CA,16#5B,16#29,
             16#B8,16#CD,16#5C,16#2E,16#BF,16#90,16#01,16#73,16#E2,16#97,16#06,
             16#74,16#E5,16#9E,16#0F,16#7D,16#EC,16#99,16#08,16#7A,16#EB,16#8C,
             16#1D,16#6F,16#FE,16#8B,16#1A,16#68,16#F9,16#82,16#13,16#61,16#F0,
             16#85,16#14,16#66,16#F7,16#A8,16#39,16#4B,16#DA,16#AF,16#3E,16#4C,
             16#DD,16#A6,16#37,16#45,16#D4,16#A1,16#30,16#42,16#D3,16#B4,16#25,
             16#57,16#C6,16#B3,16#22,16#50,16#C1,16#BA,16#2B,16#59,16#C8,16#BD,
             16#2C,16#5E,16#CF}
}.

%%
%% Check if checksum is correct
%%
is_valid(Bytes,FCS) ->
    T = table(),
    CRC0 = init(T),
    CRC1 = update(T, CRC0, Bytes),
    case update_byte(T, CRC1, FCS) of
	16#CF -> true;
	_ -> false
    end.

%%
%% Initiate CRC value from CRC tabe
%%
crc(Bytes) ->
    T = table(),
    CRC0 = init(T),
    CRC1 = update(T, CRC0, Bytes),
    final(T, CRC1).

init(CRCt) -> 
    CRCt#crc_table.init.

final(CRCt, CRC) -> 
    CRC bxor CRCt#crc_table.final.

update_byte(#crc_table { table=Table},CRC, Val) ->
    element((CRC bxor Val)+1,Table).


update(CRCt, CRC, [H|T]) ->
    if is_integer(H) ->
	    update(CRCt, update_byte(CRCt,CRC,H), T);
       true ->
	    update(CRCt, update(CRCt, CRC, H), T)
  end;
update(_CRCt, CRC, []) -> CRC;
update(CRCt, CRC, Value) when is_integer(Value) ->
    update_byte(CRCt, CRC, Value);
update(CRCt, CRC, Bin) when is_binary(Bin) ->
    update_bin_(CRCt, CRC, Bin).

update_bin_(_CRCt, CRC, <<>>) ->
    CRC;
update_bin_(CRCt, CRC, <<Value:8,Bin/binary>>) ->
    update_bin_(CRCt, update_byte(CRCt,CRC,Value), Bin).
