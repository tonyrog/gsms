%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%    Send/Recive SMS with GSM modem
%%% @end
%%% Created : 15 Oct 2012 by Tony Rogvall <tony@rogvall.se>

-module(gsms).

-compile(export_all).

-export([decode_in/1, decode_in_hex/1]).
-export([decode_out/1, decode_out_hex/1]).
-export([hex_to_binary/1]).

-include("../include/gsms.hrl").

%%
%% Check if pin code is needed
%% ===========================
%% AT+CPIN?
%% +CPIN: READY
%%
%% OK
%%
%% Set pin code if not ready
%% =========================
%% AT+CPIN="1234"
%% 
%% Disable pin code when read
%% ==========================
%% AT+CLCK="SC",0,"1234"
%%
%%
%% Register in network
%% ===================
%% AT+CREG=1
%%
%% Write message to storage
%% ========================
%% AT+CMGW="+46702575687"
%% > Hej <ctl-z>
%% +CMGW: 1
%%
%% Send message from storage (1)
%% =============================
%% AT+CMSS=1
%% +CMS ERROR: 331 (network not available)
%%
%% Service center
%% ==============
%% AT+CSCA?
%% +CSCA: "+46705008999",145
%%
%%
%% Text/Pdu mode
%% =============
%% AT+CMGF=1    text mode
%% AT+CMGF=0    pdu mode
%% 
%% Receive SMS
%% ===========
%% AT+CNMI=1,1   - New incoming SMS is displayed like
%% AT+CNMI=1,2   - Display incoming messages on arrival (unsolicited)
%% 
%% +CMTI: "SM",<x>   (x = counter)
%% 
%% AT+CMGL
%% +CMGL:<x>,"REC UNREAD","+number"
%% message
%% 
%% AT+CMGL="ALL"
%% AT+CMGL=1   (pdu mode)
%% +CMGL: <x>,<y>,<str>,<len>
%% <PDU>
%%
%% AT+CMGR=<x>   (read message <x>)
%%
%% AT+CMGD=<x>(,<x>)*  (delete message )
%% 
%%
%% Send SMS (pdu)
%% ==============
%% AT+CMGF=0    //Set PDU mode
%% AT+CSMS=0    //Check if modem supports SMS commands
%% AT+CMGS=23  //Send message, 23 octets (excluding the two initial zeros)
%% >0011000B916407281553F80000AA0AE8329BFD4697D9EC37<ctrl-z>
%%)
%% There are 23 octets in this message (46 'characters'). The first octet ("00")
%% doesn't count, it is only an indicator of the length of the SMSC information
%% supplied (0).
%%
%%

decode_in_hex(RawData) ->
    decode_in(hex_to_binary(RawData)).

decode_out_hex(RawData) ->
    decode_out(hex_to_binary(RawData)).

decode_in(
  <<L1,SmscAddr:L1/binary,
    TP_RP:1,TP_UDHI:1,TP_SRI:1,R1:1,R2:1,TP_MMS:1,?MTI_SMS_DELIVER:2,
    L2,SenderAddr:L2/binary,
    TP_PID,
    TP_DCS,
    TP_SCTS:7/binary,
    TP_UDL,
    TP_UD/binary>>) ->
    #gsms_deliver_pdu {
	   smsc = SmscAddr,
	   rp   = TP_RP,
	   udhi = TP_UDHI,
	   sri  = TP_SRI, 
	   res1 = R1,
	   res2 = R2,
	   mms  = TP_MMS,
	   mti  = ?MTI_SMS_DELIVER,
	   sender = SenderAddr,
	   pid = TP_PID,
	   dcs = TP_DCS,
	   scts = TP_SCTS,
	   udl  = TP_UDL,
	   ud = TP_UD
	  }.

decode_out(
  <<L1,SmscAddr:L1/binary,
    TP_RP:1,TP_UDHI:1,TP_SRR:1,TP_VPF:2,TP_RD:1,?MTI_SMS_SUBMIT:2,
    TP_MREF,
    L2,Recipient:L2/binary,
    TP_PID,
    TP_DCS,
    Data/binary>>) ->
    {VPLen,VPFormat} =
	case TP_VPF of
	    2#00 -> {0,none};
	    2#10 -> {1,relative};
	    2#01 -> {7,enhanced};
	    2#11 -> {7,absolute}
	end,
    <<VP:VPLen/binary, TP_UDL:8, TP_UD/binary>> = Data,
    #gsms_submit_pdu { smsc = SmscAddr,
		       rp   = TP_RP,
		       udhi = TP_UDHI,
		       srr  = TP_SRR,
		       vpf  = TP_VPF,
		       rd   = TP_RD,
		       mti  = ?MTI_SMS_SUBMIT,
		       mref = TP_MREF,
		       recipient = Recipient,
		       pid = TP_PID,
		       dcs = TP_DCS,
		       vp  = VP,
		       vpformat = VPFormat,
		       udl = TP_UDL,
		       ud = TP_UD }.
	 

hex_to_binary(RawData) ->
    << << H:4 >> || H <- hex_norm_nibbles(RawData) >>.        

hex_norm_nibbles([$\s|Cs]) -> hex_norm_nibbles(Cs);
hex_norm_nibbles([$\t|Cs]) -> hex_norm_nibbles(Cs);
hex_norm_nibbles([$\r|Cs]) -> hex_norm_nibbles(Cs);
hex_norm_nibbles([$\n|Cs]) -> hex_norm_nibbles(Cs);
hex_norm_nibbles([C|Cs]) ->
    if C >= $0, C =< $9 -> [C-$0 | hex_norm_nibbles(Cs)];
       C >= $A, C =< $F -> [(C-$A)+10 | hex_norm_nibbles(Cs)];
       C >= $a, C =< $f -> [(C-$A)+10 | hex_norm_nibbles(Cs)]
    end;
hex_norm_nibbles([]) ->
    [].
