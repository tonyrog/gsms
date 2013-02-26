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

-define(Q, $").

-define(decode_bool(X), ((X) =/= 0)).
-define(encode_bool(X), if (X) -> 1; true -> 0 end).
-define(is_byte(X), (((X) band (bnot 16#ff)) =/= 0)).

decode_in_hex(RawData) ->
    decode_in(hex_to_binary(RawData)).

decode_out_hex(RawData) ->
    decode_out(hex_to_binary(RawData)).

decode_in(<<L1,SmscAddr:L1/binary,
	    TP_RP:1,TP_UDHI:1,TP_SRI:1,R1:1,R2:1,TP_MMS:1,?MTI_SMS_DELIVER:2,
	    AddrLen,Data0/binary>>) ->
    ALen = (AddrLen + 1) bsr 1,  %% number of bytes
    <<AType,Addr:ALen/binary,TP_PID,TP_DCS,
      TP_SCTS:7/binary,TP_UDL,TP_UD/binary>> = Data0,
    AddrType = decode_addr_type(AType),
    Dcs = decode_dcs(TP_DCS),
    #gsms_deliver_pdu {
			smsc = decode_smsc_addr(SmscAddr),
			rp   = ?decode_bool(TP_RP),
			udhi = ?decode_bool(TP_UDHI),
			sri  = ?decode_bool(TP_SRI),
			res1 = R1,
			res2 = R2,
			mms  = ?decode_bool(TP_MMS),
			addr = decode_addr(AddrType,Addr),
			pid = TP_PID,
			dcs = Dcs,
			scts = decode_scts(TP_SCTS),
			udl  = TP_UDL,
			ud = decode_ud(Dcs,TP_UDHI,TP_UDL,TP_UD)
		      }.

decode_out(<<L1,SmscAddr:L1/binary,
	     TP_RP:1,TP_UDHI:1,TP_SRR:1,TP_VPF:2,TP_RD:1,?MTI_SMS_SUBMIT:2,
	     TP_MREF,
	     AddrLen,   %% in semi octets
	     Data0/binary>>) ->
    ALen = (AddrLen + 1) bsr 1,  %% number of bytes
    <<AType,Addr:ALen/binary,TP_PID,TP_DCS,Data1/binary>> = Data0,
    AddrType = decode_addr_type(AType),
    VPF = decode_vpf(TP_VPF),
    VPLen = case VPF of
		none -> 0;
		relative -> 1;
		enhanced -> 7;
		absolute -> 7
	    end,
    <<VP:VPLen/binary, TP_UDL:8, TP_UD/binary>> = Data1,
    Dcs = decode_dcs(TP_DCS),
    #gsms_submit_pdu { 
		       smsc = decode_smsc_addr(SmscAddr),
		       rp   = ?decode_bool(TP_RP),
		       udhi = ?decode_bool(TP_UDHI),
		       srr  = ?decode_bool(TP_SRR),
		       vpf  = VPF,
		       rd   = ?decode_bool(TP_RD),
		       mref = TP_MREF,
		       addr = decode_addr(AddrType,Addr),
		       pid = TP_PID,
		       dcs = Dcs,
		       vp  = decode_vp(VPF,VP),
		       udl = TP_UDL,
		       ud = decode_ud(Dcs,TP_UDHI,TP_UDL,TP_UD)
		     }.

encode_sms_submit(Opts,Body) ->
    Pdu = set_pdu_opts(Opts,#gsms_submit_pdu{}),
    %% fixme: may be moved to encode_sms!
    {Udl,Ud} = encode_ud(Pdu#gsms_submit_pdu.dcs, Body),
    Pdu1 = Pdu#gsms_submit_pdu { udl=Udl, ud=Ud },
    dump_yang(Pdu1),
    Bin = encode_sms(Pdu1),
    binary_to_hex(Bin).


set_pdu_opts([{Key,Value}|Kvs], R=#gsms_submit_pdu{}) ->
    case Key of
	smsc -> %% FIXME: check format
	    set_pdu_opts(Kvs, R#gsms_submit_pdu { smsc=Value });	    
	rp when is_boolean(Value) ->
	    set_pdu_opts(Kvs, R#gsms_submit_pdu { rp=Value });
	udhi when is_boolean(Value) ->
	    set_pdu_opts(Kvs, R#gsms_submit_pdu { udhi=Value });
	srr when is_boolean(Value) ->
	    set_pdu_opts(Kvs, R#gsms_submit_pdu { srr=Value });
	mref when ?is_byte(Value) ->
	    set_pdu_opts(Kvs, R#gsms_submit_pdu { mref=Value });
	vpf ->
	    set_pdu_opts(Kvs, R#gsms_submit_pdu { vpf=Value });
	addr when is_record(Value,gsms_addr) ->
	    set_pdu_opts(Kvs, R#gsms_submit_pdu { addr=Value });
	addr when is_list(Value),hd(Value)=:=$+ ->
	    Addr = #gsms_addr { type=international, addr=Value },
	    set_pdu_opts(Kvs, R#gsms_submit_pdu { addr=Addr});
	addr when is_list(Value) ->
	    Addr = #gsms_addr { type=unknown, addr=Value },
	    set_pdu_opts(Kvs, R#gsms_submit_pdu { addr=Addr});
	pid ->
	    set_pdu_opts(Kvs, R#gsms_submit_pdu { pid=Value });
	dcs ->
	    if is_integer(Value) ->
		    Dcs1 = try decode_dcs(Value) of
			       Dcs0 -> Dcs0
			   catch
			       error:_ -> Value
			   end,
		    set_pdu_opts(Kvs, R#gsms_submit_pdu { dcs=Dcs1 });
	       true ->
		    encode_dcs(Value),
		    set_pdu_opts(Kvs, R#gsms_submit_pdu { dcs=Value })
	    end;
	vp ->
	    set_pdu_opts(Kvs, R#gsms_submit_pdu { vp=Value })
    end;
set_pdu_opts([], R) ->
    R.


encode_sms(R=#gsms_submit_pdu{}) ->
    SmscAddr = encode_smsc_addr(R#gsms_submit_pdu.smsc),
    TP_RP   = ?encode_bool(R#gsms_submit_pdu.rp),
    TP_UDHI = ?encode_bool(R#gsms_submit_pdu.udhi),
    TP_SRR  = ?encode_bool(R#gsms_submit_pdu.srr),
    TP_VPF  = encode_vpf(R#gsms_submit_pdu.vpf),
    TP_RD   = ?encode_bool(R#gsms_submit_pdu.rd),
    TP_MREF = R#gsms_submit_pdu.mref,
    {AType,Addr,AddrLen} = encode_addr(R#gsms_submit_pdu.addr),
    TP_PID = R#gsms_submit_pdu.pid,
    TP_DCS = encode_dcs(R#gsms_submit_pdu.dcs),
    TP_VP  = encode_vp(R#gsms_submit_pdu.vpf,R#gsms_submit_pdu.vp),
    TP_UDL = R#gsms_submit_pdu.udl,
    TP_UD  = R#gsms_submit_pdu.ud,
    io:format("SmscAddr=~p,Addr=~p,TP_VP=~p,TP_UD=~p\n", 
	      [SmscAddr,Addr,TP_VP,TP_UD]),
    L1 = byte_size(SmscAddr),
    <<L1,SmscAddr:L1/binary,
      TP_RP:1,TP_UDHI:1,TP_SRR:1,TP_VPF:2,TP_RD:1,?MTI_SMS_SUBMIT:2,
      TP_MREF,
      AddrLen,  %% in semi octets
      AType,
      Addr/binary,
      TP_PID,
      TP_DCS,
      TP_VP/binary,
      TP_UDL, 
      TP_UD/binary>>.


binary_to_hex(Bin) ->
    [ element(I+1,{$0,$1,$2,$3,$4,$5,$6,$7,$8,$9,
		   $A,$B,$C,$D,$E,$F}) || <<I:4>> <= Bin ].

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

%%
%% addresses
%%

decode_smsc_addr_type(<<AType,_/binary>>) ->
    decode_addr_type(AType);
decode_smsc_addr_type(<<>>) ->
    undefined.

decode_smsc_addr(<<AType,Ds/binary>>) ->
    T = decode_addr_type(AType),
    A = decode_addr_semi(T, Ds),
    #gsms_addr { type=T, addr=A };
decode_smsc_addr(<<>>) ->
    undefined.

decode_vpf(2#00) -> none;
decode_vpf(2#10) -> relative;
decode_vpf(2#01) -> enhanced;
decode_vpf(2#11) -> absolute.

encode_vpf(none)     -> 2#00;
encode_vpf(relative) -> 2#10;
encode_vpf(enhanced) -> 2#01;
encode_vpf(absolute) -> 2#11.

%% validity period in seconds
decode_vp(none,<<>>) -> 0;
decode_vp(relative,<<VP>>) ->
    if VP =< 143 -> (VP+1)*5*60;               %% 1..12  hours  (+ 5min)
       VP =< 167 -> (12*60 + (VP-143)*30)*60;  %% 12..24 hours (+ 30min)
       VP =< 196 -> (VP-166)*24*60*60;         %% 2 .. 30 days
       true      -> ((VP-192)*7*24)*60*60      %% 5 .. 63 weeks
    end;
decode_vp(absolute,V) ->
    decode_scts(V).

encode_vp(none, _) -> <<>>;
encode_vp(relative, V) -> 
    Min_0  = V div 60,        %% number of minutes
    Hour_1 = Min_0 div 60,    %% number of hours
    _Min_1  = Min_0 rem 60,   %% mintes with in the hour
    Day_2  = Hour_1 div 24,   %% days
    _Hour_2 = Hour_1 rem 24,  %% hour with in day
    Week_3 = Day_2 div 7,     %% weeks
    _Day_3  = Day_2 rem 7,    %% day within week
    if Week_3 >= 5, Week_3 =< 64 -> 
	    <<((Week_3-5)+197)>>;
       Day_2  >= 2, Day_2 =< 30 ->
	    <<((Day_2-2)+168)>>;
       Min_0  >= 12*60, Min_0 =< 24*60 ->
	    <<(((Min_0 - 12*60) div 30) + 143)>>;
       Min_0 >= 5 ->
	    <<((Min_0 div 5)-1)>>;
       true ->
	    <<0>>
    end;
encode_vp(absolute,V) -> 
    encode_scts(V).
       
encode_smsc_addr(undefined) ->
    <<>>;
encode_smsc_addr(#gsms_addr {type=T,addr=A}) ->
    AType = encode_addr_type(T),
    Ds = encode_addr_semi(T, A),
    <<AType,Ds/binary>>.

%% Handle the ones in use only! (FIXME)
decode_addr_type(2#10000001) -> unknown;
decode_addr_type(2#10101000) -> national;
decode_addr_type(2#10010001) -> international;
decode_addr_type(T) -> T.

encode_addr_type(unknown)       -> 16#81; %% 129
encode_addr_type(international) -> 16#91; %% 145
encode_addr_type(national)      -> 16#A8;
encode_addr_type(T) -> T.

decode_addr(T, <<>>) -> 
    #gsms_addr { type=T, addr=[] };
decode_addr(T, Data) ->
    A = decode_addr_semi(T, Data),
    #gsms_addr { type=T, addr=A }.

encode_addr(#gsms_addr { type=T, addr=A }) ->
    Addr = encode_addr_semi(T, A),
    AddrLen = length(A),   %% number semi digits
    {encode_addr_type(T),Addr,AddrLen}.


decode_addr_semi(international, SemiOctets) ->
    [$+|decode_semi_octets(SemiOctets)];
decode_addr_semi(_, SemiOctets) ->
    decode_semi_octets(SemiOctets).

encode_addr_semi(international, [$+|Ds]) ->
    encode_semi_octets(Ds);
encode_addr_semi(_, Ds) ->
    encode_semi_octets(Ds).

     
decode_bcd(<<H:4,L:4, T/binary>>) ->
    [H*10+L | decode_bcd(T)];
decode_bcd(<<>>) ->
    [].

decode_swapped_bcd(Bin) ->
    [(H*10+L) || <<L:4,H:4>> <= Bin].

encode_swapped_bcd(Ds) ->
    << <<(D rem 10):4, (D div 10):4>> || D <- Ds >>.

decode_semi_octets(<<>>) -> [];
decode_semi_octets(<<16#F:4,D:4>>) -> [D+$0];
decode_semi_octets(<<L:4,H:4,Ds/binary>>) -> [H+$0,L+$0|decode_semi_octets(Ds)].

encode_semi_octets([])  -> <<>>;
encode_semi_octets([D]) -> <<16#F:4,(D-$0):4>>;
encode_semi_octets([H,L|Ds]) ->
    <<(L-$0):4,(H-$0):4, (encode_semi_octets(Ds))/binary>>.
    
decode_scts(Bin) ->
    [YY,Mon,Day,Hour,Min,Sec,Tz] = decode_swapped_bcd(Bin),
    {Year0,_,_} = date(),   %% Year0 is current year
    YY0 = Year0 rem 100,    %% YY0 curent year two digits
    Century = Year0 - YY0,  %% Current Century,
    Year = if YY > YY0+1 ->
		   YY + (Century - 100);
	      true ->
		   YY + Century
	   end,
    Date = {Year,Mon,Day},
    Time = {Hour,Min,Sec},
    Tz0  = (15*(Tz band 16#7f))/60,
    TzH = if (Tz band 16#80) =/= 0 -> -Tz0; true -> Tz0 end,
    {{Date,Time},TzH}.

encode_scts({{{Year0,Mon,Day},{Hour,Min,Sec}},TzH}) ->
    Year = Year0 - 2000,
    Tz = if TzH < 0 -> 
		 16#80 bor (trunc((TzH * 60)/15) band 16#7f);
	    true ->
		 trunc((TzH * 60)/15) band 16#7f
	 end,
    encode_swapped_bcd([Year,Mon,Day,Hour,Min,Sec,Tz]).


decode_dcs(V) ->
    if (V bsr 6) =:= 2#00 -> %% 00xxxxxx
	    [message] ++
		if V band 2#00100000 =:= 0 -> [uncompressed];
		   true -> [compressed]
		end ++
		case V band 2#1100 of
		    2#0000 -> [default];
		    2#0100 -> [octet];
		    2#1000 -> [ucs2];
		    2#1100 -> [reserved]
		end ++
		case V band 2#11 of
		    2#00 -> [alert];
		    2#01 -> [me];
		    2#10 -> [sim];
		    2#11 -> [te]
		end;
       (V bsr 4) =:= 2#1100 -> %% message waiting, discard
	    dcs_message_waiting(V,discard,default);
       (V bsr 4) =:= 2#1101 -> %% message waiting, store
	    dcs_message_waiting(V,store,default);
       (V bsr 4) =:= 2#1110 -> %% message waiting, store
	    dcs_message_waiting(V,store,ucs2);
       (V bsr 4) =:= 2#1111 ->
	    [data,uncompressed] ++
	    if V band 2#0100 =:= 0 -> [octet];
	       true -> [default]
	    end ++
	    case V band 2#11 of
		2#00 -> [alert];
		2#01 -> [me];
		2#10 -> [sim];
		2#11 -> [te]
	    end;
       true -> V
    end.
	    
dcs_message_waiting(V,Store,Alphabet) ->
    [message_waiting,uncompressed,Alphabet,Store]++
    if V band 2#0100 =:= 0 -> [inactive]; true -> [active] end ++
    case V band 2#11 of
	2#00 -> [voicemail];
	2#01 -> [fax];
	2#10 -> [email];
	2#11 -> [other]
    end.

encode_dcs([message,Comp,Alpha,Class]) ->
    2#00000000 +
	if Comp =:= compressed -> 2#00100000;
	   true -> 2#00000000
	end +
	case Alpha of 
	    default  -> 2#0000;
	    octet    -> 2#0100;
	    ucs2     -> 2#1000;
	    reserved -> 2#1100
	end + 
	case Class of
	    alert -> 2#00;
	    me    -> 2#01;
	    sim   -> 2#10;
	    te    -> 2#11
	end;
encode_dcs([message_waiting,uncompressed,Alphabet,Store,Active,Type]) ->
    if Alphabet =:= ucs2, Store =:= store ->
	    2#11110000;
       Alphabet =:= default, Store =:= store ->
	    2#11100000;
       Alphabet =:= default, Store =:= discard ->
	    2#11000000
    end +
	if Active =:= active -> 2#0100;
	   true -> 2#0000
	end +
	case Type of
	    voicemail -> 2#00;
	    fax -> 2#01;
	    email -> 2#10;
	    other -> 2#11
	end;
encode_dcs([data,uncompressed,Alphabet,Class]) ->
    2#11110000 +
	if Alphabet =:= octet -> 2#100;
	   true -> 2#000
	end +
	case Class of
	    alert -> 2#00;
	    me    -> 2#01;
	    sim   -> 2#10;
	    te    -> 2#11
	end;
encode_dcs(V) when is_integer(V) ->
    V.
		   
%% FIXME: UDHL/UDH multi part header etc
decode_ud([_Msg,uncompressed,default|_], 0, UDL, Data) ->
    L7 = UDL,                  %% number of septets
    L8 = (L7*7 + 7) div 8,     %% number octets needed 
    LB = ((L7 + 6) div 7)*7,   %% number of septet blocks
    Pad = LB - L8,             %% the pad needed for decode_gsm7
    io:format("Data: ~w\n", [Data]),
    io:format("Gsm7:(#7:~w,#8:~w,#8*:~w)\n", [L7,L8,LB]),
    <<Gsm8:L7/binary,_/binary>> = decode_gsm7(<<Data/binary,0:Pad/unit:8>>),
    io:format("Gsm8: ~w\n", [Gsm8]),
    gsms_0338:decode(Gsm8);
decode_ud([_Msg,uncompressed,ucs2|_],_UDHI,_UDL,Data) ->
    %% FIXME: pad?
    Ucs = [ U || <<U:16/big>> <= Data ],
    io:format("Ucs2: ~w\n", [Ucs]),
    Ucs;
decode_ud([_Msg,uncompressed,octet|_],_UDHI,_UDL,Data) ->
    %% FIXME: pad?
    Octets = [ U || <<U:8>> <= Data ],
    io:format("Octets: ~w\n", [Octets]),
    Octets;
decode_ud(_DCS,_UDHI,_UDL,UD) ->
    UD.

encode_ud([_Msg,uncompressed,default|_], Message) ->
    %% FIXME: Message => 7bit gsms_0338:encode(...)
    Gsm8 = iolist_to_binary(Message),
    L7 = byte_size(Gsm8),        %% number of septets
    L8 = (L7*7 + 7) div  8,      %% number of octets needed
    LB = ((L7 + 7) div 8)*8,     %% number of octet blocks
    Pad = LB - L7,               %% the pad needed for encode_gsm7
    io:format("L7=~w,L8=~w,LB=~w,Pad=~w\n",[L7,L8,LB,Pad]),
    <<Gsm7:L8/binary,_/binary>> = encode_gsm7(<<Gsm8/binary,0:Pad/unit:8>>),
    {L7,Gsm7};
encode_ud([_Msg,uncompressed,ucs2|_], Message) ->
    Udl = 2*length(Message),
    {Udl, << <<U:16/big>> || U <- Message >>};
encode_ud([_Msg,uncompressed,octet|_], Message) ->
    Octets = iolist_to_binary(Message),
    {byte_size(Octets), Octets}.
    
%% how invented this stupid encoding ?
%% convert Septet coding into octet coding
decode_gsm7(<<X1:1,Y7:7, X2:2,Y6:6, X3:3,Y5:5, X4:4,Y4:4,
	      X5:5,Y3:3, X6:6,Y2:2, X7:7,Y1:1,  More/binary>>) ->
    <<0:1,Y7:7,
      0:1,Y6:6,X1:1,
      0:1,Y5:5,X2:2,
      0:1,Y4:4,X3:3,
      0:1,Y3:3,X4:4,
      0:1,Y2:2,X5:5,
      0:1,Y1:1,X6:6,
      0:1,X7:7,
      (decode_gsm7(More))/binary>>;
decode_gsm7(<<>>) ->
    <<>>.

%% padding must be added before this procedure
encode_gsm7(<<0:1,Y7:7,      0:1,Y6:6,X1:1, 0:1,Y5:5,X2:2, 0:1,Y4:4,X3:3,
	      0:1,Y3:3,X4:4, 0:1,Y2:2,X5:5, 0:1,Y1:1,X6:6, 0:1,     X7:7,
	      More/binary >>) ->
    <<X1:1,Y7:7, X2:2,Y6:6, X3:3,Y5:5, X4:4,Y4:4,
      X5:5,Y3:3, X6:6,Y2:2, X7:7,Y1:1,  
      (encode_gsm7(More))/binary >>;
encode_gsm7(<<>>) ->
    <<>>.


is_gsms_record(P) ->
    case P of
	#gsms_addr{} -> true;
	#gsms_submit_pdu{} -> true;
	#gsms_deliver_pdu{} -> true;
	_ -> false
    end.

separator(D,S) ->
    case is_gsms_record(D) of
	true -> "";
	false -> S
    end.

indent(I) ->
    lists:duplicate(I, $\s).

dump(P) ->
    dump_json(P).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Format values and records in JSON format
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dump_json(P) ->
    io:format("~s\n", [fmt_json(P)]).

fmt_json(P) ->
    case is_gsms_record(P) of
	true  -> fmt_json_record(0,P,"\n");
	false -> fmt_json_value(0,P,"\n")
    end.

fmt_json_record(I,P=#gsms_addr{},N)  ->
    fmt_json_record(I,P,record_info(fields,gsms_addr),N);
fmt_json_record(I,P=#gsms_submit_pdu{},N)  -> 
    fmt_json_record(I,P,record_info(fields,gsms_submit_pdu),N);
fmt_json_record(I,P=#gsms_deliver_pdu{},N) -> 
    fmt_json_record(I,P,record_info(fields,gsms_deliver_pdu),N).

fmt_json_record(I,P,Fs,N) ->
    [R|Ds] = tuple_to_list(P),
    [ "{",N,
      fmt_json_fields(I+2, R, [struct|Fs], [R|Ds], N),
      N, indent(I), "}" ].

fmt_json_fields(I,R,[F],[D],N) ->
    [ [indent(I),fmt_json_field(I,R,F,D,N) ] ];
fmt_json_fields(I,R,[F|Fs],[D|Ds],N) ->
    [ [indent(I),fmt_json_field(I,R,F,D,N),",",N] |
      fmt_json_fields(I,R,Fs,Ds,N)];
fmt_json_fields(_I,_R,[],[],_N) ->
    [].

fmt_json_field(I,R,F,D,N) ->
    Fk = atom_to_list(F),
    Dk = fmt_json_value(I,R,F,D,N),
    [Fk,": ",Dk].

fmt_json_value(I,D,N) ->
    fmt_json_value(I,undefined,undefined,D,N).

fmt_json_value(I,R,F,D,N) ->
    if  
	is_boolean(D) -> [atom_to_list(D)];
	is_integer(D) -> [integer_to_list(D)];
	is_atom(D)    -> [?Q,atom_to_list(D),?Q];
	is_binary(D), R =:= ipv4, (F =:= src orelse F =:= dst) ->
	    <<X1,X2,X3,X4>> = D,
	    [?Q,io_lib:format("~w.~w.~w.~w", [X1,X2,X3,X4]),?Q];
	is_binary(D) ->		 		     
	    io_lib:format("~p", [D]);
	is_tuple(D) ->
	    case is_gsms_record(D) of
		true ->
		    fmt_json_record(I+2,D,N);
		false ->
		    io_lib:format("~p", [D])
	    end;
	is_list(D) ->
	    try iolist_size(D) of
		_Sz -> [?Q,D,?Q]
	    catch
		error:_ ->  %% formt as JSON array?
		    fmt_json_array(D)
	    end
    end.

fmt_json_array(Ds) ->
    ["[", fmt_json_elems(Ds), "]"].

fmt_json_elems([D]) ->
    fmt_json_value(0,D,"");
fmt_json_elems([D|Ds]) ->
    [fmt_json_value(0,D,""),"," | fmt_json_elems(Ds)];
fmt_json_elems([]) -> [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% YANG FORMAT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dump_yang(P) ->
    io:format("~s\n", [fmt_yang(P)]).

fmt_yang(P) ->
    case is_gsms_record(P) of
	true  -> fmt_yang_record(0,P,"\n");
	false -> fmt_yang_value(0,P,"\n")
    end.

fmt_yang_record(I,P=#gsms_addr{},N)  ->
    fmt_yang_record(I,P,record_info(fields,gsms_addr),N);
fmt_yang_record(I,P=#gsms_submit_pdu{},N)  -> 
    fmt_yang_record(I,P,record_info(fields,gsms_submit_pdu),N);
fmt_yang_record(I,P=#gsms_deliver_pdu{},N) -> 
    fmt_yang_record(I,P,record_info(fields,gsms_deliver_pdu),N).

fmt_yang_record(I,P,Fs,N) ->
    [R|Ds] = tuple_to_list(P),
    [ atom_to_list(R), " {", N,
      fmt_yang_fields(I+2, R, Fs, Ds, N),
      indent(I), "}" ].

fmt_yang_fields(I,R,[F|Fs],[D|Ds],N) ->
    [ [indent(I),fmt_yang_field(I,R,F,D,N),separator(D,";"),N] |
      fmt_yang_fields(I,R,Fs,Ds,N)];
fmt_yang_fields(_I,_R,[],[],_N) ->
    [].

fmt_yang_field(I,R,F,D,N) ->
    Fk = atom_to_list(F),
    Dk = fmt_yang_value(I,R,F,D,N),
    [Fk," ",Dk].

fmt_yang_value(I,D,N) ->
    fmt_yang_value(I,undefined,undefined,D,N).

fmt_yang_value(I,R,F,D,N) ->
    if  
	is_boolean(D) -> [atom_to_list(D)];
	is_integer(D) -> [integer_to_list(D)];
	is_atom(D)    -> [atom_to_list(D)];
	is_binary(D), R =:= ipv4, (F =:= src orelse F =:= dst) ->
	    <<X1,X2,X3,X4>> = D,
	    [?Q,io_lib:format("~w.~w.~w.~w", [X1,X2,X3,X4]),?Q];
	is_binary(D) -> %% fixme!
	    [?Q,io_lib:format("~p", [D]),?Q];
	is_tuple(D) ->
	    case is_gsms_record(D) of
		true ->
		    fmt_yang_record(I+2,D,N);
		false ->
		    io_lib:format("~p", [D])
	    end;
	is_list(D) ->
	    try iolist_size(D) of
		_Sz -> [?Q,D,?Q]
	    catch
		error:_ ->  %% formt as YANG array?
		    fmt_yang_array(D)
	    end
    end.

fmt_yang_array(Ds) ->
    fmt_yang_elems(Ds).

fmt_yang_elems([D]) ->
    fmt_yang_value(0,D,"");
fmt_yang_elems([D|Ds]) ->
    [fmt_yang_value(0,D,"")," " | fmt_yang_elems(Ds)];
fmt_yang_elems([]) -> [].
