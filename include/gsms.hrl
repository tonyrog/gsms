%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%    sms pdu format (ETSI 03.40)
%%% @end
%%  ref: http://www.dreamfabric.com/sms/vp.html
%%% Created : 15 Oct 2012 by Tony Rogvall <tony@rogvall.se>

-ifndef(__GSMS_HRL__).
-define(__GSMS_HRL__, true).

-define(MTI_SMS_SUBMIT, 2#01).

-record(gsms_submit_pdu, {
	  smsc,  %% smsc information
	  rp,    %% :1 reply path exists
	  udhi,  %% :1 user data header exists
	  srr,   %% :1 status report request
	  vpf,   %% :2 validity periad format 0..3
	  rd,    %% :1 reject duplicates
	  mti,   %% :2 message type indicator 2#01 = SUBIT
	  mref,  %% :8 byte
	  recipient, %% Len:8,Type:8,Addr:Len
	  pid,   %% protocol identifire
	  dcs,   %% data coding scheme
	  vp,    %% :8 validity peridod
	  vpformat, 
	  udl,
	  ud
	 }).

-define(MTI_SMS_DELIVER, 2#00).

-record(gsms_deliver_pdu, {
	  smsc,  %% smsc information
	  rp,    %% :1 reply path exists
	  udhi,  %% :1 user data header exists
	  sri,   %% :1 status report indication
	  res1,  %% 0:1
	  res2,  %% 0:1
	  mms,   %% :1 more messages to send
	  mti,   %% :2 message type indicator 2#00 = DELIVER
	  message_ref,  %% :8 byte
	  sender, %% Len:8,Type:8,Addr:Len
	  pid,    %% protocol identifire
	  dcs,    %% data coding scheme
	  scts,     %% :8  (tp_vpf)
	  udl,   %% length in septets/octets (depend on dcs)
	  ud 
	 }).


-endif.


