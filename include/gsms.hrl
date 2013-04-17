%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%    sms pdu format (ETSI 03.40)
%%% @end
%%  ref: http://www.dreamfabric.com/sms/vp.html
%%% Created : 15 Oct 2012 by Tony Rogvall <tony@rogvall.se>

-ifndef(__GSMS_HRL__).
-define(__GSMS_HRL__, true).

-record(gsms_addr,
	{
	  type = unknown,
	  addr = ""
	}).

-define(IE_CONCAT8,  16#00).
-define(IE_PORT8,    16#04).
-define(IE_PORT16,   16#05).
-define(IE_CONCAT16, 16#08).

-define(MAX_7BIT_LEN,  160).  %% 153 with concat8 header
-define(MAX_8BIT_LEN,  140).  %% 134 with concat8 header
-define(MAX_16BIT_LEN, 70).   %% 67  with concat8 header


-define(VP_RELATIVE, (60*60*24)).   %% 1 day
-define(DEFAULT_DCS, [message,uncompressed,auto,alert]).
-define(DEFAULT_PID, 0).

-define(MTI_SMS_DELIVER, 2#00).

-type uint() :: non_neg_integer().
-type uint8() :: 0..255.
-type uint16() :: 0..65535.

-type ie() :: {concat,Ref::uint(),N::uint8(),I::uint8()} |
	      {concat16,Ref::uint16(),N::uint8(),I::uint8()} |
	      {concat8,Ref::uint8(),N::uint8(),I::uint8()} |
	      {port8, Dst::uint8(),Src::uint8()} |
	      {port16,Dst::uint16(),Src::uint16()}
	      .

-record(gsms_deliver_pdu, {
	  smsc,             %% :: #gsms_addr{} smsc information
	  rp=false,         %% :1 reply path exists
	  udhi=false,       %% :1 user data header exists
	  sri=false,        %% :1 status report indication
	  res1=0,           %% 0:1
	  res2=0,           %% 0:1
	  mms=false,        %% :1 more messages to send
	  addr,             %% :: #gsms_addr{}
	  pid=?DEFAULT_PID, %% protocol identifier
	  dcs=?DEFAULT_DCS, %% data coding scheme
	  scts,             %% :7/binary
	  udh=[] :: [ie()], %% user data header
	  udl,              %% length in septets/octets (depend on dcs)
	  ud 
	 }).

-define(MTI_SMS_SUBMIT, 2#01).

-record(gsms_submit_pdu, {
	  smsc,             %% ::gsms_addr{}  smsc information
	  rp=false,         %% :1 reply path exists
	  udhi=false,       %% :1 user data header exists
	  srr=false,        %% :1 status report request
	  vpf=relative,     %% :2 validity periad format 0..3
	  rd=true,          %% :1 reject duplicates
	  mref=0,           %% :8
	  addr,             %% ::gsms_addr{} 
	  pid=?DEFAULT_PID, %% protocol identifire
	  dcs=?DEFAULT_DCS, %% data coding scheme
	  vp=?VP_RELATIVE,  %% vary depend on vpf
	  udh=[] :: [ie()], %% user data header
	  udl,
	  ud
	 }).


-endif.


