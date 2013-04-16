%%%---- BEGIN COPYRIGHT --------------------------------------------------------
%%%
%%% Copyright (C) 2007 - 2012, Rogvall Invest AB, <tony@rogvall.se>
%%%
%%% This software is licensed as described in the file COPYRIGHT, which
%%% you should have received as part of this distribution. The terms
%%% are also available at http://www.rogvall.se/docs/copyright.txt.
%%%
%%% You may opt to use, copy, modify, merge, publish, distribute and/or sell
%%% copies of the Software, and permit persons to whom the Software is
%%% furnished to do so, under the terms of the COPYRIGHT file.
%%%
%%% This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
%%% KIND, either express or implied.
%%%
%%%---- END COPYRIGHT ----------------------------------------------------------
%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @author Malotte Westman Lönne <malotte@malotte.net>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%     SMS command driver.
%%%
%%% Created :  1 Jul 2010 by Tony Rogvall 
%%% @end
%%%-------------------------------------------------------------------
-module(gsms_drv).

-behaviour(gen_server).

-include_lib("lager/include/log.hrl").

%% API
-export([start_link/1, 
	 stop/1,
	 subscribe/1,
	 subscribe/2,
	 unsubscribe/2,
	 debug/2,
	 setopts/2,
	 at/2, ats/2, atd/2]).

%% Option processing
-export([options/0,
	 split_opts/2,
	 normalise_opts/1,
	 validate_opts/1,
	 validate_opt/2]).
%%
-export([reset/1]).
%% mode bits
-export([init_csms_service/1]).
-export([check_csms_capability/1]).
-export([set_csms_notification/1]).
-export([set_csms_pdu_mode/1]).
-export([get_version/1]).
-export([get_manufacturer/1, get_model/1]).
-export([get_imei/1, get_msisdn/1, get_imsi/1]).
-export([get_activity_status/1]).
-export([get_network_registration_status/1]).
-export([get_signal_strength/1]).
-export([get_battery_status/1]).
-export([get_smsc/1]).

%% sms
-export([list_unread_messages/1]).
-export([list_read_messages/1]).
-export([list_unsent_messages/1]).
-export([list_sent_messages/1]).
-export([list_all_messages/1]).
-export([list_indices/1]).

-export([delete_message/2, delete_message/3]).
-export([delete_all_message/1]).
-export([read_message/2]).
-export([read_all_messages/1]).
-export([write_message/3]).
-export([send_message/3]).
-export([trimhd/1,trimtl/1,trim/1]).

%% gen_server callbacks
-export([init/1, 
	 handle_call/3, 
	 handle_cast/2, 
	 handle_info/2,
	 terminate/2, 
	 code_change/3]).

-define(SERVER, ?MODULE). 
-define(UART_DEFAULT_OPTS,
	[{baud,115200},{mode,list},{active,true},
	 {packet,line},{csize,8},{parity,none},{stopb,1}]).
-define(CTRL_Z, 16#1A).
-define(ESC,    16#1B).

%% For dialyzer
-type gsms_option() :: device | reopen_timeout | reply_timeout | debug.

-type gsms_options()::{device, string()} |
		      {reopen_timeout, timeout()} |
		      {reply_timeout, timeout()} |
		      {smsc, string()} | 
		      {debug, boolean()}.

-type driver() :: pid() | atom().

-record(subscription,
	{
	  pid,
	  mon,
	  pattern
	}).

-record(ctx,
	{
	  uart,           %% serial port descriptor
	  device,         %% device string
	  uopts=[],       %% uart options
 	  opts=[],        %% sms options
	  command="",     %% last command
	  reply=[],       %% list of reply line data
	  activity=none,  %% |at,ats,atd
	  client,         %% last client
	  queue,          %% request queue
	  reply_timer,    %% timeout waiting for reply
	  reopen_timer,   %% timer ref
	  subs = [],      %% #subscription{}
	  trace           %% debug tracing
	}).

%%%===================================================================
%%% API
%%%===================================================================

-spec options() -> [uart:uart_option()|gsms_option()].

options() ->
    uart:options() ++
	[smsc,
	 reopen_timeout,
	 reply_timeout,
	 debug].

-spec reset(Drv::driver()) -> ok.

reset(Drv)  ->
    at(Drv,"Z"),
    at(Drv,"E0"),
    set_csms_pdu_mode(Drv),
    set_csms_notification(Drv).


-spec init_csms_service(Drv::driver()) -> ok.    
init_csms_service(Drv) ->
    ok = check_csms_capability(Drv),
    ok = set_csms_pdu_mode(Drv),
    ok = set_csms_notification(Drv),
    %% how do we read the stored pdu like they where receive and when?
    %% trigger automatic read of stored SMS when a subsrciber is 
    %% registered? subscriber need to ack in order to delete the
    %% stored SMS
    ok.


-spec check_csms_capability(Drv::driver()) -> ok.
check_csms_capability(Drv) ->
    case at(Drv,"+CSMS=0") of
	{ok, "+CSMS:"++Storage} ->
	    io:format("sms_pdu: +CSMS: ~s\n", [Storage]);
	Error ->
	    Error
    end.

-spec set_csms_pdu_mode(Drv::driver()) -> ok.

set_csms_pdu_mode(Drv) ->  at(Drv,"+CMGF=0").

%% AT+CNMI=1,1,0,0,0 Set the new message indicators.
%%
%% AT+CNMI=<mode>,<mt>,<bm>,<ds>,<bfr>
%% 
%% <mode>=1 discard unsolicited result codes indication when TA – 
%%          TE link is reserved.
%% <mt>=1 SMS-DELIVERs are delivered to the SIM and routed using 
%%        unsolicited code.
%% <bm>=0 no CBM indications are routed to the TE.
%% <ds>=0 no SMS-STATUS-REPORTs are routed.
%% <bfr>=0 TA buffer of unsolicited result codes defined within this
%%         command is flushed to the TE.
%% OK Modem Response.
set_csms_notification(Drv) -> at(Drv,"+CNMI=1,1,0,0,0").

%% pick up information about various things
get_version(Drv) -> at(Drv,"+CGMR").
    
get_manufacturer(Drv) -> at(Drv,"+CGMI").

get_model(Drv) -> at(Drv,"+CGMM").

get_imei(Drv) -> at(Drv,"+CGSN").

get_msisdn(Drv) -> at(Drv,"+CNUM").

get_imsi(Drv) -> at(Drv,"+CIMI").

get_activity_status(Drv) -> at(Drv,"+CPAS").

get_network_registration_status(Drv) -> at(Drv,"+CREG?").

get_signal_strength(Drv) -> at(Drv,"+CSQ").

get_battery_status(Drv) -> at(Drv,"+CBC").

get_smsc(Drv) -> at(Drv, "+CSCA?").
    

%% SMS commands
list_unread_messages(Drv) ->  at(Drv,"+CMGL=0").
list_read_messages(Drv)   ->  at(Drv,"+CMGL=1").
list_unsent_messages(Drv) ->  at(Drv,"+CMGL=2").
list_sent_messages(Drv)   ->  at(Drv,"+CMGL=3").
list_all_messages(Drv)    ->  at(Drv,"+CMGL=4").

%% message list
list_indices(Drv) ->
    case at(Drv,"+CMGD=?") of
	{ok,"+CMGD:"++Params} ->
	    case erl_scan:string(Params) of
		{ok,Ts,_} ->
		    parse_index_lists(Ts);
		Error ->
		    Error
	    end;
	Error ->
	    Error
    end.


%% +CMGD=I    == +CMDG=I,0   only delete message I
%% +CMGD=I,1  == +CMGD=0,1   delete ALL "read" messages
%% +CMGD=I,2  == +CMGD=0,2   delete ALL "read","sent" messages
%% +CMGD=I,3  == +CMGD=0,3   delete ALL "read","sent", "unsent" messages
%% +CMFD=I,4  == +CMGD=0,4   delete ALL messages

delete_message(Drv,I) when is_integer(I), I>=0 ->
    delete_message(Drv,I,0).

delete_message(Drv,I,F) when is_integer(I), I>=0, F>=0,F=<4 ->
    at(Drv,"+CMGD="++integer_to_list(I)++","++integer_to_list(F)).

delete_all_message(Drv) ->
    delete_message(Drv,1,4).

read_message(Drv,I) when is_integer(I), I>=0 ->
    case at(Drv,"+CMGR="++integer_to_list(I)) of
	ok ->
	    {error, no_such_index};
	{ok,["+CMGR: "++_StatStoreLen,HexPdu]} ->
	    {ok,gsms:decode_in_hex(HexPdu)};
	{error, Error} ->
	    {error, cms_error(Error)}
    end.

read_all_messages(Drv) ->
    at(Drv,"+CMGL=1").

send_message(Drv,Opts,Body) ->
    lists:foreach(
      fun(Pdu) ->
	      gsms:dump_yang(Pdu),
	      Bin = gsms:encode_sms(Pdu),
	      Hex = gsms:binary_to_hex(Bin),
	      Len = (length(Hex)-2) div 2,
	      case ats(Drv,"+CMGS="++integer_to_list(Len)) of
		  ready_to_send -> atd(Drv,Hex);
		  Error -> Error
	      end
      end, gsms:make_sms_submit(Opts, Body)).


write_message(Drv,Opts,Body) ->
    lists:foreach(
      fun(Pdu) ->
	      gsms:dump_yang(Pdu),
	      Bin = gsms:encode_sms(Pdu),
	      Hex = gsms:binary_to_hex(Bin),
	      Len = (length(Hex)-2) div 2,
	      case ats(Drv,"+CMGW="++integer_to_list(Len)) of
		  ready_to_send -> atd(Drv,Hex);
		  Error -> Error
	      end
      end, gsms:make_sms_submit(Opts, Body)).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server.
%%
%% Device contains the path to the Device. <br/>
%% reopen_timeout =/= 0 means that if the driver fails to open the device it
%% will try again in Timeout milliseconds.<br/>
%% Debug controls trace output.<br/>
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link([gsms_options()]) -> 
		   {ok, Pid::pid()} | 
		   ignore | 
		   {error, Error::term()}.
%%
%% HUAWEI: uses device /dev/tty.HUAWEIMobile-Pcui
%% for SMS services to be able to get notifications which are
%% NOT available on /dev/tty.HUAWEIMobile-Modem
%%
start_link(Opts) ->
    lager:info("~p: start_link: args = ~p\n", [?MODULE, Opts]),
    case proplists:get_value(name, Opts) of
	undefined ->
	    gen_server:start_link(?MODULE, Opts, []);
	Name when is_atom(Name) ->
	    Opts1 = proplists:delete(name, Opts),
	    gen_server:start_link({local,Name},?MODULE, Opts1, [])
    end.

%%--------------------------------------------------------------------
%% @doc
%% Stops the server.
%%
%% @end
%%--------------------------------------------------------------------
-spec stop(Drv::pid()) -> ok | {error, Error::term()}.

stop(Drv) ->
    gen_server:call(Drv, stop).

%%--------------------------------------------------------------------
%% @doc
%% Subscribe to sms events.
%%
%% @end
%%--------------------------------------------------------------------
-spec subscribe(Drv::pid(),Pattern::[{atom(),string()}]) ->
		       {ok,reference()} | {error, Error::term()}.
subscribe(Drv,Pattern) ->
    gen_server:call(Drv, {subscribe,self(),Pattern}).


%%--------------------------------------------------------------------
%% @doc
%% Subscribe to sms events.
%%
%% @end
%%--------------------------------------------------------------------
-spec subscribe(Drv::pid()) -> {ok,reference()} | {error, Error::term()}.
subscribe(Drv) ->
    gen_server:call(Drv, {subscribe,self(),[]}).

%%--------------------------------------------------------------------
%% @doc
%% Unsubscribe from sms events.
%%
%% @end
%%--------------------------------------------------------------------
-spec unsubscribe(Drv::pid(),Ref::reference()) -> ok | {error, Error::term()}.
unsubscribe(Drv,Ref) ->
    gen_server:call(Drv, {unsubscribe,Ref}).

%%--------------------------------------------------------------------
%% @doc
%% Enable debug trace of the gsms_drv module
%%
%% @end
%%--------------------------------------------------------------------

debug(Drv,On) when is_boolean(On) ->
    gen_server:call(Drv, {debug, On}).

%%--------------------------------------------------------------------
%% @doc
%% Set various options from uart options to smsc options
%%
%% @end
%%--------------------------------------------------------------------

setopts(Drv,Opts) when is_list(Opts) ->
    gen_server:call(Drv, {setopts, Opts}).

%%--------------------------------------------------------------------
%% @doc
%% Run a raw command
%%
%% @end
%%--------------------------------------------------------------------

%% normal at command
at(Drv,Command) ->
    gen_server:call(Drv, {at,Command}).

%% at command that could lead to data-enter state
ats(Drv,Command) ->
    gen_server:call(Drv, {ats,Command}, 10000).

%% send data in data-enter state
atd(Drv, Hex) ->
    gen_server:call(Drv, {atd,Hex}, 20000).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%%--------------------------------------------------------------------
%% @doc
%% Initializes the server
%%
%% @end
%%--------------------------------------------------------------------
-spec init([gsms_options()]) -> 
		  {ok, Ctx::#ctx{}} |
		  {ok, Ctx::#ctx{}, Timeout::timeout()} |
		  ignore |
		  {stop, Reason::term()}.

init(Opts) ->
    lager:start(),  %% set dependency
    {ok,Trace} = set_trace(proplists:get_bool(debug, Opts), undefined),
    lager:info("~p: init: args = ~p,\n pid = ~p\n", [?MODULE, Opts, self()]),
    Opts1 = normalise_opts(?UART_DEFAULT_OPTS ++ Opts),
    {Uopts0,Opts2} = split_opts(Opts1, uart:options()),
    {Gopts0,Opts3} = split_opts(Opts2, options()),
    case check_options(Uopts0,Gopts0,Opts3) of
	ok ->
	    Uopts1 = proplists:delete(device, Uopts0),
	    Device = case proplists:get_value(device, Uopts0) of
			 undefined ->
			     case os:getenv("GSMS_DEVICE") of
				 false -> "";
				 Name -> Name
			     end;
			 Name -> Name
		     end,
	    S = #ctx { device = Device, 
		       uopts  = Uopts1,
		       opts   = Gopts0,
		       queue  = queue:new(),
		       trace  = Trace },
	    case open(S) of
		{ok, S1} -> {ok, S1};
		Error -> {stop, Error}
	    end;
	Error ->
	    {stop, Error}
    end.

check_options(Uopts,Gopts,[]) ->
    case uart:validate_opts(Uopts) of
	ok -> validate_opts(Gopts);
	Error -> Error
    end;
check_options(_Uopt,_Gopts,Opts) ->
    {error, {unknown_opts,Opts}}.

	    
open(Ctx=#ctx {device = ""}) ->
    lager:debug("open: simulated\n", []),
    {ok, Ctx#ctx { uart=simulated }};

open(Ctx=#ctx {device = Name, uopts=UOpts }) ->
    case uart:open(Name,UOpts) of
	{ok,U} ->
	    lager:debug("open: ~s [~w]: ~p", [Name,UOpts,U]),
	    %% waky, waky ? do not echo
	    uart:send(U, [?ESC,       %% escape if stuck in message send
			  "AT\r\n",   %% empty
			  "AT\r\n",   %% epty
			  "ATZ\r\n",  %% reset
			  "ATE0\r\n", %% echo off
			  "AT\r\n"
			 ]),
	    flush_uart(U),
	    {ok, Ctx#ctx { uart=U }};
	{error, E} when E == eaccess;
			E == enoent ->
	    case proplists:get_value(reopen_timeout, Ctx#ctx.opts,infinity) of
		infinity ->
		    lager:debug("open: Driver not started, reason = ~p.\n",[E]),
		    {error, E};
		Ival ->
		    lager:debug("open: uart could not be opened, will try again"
				" in ~p millisecs.\n", [Ival]),
		    Reopen_timer = erlang:start_timer(Ival, self(), reopen),
		    {ok, Ctx#ctx { reopen_timer = Reopen_timer }}
	    end;
	    
	Error ->
	    lager:debug("open: Driver not started, reason = ~p.\n", 
		 [Error]),
	    Error
    end.

close(Ctx=#ctx {uart = U}) when is_port(U) ->
    lager:debug("close: ~p", [U]),
    uart:close(U),
    {ok, Ctx#ctx { uart=undefined }};
close(Ctx) ->
    {ok, Ctx}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------

handle_call({setopts, Opts},_From, Ctx=#ctx { uart = U}) ->
    lager:debug("setopts ~p", [Opts]),
    Opts1 = normalise_opts(Opts),
    {Uopts0,Opts2} = split_opts(Opts1, uart:options()),
    {Gopts0,Opts3} = split_opts(Opts2, options()),
    case check_options(Uopts0,Gopts0,Opts3) of
	ok ->	
	    Uopts1 = proplists:delete(device, Uopts0),
	    case proplists:get_value(device, Uopts0) of
		Device when is_list(Device), Device =/= Ctx#ctx.device ->
		    Uopts2 = normalise_opts(Ctx#ctx.uopts ++ Uopts1),
		    Ctx1 = close(Ctx),
		    Ctx2 = Ctx1#ctx { uart=undefined, reopen_timer = undefined,
				      device=Device, uopts=Uopts2, 
				      opts=Gopts0 },
		    case open(Ctx2) of
			{ok, Ctx3} -> {reply,ok,Ctx3};
			Error -> {stop, Error, Ctx2}
		    end;
		_ ->
		    case U of
			undefined ->
			    {reply, {error,no_port}, Ctx};
			simulated ->
			    {reply, ok, Ctx#ctx { uopts=Uopts1, opts=Gopts0} };
			_ ->
			    case uart:setopts(U, Uopts1) of
				ok ->
				    {reply, ok, Ctx#ctx { uopts=Uopts1,
							  opts=Gopts0} };
				Error ->
				    {reply, Error, Ctx}
			    end
		    end
	    end;
	Error ->
	    {reply, Error, Ctx}
    end;

handle_call({subscribe,Pid,Pattern},_From,Ctx=#ctx { subs=Subs}) ->
    Mon = erlang:monitor(process, Pid),
    Subs1 = [#subscription { pid = Pid, mon = Mon, pattern = Pattern}|Subs],
    {reply, {ok,Mon}, Ctx#ctx { subs = Subs1}};
handle_call({unsubscribe,Ref},_From,Ctx) ->
    erlang:demonitor(Ref),
    Ctx1 = remove_subscription(Ref,Ctx),
    {reply, ok, Ctx1};
handle_call({debug, On}, _From, Ctx) ->
    case set_trace(On, Ctx#ctx.trace) of
	{ok,Trace} ->
	    {reply, ok, Ctx#ctx { trace = Trace }};
	Error ->
	    {reply, Error, Ctx}
    end;
handle_call(stop, _From, Ctx) ->
    {stop, normal, ok, Ctx};
%% other commands we queue if gsms_drv is busy processing command
handle_call(Call,From,Ctx=#ctx {client = Client}) 
  when Client =/= undefined andalso Call =/= stop ->
    %% Driver is busy ..
    lager:debug("handle_call: Driver busy, store call ~p", [Call]),
    %% set timer already here? probably!
    Q = queue:in({call,Call,From}, Ctx#ctx.queue),
    {noreply, Ctx#ctx { queue = Q }};

handle_call({at,Command},From,Ctx) ->
    lager:debug("handle_call: command ~p", [Command]),
    case Ctx#ctx.uart of
	simulated ->
	    lager:info("simulated output ~p\n", [Command]),
	    {reply, ok, Ctx};
	undefined ->
	    lager:info("~p: No port defined yet.\n", [?MODULE]),
	    {reply, {error,no_port}, Ctx};
	U ->
	    case uart:send(U, ["AT",Command,"\r\n"]) of
		ok ->
		    lager:debug("command: sent"),
		    %% Wait for confirmation
		    Tm=proplists:get_value(reply_timeout,Ctx#ctx.opts,5000),
		    TRef = erlang:start_timer(Tm, self(), reply),
		    {noreply,Ctx#ctx {command = Command,client=From,
				      activity = at,
				      reply = [],
				      reply_timer = TRef}};
		Other ->
		    lager:debug("command: send failed, reason ~p", [Other]),
		    {reply, Other, Ctx}
	    end
    end;

handle_call({ats,Command},From,Ctx) ->
    lager:debug("handle_call: command ~p", [Command]),
    case Ctx#ctx.uart of
	simulated ->
	    lager:info("simulated output ~p\n", [Command]),
	    {reply, ok, Ctx};
	undefined ->
	    lager:info("~p: No port defined yet.\n", [?MODULE]),
	    {reply, {error,no_port}, Ctx};
	U ->
	    uart:setopts(U, [{active,false}]),
	    case uart:send(U, ["AT",Command,"\r\n"]) of
		ok ->
		    lager:debug("command: sent\n", []),
		    %% wait for exacly ">\r\n"
		    uart:setopts(U, [{active,once},{packet,{size,3}}]),
		    %% Wait for confirmation
		    Tm=proplists:get_value(reply_timeout,Ctx#ctx.opts,3000),
		    TRef = erlang:start_timer(Tm, self(), reply),
		    {noreply,Ctx#ctx {command  = Command,client=From,
				      activity = ats,
				      reply = [],
				      reply_timer = TRef}};
		Other ->
		    uart:setopts(U, [{active,true}]),
		    lager:debug("command: send failed, reason ~p", [Other]),
		    {reply, Other, Ctx}
	    end
    end;
%%
%% Send DATA command end with ^Z
%% Hmm how many lines per row ?
%%
handle_call({atd,Hex},From,Ctx) ->
    lager:debug("handle_call: emit ~p", [Hex]),
    case Ctx#ctx.uart of
	simulated ->
	    lager:info("simulated output ~p\n", [Hex]),
	    {reply, ok, Ctx};
	undefined ->
	    lager:info("~p: No port defined yet.\n", [?MODULE]),
	    {reply, {error,no_port}, Ctx};
	U ->
	    case uart:send(U, [Hex,?CTRL_Z]) of
		ok ->
		    lager:debug("command: sent\n", []),
		    %% Wait for confirmation
		    Tm=proplists:get_value(reply_timeout,Ctx#ctx.opts,10000),
		    TRef = erlang:start_timer(Tm, self(), reply),
		    {noreply,Ctx#ctx {command=Hex,client=From,
				      activity = atd,
				      reply = [],
				      reply_timer = TRef}};
		Other ->
		    lager:debug("command: send failed, reason ~p", [Other]),
		    {reply, Other, Ctx}
	    end
    end;    

handle_call(_Request, _From, Ctx) ->
    {reply, {error,bad_call}, Ctx}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Msg::term(), Ctx::#ctx{}) -> 
			 {noreply, Ctx::#ctx{}} |
			 {stop, Reason::term(), Ctx::#ctx{}}.


handle_cast(Cast, Ctx=#ctx {uart = U, client=Client})
  when U =/= undefined, Client =/= undefined ->
    lager:debug("handle_cast: Driver busy, store cast ~p", [Cast]),
    Q = queue:in({cast,Cast}, Ctx#ctx.queue),
    {noreply, Ctx#ctx { queue = Q }};

handle_cast(_Msg, Ctx) ->
    lager:debug("handle_cast: Unknown message ~p", [_Msg]),
    {noreply, Ctx}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @end
%%--------------------------------------------------------------------
-type info()::
	{uart, U::port(), Data::binary()} |
	{uart_error, U::port(), Reason::term()} |
	{uart_closed, U::port()} |
	{timeout, reference(), reply} |
	{timeout, reference(), reopen} |
	{'DOWN',Ref::reference(),process,pid(),Reason::term()}.

-spec handle_info(Info::info(), Ctx::#ctx{}) -> 
			 {noreply, Ctx::#ctx{}} |
			 {stop, Reason::term(), Ctx::#ctx{}}.

handle_info({timeout,TRef,reply}, 
	    Ctx=#ctx {client=Client, reply_timer=TRef}) ->
    lager:debug("handle_info: timeout waiting for port", []),
    gen_server:reply(Client, {error, port_timeout}),
    Ctx1 = Ctx#ctx { reply_timer=undefined, reply=[], client = undefined},
    next_command(Ctx1);

handle_info({uart,U,Data},  Ctx) when U =:= Ctx#ctx.uart ->
    lager:debug("got uart data: ~p\n", [Data]),
    case trim(Data) of
	"" -> %% empty line (may add this later?)
	    {noreply, Ctx};
	">" when Ctx#ctx.activity =:= ats ->
	    uart:setopts(U, [{active,true},{packet,line}]),
	    reply(ready_to_send, Ctx);
	"OK" ->
	    reply(ok, Ctx);
	"ERROR" ->
	    reply(error, Ctx);
	"+CMS ERROR:"++Code ->
	    reply(error, Ctx#ctx { reply=[trimhd(Code)|Ctx#ctx.reply]});
	"+CMTI:"++EventData -> %% new SMS message (stored) arrived
	    Ctx1 = event_notify(trimhd(EventData), Ctx),
	    {noreply, Ctx1};
	"+CMT:"++EventData -> %% new SMS message arrived
	    Ctx1 = event_notify(trimhd(EventData), Ctx),
	    {noreply, Ctx1};
	"+CDSI:"++EventData -> %% SMS status report (stored) arrived
	    Ctx1 = event_notify(trimhd(EventData), Ctx),
	    {noreply, Ctx1};
	"+CDS:"++EventData -> %% SMS status report
	    Ctx1 = event_notify(trimhd(EventData), Ctx),
	    {noreply, Ctx1};
	Reply ->
	    if Ctx#ctx.client =/= undefined ->
		    lager:debug("handle_info: data ~p", [Reply]),
		    {noreply,Ctx#ctx { reply=[Reply|Ctx#ctx.reply]}};
	       true ->
		    lager:debug("handle_info: noreply ~p", [Reply]),
		    {noreply,Ctx}
	    end
    end;

handle_info({uart_error,U,Reason}, Ctx) when U =:= Ctx#ctx.uart ->
    if Reason =:= enxio ->
	    lager:error("uart error ~p device ~s unplugged?", 
			[Reason,Ctx#ctx.device]);
       true ->
	    lager:error("uart error ~p for device ~s", 
			[Reason,Ctx#ctx.device])
    end,
    {noreply, Ctx};

handle_info({uart_closed,U}, Ctx) when U =:= Ctx#ctx.uart ->
    uart:close(U),
    lager:error("uart close device ~s will retry", [Ctx#ctx.device]),
    case open(Ctx#ctx { uart=undefined}) of
	{ok, Ctx1} -> {noreply, Ctx1};
	Error -> {stop, Error, Ctx}
    end;

handle_info({timeout,Ref,reopen}, Ctx) when Ctx#ctx.reopen_timer =:= Ref ->
    case open(Ctx#ctx { uart=undefined, reopen_timer=undefined}) of
	{ok, Ctx1} -> {noreply, Ctx1};
	Error -> {stop, Error, Ctx}
    end;

handle_info({'DOWN',Ref,process,_Pid,_Reason},Ctx) ->
    lager:debug("handle_info: subscriber ~p terminated: ~p", 
	 [_Pid, _Reason]),
    Ctx1 = remove_subscription(Ref,Ctx),
    {noreply, Ctx1};
handle_info(_Info, Ctx) ->
    lager:debug("handle_info: Unknown info ~p", [_Info]),
    {noreply, Ctx}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason::term(), Ctx::#ctx{}) -> 
		       ok.

terminate(_Reason, Ctx) ->
    stop_trace(Ctx#ctx.trace),
    close(Ctx),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process ctx when code is changed
%%
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn::term(), Ctx::#ctx{}, Extra::term()) -> 
			 {ok, NewCtx::#ctx{}}.

code_change(_OldVsn, Ctx, _Extra) ->
    {ok, Ctx}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

stop_trace(undefined) ->
    undefined;
stop_trace(Trace) ->
    lager:stop_trace(Trace),
    undefined.

%% enable/disable module debug trace
set_trace(false, Trace) ->
    Trace1 = stop_trace(Trace),
    lager:set_loglevel(lager_console_backend, info),
    {ok,Trace1};
set_trace(true, undefined) ->
    lager:trace_console([{module,?MODULE}], debug);
set_trace(true, Trace) -> 
    {ok,Trace}.

%% flush uart data messages
flush_uart(U) ->
    flush_uart(U, 250, 50).

flush_uart(U,T0,T1) ->
    receive
	{uart,U,_Data} ->
	    lager:debug("flush: uart ~p\n", [_Data]),
	    flush_uart(U,T1)
    after T0 ->
	    ok
    end.

flush_uart(U,T) ->
    receive
	{uart,U,_Data} ->
	    lager:debug("flush: uart ~p\n", [_Data]),
	    flush_uart(U,T)
    after T ->
	    ok
    end.



reply(Tag, Ctx) ->
    if Ctx#ctx.client =/= undefined ->
	    erlang:cancel_timer(Ctx#ctx.reply_timer),
	    case lists:reverse(Ctx#ctx.reply) of
		[] ->
		    gen_server:reply(Ctx#ctx.client, Tag);
		[Response] ->
		    gen_server:reply(Ctx#ctx.client, {Tag,Response});
		MultiResponse ->
		    gen_server:reply(Ctx#ctx.client, {Tag,MultiResponse})
	    end,
	    Ctx1 = Ctx#ctx { client=undefined, 
			     reply_timer=undefined,
			     activity = none,
			     command = "", reply=[] },
	    next_command(Ctx1);
       true ->
	    {noreply, Ctx}
    end.

next_command(Ctx) ->
    case queue:out(Ctx#ctx.queue) of
	{{value,{call,Call,From}}, Q1} ->
	    case handle_call(Call, From, Ctx#ctx { queue=Q1}) of
		{reply,Reply,Ctx1} ->
		    gen_server:reply(From,Reply),
		    {noreply,Ctx1};
		CallResult ->
		    CallResult
	    end;
	{{value,{cast,Cast}}, Q1} ->
	    handle_cast(Cast, Ctx#ctx { queue=Q1});
	{empty, Q1} ->
	    uart:setopts(Ctx#ctx.uart, [{active,true}]),
	    {noreply, Ctx#ctx { queue=Q1}}
    end.

trimhd([$\s|Cs]) -> trimhd(Cs);
trimhd([$\t|Cs]) -> trimhd(Cs);
trimhd([$\r|Cs]) -> trimhd(Cs);
trimhd([$\n|Cs]) -> trimhd(Cs);
trimhd([0|Cs])   -> trimhd(Cs);
trimhd(Cs) -> Cs.

trimtl(Cs) -> lists:reverse(trimhd(lists:reverse(Cs))).

trim(Cs) -> trimtl(trimhd(Cs)).
	    
unquote([$"|Cs]) ->
    case lists:reverse(Cs) of
	[$"|Cs1] -> lists:reverse(Cs1);
	Cs1 -> Cs1
    end;
unquote(Cs1) -> Cs1.

to_integer(Cs) ->
    try list_to_integer(Cs, 10) of
	Value -> Value
    catch
	error:_ -> Cs
    end.

remove_subscription(Ref, Ctx=#ctx { subs=Subs}) ->
    Subs1 = lists:keydelete(Ref, #subscription.mon, Subs),
    Ctx#ctx { subs = Subs1 }.
    

event_notify(String, Ctx) ->
    Event = case string:tokens(String, ",") of
		[Store,Index] ->
		    [{"store",unquote(Store)},{"index",to_integer(Index)}];
		Items ->
		    [{"items", Items}]
	    end,
    send_event(Ctx#ctx.subs, Event),
    %% send to event listener(s)
    io:format("Event: ~p\n", [Event]),
    Ctx.

send_event([#subscription{pid=Pid,mon=Ref,pattern=Pattern}|Tail], Event) ->
    case match_event(Pattern, Event) of
	true -> Pid ! {tellstick_event,Ref,Event};
	false -> false
    end,
    send_event(Tail,Event);
send_event([],_Event) ->
    ok.

match_event([], _) -> true;
match_event([{Key,ValuePat}|Kvs],Event) ->
    case lists:keyfind(Key, 1, Event) of
	{Key,ValuePat} -> match_event(Kvs, Event);
	_ -> false
    end.

%% split options in two groups {A,B}
%% where A is the group with all keys in Keys B are the rest of the options
split_opts(List, Keys) ->
    {Lists,Rest} = proplists:split(List, Keys),
    List1 =  %% get last element from each list, simulate seq setting
	lists:foldr(fun([],Acc) -> Acc;
		       (L,Acc) -> [lists:last(L)|Acc]
		    end, [], Lists),
    {List1,Rest}.

%% remove duplicate options keep later than earlier options
%% normalise boolean options
normalise_opts([Opt|Opts]) ->
    case Opt of
	Kv={Key,_} ->
	    case proplists:is_defined(Key, Opts) of
		true -> normalise_opts(Opts);
		false -> [Kv|normalise_opts(Opts)]
	    end;
	Key ->
	    case proplists:is_defined(Key, Opts) of
		true -> normalise_opts(Opts);
		false -> [{Key,true}|normalise_opts(Opts)]
	    end
    end;
normalise_opts([]) ->
    [].

validate_opts([{K,V}|Kvs]) ->
    case validate_opt(K,V) of
	true -> validate_opts(Kvs);
	false -> {error,{type_error,K,V}};
	undefined -> {error,{unknown_opt,K}};
	Error -> Error
    end;
validate_opts([]) ->
    ok.

validate_opt(device, Arg) -> is_list(Arg);
validate_opt(reopen_timeout, Arg) -> is_timeout(Arg);
validate_opt(reply_timeout, Arg) -> is_timeout(Arg);
validate_opt(smsc, Arg) -> is_list(Arg);
validate_opt(debug, Arg) -> is_boolean(Arg);
validate_opt(_,_Arg) -> undefined.

is_timeout(T) ->
    (T =:= infinity) orelse
	(is_integer(T) andalso (T>=0)).

parse_index_lists(Ts) ->
    parse_index_lists(Ts, []).

parse_index_lists([{'(',_}|Ts], Acc) ->   parse_index_list(Ts, [], Acc);
parse_index_lists([], Acc) ->   {ok,lists:reverse(Acc)};
parse_index_lists(_Ts, _Acc) ->  {error, {syntax_error, _Ts}}.

parse_index_lists1([{',',_}|Ts], Acc) -> parse_index_lists(Ts, Acc);
parse_index_lists1([], Acc) -> {ok,lists:reverse(Acc)};
parse_index_lists1(_Ts, _Acc) ->  {error, {syntax_error, _Ts}}.

%%
%% ival = <i> '-' <j>
%% ival = <i>
%% ival-list = '(' (ival (',' ival)*) ? ')'
%%
parse_index_list([{integer,_,I},{'-',_},{integer,_,J}|Ts],Iv,Acc) ->
    parse_index_list1(Ts,[{I,J}|Iv],Acc);
parse_index_list([{integer,_,I}|Ts],Iv,Acc) ->
    parse_index_list1(Ts,[I|Iv],Acc);
parse_index_list([{')',_}|Ts],Iv,Acc) ->
    parse_index_lists1(Ts,[lists:reverse(Iv)|Acc]);
parse_index_list(_Ts, _Iv, _Acc) ->
    {error, {syntax_error,_Ts}}.    

parse_index_list1([{',',_},{integer,_,I},{'-',_},{integer,_,J}|Ts],Iv,Acc) ->
    parse_index_list1(Ts,[{I,J}|Iv],Acc);
parse_index_list1([{',',_},{integer,_,I}|Ts],Iv,Acc) ->
    parse_index_list1(Ts,[I|Iv],Acc);
parse_index_list1([{')',_}|Ts],Iv,Acc) ->
    parse_index_lists1(Ts,[lists:reverse(Iv)|Acc]);
parse_index_list1(_Ts,_Iv,_Acc) ->
    {error, {syntax_error,_Ts}}.

cms_error(Code) when is_list(Code) ->
    Code ++ ": " ++ cms_error_string(list_to_integer(Code)).

cms_error_string(300) -> "Phone failure";
cms_error_string(301) -> "SMS service of phone reserved";
cms_error_string(302) -> "Operation not allowed";
cms_error_string(303) -> "Operation not supported";
cms_error_string(304) -> "Invalid PDU mode parameter";
cms_error_string(305) -> "Invalid text mode parameter";
cms_error_string(310) -> "SIM not inserted";
cms_error_string(311) -> "SIM PIN necessary";
cms_error_string(312) -> "PH-SIM PIN necessary";
cms_error_string(313) -> "SIM failure";
cms_error_string(314) -> "SIM busy";
cms_error_string(315) -> "SIM wrong";
cms_error_string(320) -> "Memory failure";
cms_error_string(321) -> "Invalid memory index";
cms_error_string(322) -> "Memory full";
cms_error_string(330) -> "SMSC (message service center) address unknown";
cms_error_string(331) -> "No network service";
cms_error_string(332) -> "Network timeout";
cms_error_string(500) -> "Unknown error";
cms_error_string(512) -> "Manufacturer specific";
cms_error_string(_) -> "Unknown".
