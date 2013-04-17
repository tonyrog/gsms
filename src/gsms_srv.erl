%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%    SMS service using gsms_drv to send and receive messages
%%% @end
%%% Created : 24 Oct 2012 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(gsms_srv).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([start_huawei/0]).
-export([scan_input/1]).
-export([send/3]).

-include("../include/gsms.hrl").

-type qkey_t()    :: {#gsms_addr{},MRef::integer}.
-type segment_t() :: {I::integer(),Ix::integer(),Sms::#gsms_deliver_pdu{}}.
-type qelem_t()   :: {qkey_t(),TRef::reference(),N::integer,[segment_t()]}.

-record(state,
	{
	  drv :: pid(),             %% pid of the gsms_drv AT driver
	  ref :: reference(),       %% notification reference
	  bnumber :: string(),      %% modem phone number
	  segment_timeout = 60000 :: integer,  %% max wait for segment
	  concat_ref :: 0..255,     %% concat reference number (maybe random?) 
	  pin,                      %% SIM pin when needed
	  sending = false,          %% sending message outstanding?
	  inq = [] :: [qelem_t()],
	  outq = [] :: [#gsms_submit_pdu{}]
	}).

%%%===================================================================
%%% API
%%%===================================================================

start_huawei() ->
    start_link([{device,"/dev/tty.HUAWEIMobile-Pcui"}]).
		
scan_input(Pid) ->
    Pid ! scan_input.

send(Pid, Opts, Body) ->
    gen_server:call(Pid, {send, Opts, Body}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(Opts) ->
    Pin      = proplists:get_value(pin, Opts),
    BNumber0 = proplists:get_value(bnumber, Opts),
    Attributes = proplists:get_value(attributes, Opts, []),
    Opts1 = lists:foldl(fun(P,Ps) -> proplists:delete(P,Ps) end, 
			Opts, [pin,bnumber,attributes]),
    {ok,Pid} = gsms_drv:start_link(Opts1),
    {ok,Ref} = gsms_drv:subscribe(Pid),  %% subscribe to all evenet
    %% FIXME: check if sim is locked & unlock if possible
    ok = gsms_drv:check_csms_capability(Pid),
    ok = gsms_drv:set_csms_pdu_mode(Pid),
    ok = gsms_drv:set_csms_notification(Pid),
    BNumber = if BNumber0 =:= undefined ->
		      case gsms_drv:get_msisdn(Pid) of
			  ok ->  "";
			  {ok,B} -> B
		      end;
		 true -> BNumber0
	      end,
    State = #state { drv = Pid,
		     ref = Ref,
		     pin = Pin,
		     bnumber = BNumber },
    gsms_router:join(BNumber, Attributes),
    lager:debug("init state = ~p", [State]),
    {ok,State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({send,Opts,Body}, _From, State) ->
    CRef = (State#state.concat_ref + 1) band 255,
    Opts1 = case lists:keymember(ref, Opts) of
		false -> [{ref,CRef} | Opts];
		true -> Opts
	    end,
    PduList = gsms_codec:make_sms_submit(Opts1, Body),
    OutQ = State#state.outq ++ PduList,
    State1 = sending(OutQ,State),
    {reply, ok, State1#state { concat_ref = CRef}};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({gsms_event,Ref,Event}, State) when State#state.ref =:= Ref ->
    case Event of
	[{"store",_Name},{"index", Ix}] ->
	    %% read a stored message
	    case gsms_drv:read_message(State#state.drv, Ix) of
		{ok, Sms} ->
		    lager:debug("read_message: ~p", [Sms]),
		    State1 = handle_sms(Sms, Ix, State),
		    {noreply, State1};
		Error ->
		    lager:info("read_message failed: ~p\n", [Error]),
		    {noreply, State}
	    end;
	_ ->
	    lager:info("event ignored ~p\n", [Event]),
	    {noreply, State}	    
    end;
handle_info({timeout,TRef,{cancel,Key}}, State) ->
    %% reject incoming message because of timeout
    Q = State#state.inq,
    case lists:keytake(Key, 1, Q) of
	false ->
	    lager:warning("message from ~p not present in timeout", [Key]),
	    {noreply, State};
	{value,{_,TRef,_N,Segments},Q0} ->
	    lager:warning("message from ~p dropped, timeout", [Key]),
	    lists:foreach(
	      fun({_I,Ix,_}) ->
		      gsms_drv:delete_message(State#state.drv, Ix)
	      end, Segments),
	    {noreply, State#state {inq = Q0}}
    end;
handle_info(scan_input, State) ->
    %% scan_input should be run after startup to get buffered
    %% messages stored in SIM card while application was down
    case gsms_drv:list_indices(State#state.drv) of
	{ok,[Ixs | _]} ->
	    Pid = self(),
	    Ref = State#state.ref,
	    lists:foreach(
	      fun(Ix) ->
		      Pid ! {gsms_event,Ref,[{"store","SM"},{"index",Ix}]}
	      end, Ixs),
	    {noreply, State};
	Reply ->
	    lager:debug("list_indices reply ~p", [Reply]),
	    {noreply, State}
    end;
handle_info(send, State) ->
    %% send next PDU 
    case State#state.outq of
	[Pdu|OutQ] ->
	    gsms_codec:dump_yang(Pdu),
	    Bin = gsms_codec:encode_sms(Pdu),
	    Hex = gsms_codec:binary_to_hex(Bin),
	    Len = (length(Hex)-2) div 2,
	    case gsms_drv:ats(State#state.drv,"+CMGS="++integer_to_list(Len)) of
		ready_to_send -> gsms_drv:atd(State#state.drv,Hex);
		Error -> Error
	    end,
	    %% handle error
	    {noreply, sending(OutQ, State)};
	[] ->
	    {noreply, State#state { sending = false }}
    end;
handle_info(_Info, State) ->
    {noreply, State}.    

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

sending([], State) ->
    State#state { sending=false, outq = []};
sending(OutQ, State) ->
    self() ! send,  %% send async soon (could space with send_after!)
    State#state { sending=true, outq = OutQ }.


handle_sms(Sms, Ix, State) ->
    %% check if this is Sms is part of an concatenated message
    case lists:keyfind(concat, 1, Sms#gsms_deliver_pdu.udh) of
	false -> %% singleton message, forward
	    forward_sms(Sms, [Ix], State);
	{concat,_MRef,1,1} -> %% singleton message, forward !
	    forward_sms(Sms, [Ix], State);
	{concat,MRef,N,I} when I > 0, I =< N ->
	    %% check if we already have segments stored
	    Key = {Sms#gsms_deliver_pdu.addr,MRef},
	    Q = State#state.inq,
	    Tmo = State#state.segment_timeout,
	    case lists:keytake(Key, 1, Q) of
		false -> %% new enqueue
		    TRef = start_timer(Tmo,{cancel,Key}),
		    Segments = [{I,Ix,Sms}],
		    Q1 = [{Key,TRef,N,Segments} | Q],
		    State#state { inq = Q1 };
		{value,{_,TRef,N,Segments},Q0} ->
		    case lists:keymember(I,1,Segments) of
			false ->
			    stop_timer(TRef),
			    Segments1 = [{I,Ix,Sms}|Segments],
			    case length(Segments1) of
				N ->
				    {Sms1,Ixs} = assemble_sms(Segments1),
				    State1 = State#state { inq=Q0},
				    forward_sms(Sms1,Ixs,State1);
				_ ->
				    TRef1 = start_timer(Tmo,{cancel,Key}),
				    Q1 = [{Key,TRef1,N,Segments1} | Q0],
				    State#state { inq=Q1 }
			    end;
			true ->
			    lager:warning("segment ~w already received!", [I]),
			    State
		    end
	    end;
	Concat ->
	    lager:warning("bad concat udh element ~p", [Concat]),
	    State
    end.

%%
%% Send sms to gsms_router & delete stored segments
%%
forward_sms(Sms, Ixs, State) ->
    gsms_router:input_from(State#state.bnumber, Sms),
    lists:foreach(
      fun(Ix) ->
	      gsms_drv:delete_message(State#state.drv, Ix)
      end, Ixs),
    State.

%% assemble sms segments into one message,
%% assume all I 1..N are present (by pigeon hole principle)
-spec assemble_sms([segment_t()]) ->
			  {#gsms_deliver_pdu{}, [Ix::integer()]}.
		      
assemble_sms(Segments) ->
    [{1,Ix,Sms} | Segments1] = lists:keysort(1, Segments),
    Udh0 = lists:keydelete(concat, 1, Sms#gsms_deliver_pdu.udh),
    Sms0 = Sms#gsms_deliver_pdu { udh=Udh0, udl=0, ud=[] },
    assemble_(Segments1,
	     [Sms#gsms_deliver_pdu.ud], Sms#gsms_deliver_pdu.udl,
	     [Ix], Sms0).

assemble_([{_,Ix,Sms}|Segments], Uds, Udl, Ixs, Sms0) ->
    assemble_(Segments,
	     [Sms#gsms_deliver_pdu.ud|Uds], Sms#gsms_deliver_pdu.udl + Udl,
	     [Ix|Ixs], Sms0);
assemble_([], Uds, Udl, Ixs, Sms0) ->
    Ud = lists:append(lists:reverse(Uds)),
    {Sms0#gsms_deliver_pdu { udl = Udl, ud = Ud }, lists:reverse(Ixs)}.
	     

start_timer(Time, Message) ->
    erlang:start_timer(Time, self(), Message).

stop_timer(undefined) ->
    ok;
stop_timer(Ref) ->
    erlang:cancel_timer(Ref),
    receive
	{timeout, Ref, _} -> 
	    ok
    after 0 -> 
	    ok
    end.
