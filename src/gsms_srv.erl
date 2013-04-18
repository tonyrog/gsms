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

-export([start/2]).
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([start_huawei/0]).
-export([scan_input/1]).
-export([send/3]).

-include("../include/gsms.hrl").

-type qkey_t()    :: {#gsms_addr{},MRef::integer}.
-type isegment_t() :: {I::integer(),Ix::integer(),Pdu::#gsms_deliver_pdu{}}.
-type qelem_t()   :: {qkey_t(),TRef::reference(),N::integer,[isegment_t()]}.

-type osegment_t() :: {I::integer,N::integer,SRef::reference(),
		       Notify::boolean(),Sender::pid(), 
		       Pdu::#gsms_submit_pdu{}}.

-define(DEFAULT_SEND_DELAY, 0).
-define(DEFAULT_SEGMENT_TIMEOUT, 60000).
-define(DEFAULT_CONCAT_8BIT,     true).
-define(DEFAULT_CONCAT_SEQUENCE, true).
-define(DEFAULT_CONCAT_REF,  0).
-define(DEFAULT_SIMPIN, "").

-type gsms_option() ::
	{simpin, Pin::string()} |
	{bnumber, Number::string()} |
	{attributes, [{Key::atom(),Value::term()}]} |
	{segment_timeout, Timeout::timeout()} |
	{send_delay, Delay::timeout()} |
	{concat_8bit, EightBit :: boolean()} |
	{concat_seq,  Sequential :: boolean()}
	.


-record(state,
	{
	  id :: integer(),          %% id in gsms_router
	  drv :: pid(),             %% pid of the gsms_drv AT driver
	  ref :: reference(),       %% notification reference
	  %% state
	  sending = false :: boolean(),    %% sending message outstanding?
	  inq = []  :: [qelem_t()],
	  outq = [] :: [osegment_t()],
	  concat_ref = ?DEFAULT_CONCAT_REF :: integer(),
	  %% config
	  bnumber = "" :: string(),    %% modem phone number
	  simpin = "" :: string(),     %% SIM pin when needed
	  segment_timeout :: timeout(),%% max wait for segment
	  send_delay :: timeout(),     %% delay between sending segments
	  concat_seq  :: boolean(),    %% concat ref is sequence or random
	  concat_8bit :: boolean(),    %% 8bit or 16bit
	  attributes = [] :: [{atom(),term()}]
	}).

%%%===================================================================
%%% API
%%%===================================================================

start_huawei() ->
    start_link(0, [{device,"/dev/tty.HUAWEIMobile-Pcui"}]).
		
scan_input(Pid) ->
    Pid ! scan_input.

%% -spec send(Pid::pid(), Opts::[send_option()], Body::iolist()) ->
%%		  {ok, Ref::reference()} | {error, Reason::term()}.
%%
%% send options: pdu_options ++ [{notify,boolean()},{ref,ConCatRef}]

send(Pid, Opts, Body) ->
    gen_server:call(Pid, {send, self(), Opts, Body}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start(Options) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------

-spec start(Id::integer(),Opts::[gsms_option()]) ->
		   {ok,pid()} | {error,Reason::term()}.

start(Id, Opts) when is_integer(Id), is_list(Opts) ->
    gsms:start(),
    ChildSpec= {{?MODULE,Id}, {?MODULE, start_link, [Id,Opts]},
		permanent, 5000, worker, [?MODULE]},
    supervisor:start_child(gsms_if_sup, ChildSpec).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Id, Opts) ->
    gen_server:start_link(?MODULE, [Id,Opts], []).

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
init([Id,Opts]) ->
    State0 = #state { simpin = ?DEFAULT_SIMPIN,
		      segment_timeout = ?DEFAULT_SEGMENT_TIMEOUT,
		      send_delay = ?DEFAULT_SEND_DELAY,
		      concat_seq = ?DEFAULT_CONCAT_SEQUENCE,
		      concat_8bit = ?DEFAULT_CONCAT_8BIT },
    {Opts1,State1} = setopts(Opts, State0),
    {ok,Pid} = gsms_drv:start_link(Opts1),
    {ok,Ref} = gsms_drv:subscribe(Pid),  %% subscribe to all events
    {ok, State1#state { id = Id,
			drv = Pid,
			ref = Ref }}.

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
handle_call({send,Sender,Opts,Body}, _From, State) ->
    {CRef,Opts1} = next_concat_ref(Opts,State),
    Notify = proplists:get_bool(notify, Opts1),
    PduList = gsms_codec:make_sms_submit(Opts1, Body),
    N = length(PduList),
    SRef = make_ref(),
    OutQ = State#state.outq ++
	[{I,N,SRef,Notify,Sender,Pdu} || 
	    {Pdu,I} <- lists:zip(PduList, lists:seq(1,N))],
    State1 = sending(OutQ,State),
    %% FIXME: add callback info and mark each segment so that we
    %% enable possiblity to cancel sms and remove segments after 
    %% a failed send.
    {reply, {ok,SRef}, State1#state { concat_ref = CRef}};
handle_call({cancel,SRef}, _From, State) ->
    %% Remove and segments not sent for SRef
    OutQ =
	lists:foldr(
	  fun(E={_I,_N,SRef1,_Notify,_Sender,_Pdu}, Acc) ->
		  if SRef1 =:= SRef -> Acc;
		     true -> [E|Acc]
		  end
	  end, [], State#state.outq),
    {reply, ok, State#state { outq = OutQ }};

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
handle_info({gsms_drv, Pid, up}, State) when State#state.drv =:= Pid ->
    %% gsms_drv up and running 
    %% FIXME: check if sim is locked & unlock if possible
    ok = gsms_drv:check_csms_capability(Pid),
    ok = gsms_drv:set_csms_pdu_mode(Pid),
    ok = gsms_drv:set_csms_notification(Pid),
    BNumber = if State#state.bnumber =:= "" ->
		      case gsms_drv:get_msisdn(Pid) of
			  ok -> "";
			  {ok,B} -> B
		      end;
		 true -> 
		      State#state.bnumber
	      end,
    State1 = State#state { bnumber = BNumber },
    gsms_router:join(BNumber, State1#state.attributes),
    lager:debug("running state = ~p", [State1]),
    {noreply, State};

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
	[{I,N,SRef,Notify,Sender,Pdu}|OutQ] ->
	    gsms_codec:dump_yang(Pdu),
	    Bin = gsms_codec:encode_sms(Pdu),
	    Hex = gsms_codec:binary_to_hex(Bin),
	    Len = (length(Hex)-2) div 2,
	    Reply =
		case gsms_drv:ats(State#state.drv,
				  "+CMGS="++integer_to_list(Len)) of
		    ready_to_send ->
			gsms_drv:atd(State#state.drv,Hex);
		    Error ->
			Error
		end,
	    lager:debug("send status segment ~w of ~w response=~p\n", 
			[I,N,Reply]),
	    %% Fixme handle Reply=error!!! cancel rest of segments etc
	    if I =:= N, Notify -> %% assume ok 
		    Sender ! {gsms_notify, SRef, ok};
	       true ->
		    ok
	    end,
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

setopts(Opts, State) ->
    setopts(Opts, State, []).

setopts([Opt|Opts], State, Opts1) ->
    case Opt of
	{simpin,Pin} when is_list(Pin) ->
	    setopts(Opts, State#state { simpin = Pin }, Opts1);
	{bnumber,BNumber} when is_list(BNumber) ->
	    setopts(Opts, State#state { bnumber = BNumber }, Opts1);
	{attributes,As} when is_list(As) ->
	    setopts(Opts, State#state { attributes = As }, Opts1);
	{segment_timeout,T} when is_integer(T), T >= 0 ->
	    setopts(Opts, State#state { segment_timeout = T }, Opts1);
	{send_delay, T}  when is_integer(T), T >= 0 ->
	    setopts(Opts, State#state { send_delay = T }, Opts1);
	{concat_8bit, B} when is_boolean(B) ->
	    setopts(Opts, State#state { concat_8bit = B }, Opts1);
	{concat_seq, B} when is_boolean(B) ->
	    setopts(Opts, State#state { concat_seq = B }, Opts1);
	_ ->
	    setopts(Opts, State, [Opt|Opts1])
    end;
setopts([], State, Opts1) ->
    {lists:reverse(Opts1), State}.


next_concat_ref(Opts, State) ->
    case lists:keymember(ref,1,Opts) of
	true  -> %% ref is givent in the pdu 
	    {State#state.concat_ref, Opts};
	false ->
	    CRef0 = if State#state.concat_seq ->
			    State#state.concat_ref + 1;
		       true ->
			    random:unifrom(16#10000)-1
		    end,
	    CRef1 = if State#state.concat_8bit ->
			    CRef0 band 16#ff;
		       true ->
			    CRef0 band 16#ffff
		    end,
	    {CRef1, [{ref,CRef1} | Opts]}
    end.

%%
%% Continue send segments or we are done ?
%%
sending([], State) ->
    State#state { sending=false, outq = []};
sending(OutQ, State) ->
    erlang:send_after(State#state.send_delay, self(), send),
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
-spec assemble_sms([isegment_t()]) ->
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
