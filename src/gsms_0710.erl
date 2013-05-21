%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2013, Tony Rogvall
%%% @doc
%%%  GSM 07.10 multiplexing support
%%% @end
%%% Created : 21 May 2013 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(gsms_0710).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(subscriber,
	{
	  channel = -1 :: integer(),  %% -1,1,2,3..
	  pid          :: pid(),
	  mon          :: reference()
	}).


-record(state, {
	  drv :: pid(),        %% uart driver pid
	  ref :: reference(),  %% uart subscription ref
	  sub = [] :: [#subscriber{}]
	 }).

-include("gsms_0710.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Opts], []).

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
init([Opts]) ->
    {ok,Pid} = gsms_uart:start_link(Opts),
    {ok,Ref} = gsms_uart:subscribe(Pid),  %% subscribe to all events
    {ok, #state{ drv = Pid,
		 ref = Ref } }.

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
handle_call(start, _From, State) ->
    %% initiate mux control
    Reply = gsms_uart:at(State#state.drv, "+CMUX=0"),
    {reply, Reply, State};

handle_call(switch, _From, State) ->
    Reply = gsms_uart:setopts(State#state.drv, [{packet,basic_0710}]),
    {reply, Reply, State};

handle_call({establish,I}, _From, State) ->
    Address = (I bsl 2) + ?COMMAND + ?NEXTENDED,
    Length  = (0 bsl 1) + ?NEXTENDED,
    Hdr     = <<(?CONTROL_SABM+?CONTROL_P),Address,Length>>,
    FCS     = gsms_fcs:crc(Hdr),
    Reply = gsms_uart:send(State#state.drv, <<?BASIC,Hdr/binary,FCS,?BASIC>>),
    {reply, Reply, State};

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
handle_info(<<?BASIC,Address:8,Control:8,Len:7,1:1,
	      Data:Len/binary,FCS:8,?BASIC>>, State) ->
    State1 = handle_packet(Address,Control,Len,Data,FCS, State),
    {noreply, State1};
handle_info(_Info = <<?BASIC,Address:8,Control:8,L0:7,0:1,L1:8,Rest/binary>>,
	    State) ->
    Len = (L1 bsl 7) + L0,
    case Rest of
	<<Data:Len,FCS:8,?BASIC>> ->
	    State1 = handle_packet(Address,Control,Len,Data,FCS,State),
	    {noreply, State1};
	_ ->
	    io:format("Got info: ~p\n", [_Info]),
	    {noreply, State}
    end;
handle_info(_Info, State) ->
    io:format("Got info: ~p\n", [_Info]),
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

handle_packet(Address,Control,Len,Data,FCS,State) ->
    io:format("Got basic packet: ~p\n", [{Address,Control,Len,Data,FCS}]),
    State.

