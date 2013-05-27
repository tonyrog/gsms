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
-export([establish/2, release/2, send/3]).
-export([closedown/1, send_test/3]).

-export([test/0, setup/1]).

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

test() ->
    {ok,Pid} = start_link([{device,"/dev/tty.usbserial-FTF5DP2J"},
			   {baud,115200}]),
			   %% {debug, debug}]),
    setup(Pid).

setup(Pid) ->
    timer:sleep(3000),   %% up ? mux? fixme
    R = establish(Pid, 0),  %% establish multiplexer channel
    {R, Pid}.

establish(Pid, I) ->
    gen_server:call(Pid, {establish,I}).

release(Pid, I) ->
    gen_server:call(Pid, {release,I}).

closedown(Pid) ->
    L = make_length(0),
    send(Pid, 0, <<(?TYPE_CLD + ?COMMAND), L/binary>>).

send_test(Pid, I, Data) ->
    Data1 = iolist_to_binary(Data),
    L = make_length(byte_size(Data1)),
    send(Pid, I, <<(?TYPE_TEST + ?COMMAND),L/binary, Data1/binary>>).
    
send(Pid,I,Data) ->
    gen_server:call(Pid, {send,I,Data}).

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

handle_call({send,I,Data}, _From, State) ->
    Control = (?CONTROL_UIH),
    Address = (I bsl 2) + ?COMMAND + ?NEXTENDED,
    try iolist_to_binary(Data) of
	Data1 ->
	    Len = make_length(byte_size(Data1)),
	    Hdr = <<Address,Control,Len/binary>>,
	    FCS   = gsms_fcs:crc(Hdr),
	    Reply = gsms_uart:send(State#state.drv, <<?BASIC,Hdr/binary,
						      Data1/binary,
						      FCS,
						      ?BASIC>>),
	    {reply, Reply, State}
    catch
	error:_ -> %% crash client
	    {reply, {error,badarg}, State}
    end;
handle_call({establish,I}, _From, State) ->
    Control = (?CONTROL_SABM+?CONTROL_P),
    Address = (I bsl 2) + ?COMMAND + ?NEXTENDED,
    Length  = 0,
    Hdr     = <<Address,Control,Length:7,1:1>>,
    FCS     = gsms_fcs:crc(Hdr),
    Reply = gsms_uart:send(State#state.drv, <<?BASIC,Hdr/binary,FCS,?BASIC>>),
    %% wait for DM | UA
    {reply, Reply, State};

handle_call({release,I}, _From, State) ->
    Control = (?CONTROL_DISC+?CONTROL_P),
    Address = (I bsl 2) + ?COMMAND + ?NEXTENDED,
    Length  = 0,
    Hdr     = <<Address,Control,Length:7,1:1>>,
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
handle_info({gsms_event,_Ref,
	     {data,<<?BASIC,Address:8,Control:8,Len:7,1:1,
		     Data:Len/binary,FCS:8,?BASIC>>}}, State) ->
    Valid = if Control =:= ?CONTROL_UIH ->
		    gsms_fcs:is_valid(<<Address,Control,Len:7,1:1>>,FCS);
	       Control =:= ?CONTROL_UI ->
		    gsms_fcs:is_valid([<<Address,Control,Len:7,1:1>>,Data],FCS);
	       true ->
		    unchecked
	    end,
    State1 = handle_packet(Address,Control,Len,Data,FCS,Valid,State),
    {noreply, State1};
handle_info(_I={gsms_event,_Ref,
		{data,<<?BASIC,Address:8,Control:8,L0:7,0:1,L1:8,
			Rest/binary>>}},
	    State) ->
    Len = (L1 bsl 7) + L0,
    case Rest of
	<<Data:Len,FCS:8,?BASIC>> ->
	    Valid = if Control =:= ?CONTROL_UIH ->
			    gsms_fcs:is_valid(<<Address,Control,L0:7,0:1,L1>>,
					      FCS);
		       Control =:= ?CONTROL_UI ->
			    gsms_fcs:is_valid([<<Address,Control,L0:7,0:1,L1>>,
					       Data],FCS);
		       true ->
			    unchecked
		    end,
	    State1 = handle_packet(Address,Control,Len,Data,FCS,Valid,State),
	    {noreply, State1};
	_ ->
	    io:format("Got info: ~p\n", [_I]),
	    {noreply, State}
    end;
handle_info({gsms_uart,Pid,up}, State) ->
    ok = gsms_uart:at(Pid,"E0"),       %% disable echo (again)
    timer:sleep(100),                  %% help?
    case gsms_uart:at(Pid,"#SELINT=2") of
	ok ->
	    io:format("#SELINT=2 result =~p\n", [ok]),
	    ok = gsms_uart:at(Pid,"V1&K3&D2"),
	    ok = gsms_uart:at(Pid,"+IPR=115200"),
	    Cmux = gsms_uart:at(Pid,"+CMUX=?"),
	    io:format("Cmux = ~p\n", [Cmux]),
	    CmuxMode = gsms_uart:at(Pid,"#CMUXMODE=?"),
	    io:format("CmuxMode = ~p\n", [CmuxMode]),
	    gsms_uart:at(Pid,"#CMUXMODE=1");
	Sel ->
	    io:format("#SELINT=2 result =~p\n", [Sel]),
	    ok
    end,
    ok = gsms_uart:at(Pid,"+CMUX=0"),  %% enable CMUX basic mode
    ok = gsms_uart:setopts(Pid, [{packet,basic_0710},{mode,binary},
				 {active,true}]),
    {noreply, State};
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

handle_packet(Address,Control,Len,Data,FCS,Valid,State) ->
    io:format("Got basic packet:(~w) ~p\n", 
	      [Valid,{Address,Control,Len,Data,FCS}]),
    State.

make_length(N) when N =< 16#7f ->
    <<((N bsl 1) + 1)>>;
make_length(N) when N =< 16#7fff ->
    L0 = N band 16#7f,
    L1 = N bsr 7,
    <<L0:7,1:0,L1>>.
