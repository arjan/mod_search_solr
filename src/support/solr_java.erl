-module(solr_java).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).

-include_lib("zotonic_core/include/zotonic.hrl").
-define(DEFAULT_RESTART_TIMEOUT, 3000).

-record(state, {script, port=undefined, timeout=?DEFAULT_RESTART_TIMEOUT}).



%% Module API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server callbacks

init([]) ->
    process_flag(trap_exit, true),
    Script = filename:join([ code:priv_dir(zotonic_mod_search_solr) , "solr", "start.sh" ]),
    {ok, #state{script=Script}, 0}.

handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.

handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.

handle_info(timeout, State) ->
    {noreply, restart_worker(State)};

handle_info({Port, {exit_status, _N}}, State=#state{port=Port, timeout=T}) ->
    %% Restart after T ms
    {noreply, State, T};

handle_info({Port, {data, {eol, Line}}}, State=#state{port=Port}) ->
    lager:info("~s", [Line]), 
    {noreply, State};

handle_info({'EXIT', Port, normal}, State=#state{port=Port, timeout=T}) ->
    {noreply, State, T};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.




restart_worker(State) ->
    %% Kill any existing solr process first..
    os:cmd("pkill -f solr.home"),

    Port = erlang:open_port({spawn_executable, State#state.script}, 
                            [{line, 1024}, stderr_to_stdout, exit_status]),
    State#state{port=Port}.
