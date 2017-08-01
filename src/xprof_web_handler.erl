-module(xprof_web_handler).

-export([init/2,terminate/3]).

-behavior(cowboy_handler).

%% In case an XHR receives no content with no content-type Firefox will emit
%% the following error: "XML Parsing Error: no root element found..."
%% As a workaround always return a content-type of octet-stream with
%% 204 No Content responses
-define(HDR_NO_CONTENT, #{<<"Content-type">> => <<"application/octet-stream">>}).

%% Cowboy callbacks

init(Req, State) ->
    What = cowboy_req:binding(what, Req),
    handle_req(What, Req, State).

terminate(_Reason, _Req, _State) ->
    ok.

%% Private

%% Handling different HTTP requests

handle_req(<<"funs">>, Req, State) ->
    Query = cowboy_req:match_qs([{<<"query">>, [], <<"">>}], Req),

    Funs = xprof_vm_info:get_available_funs(maps:get(<<"query">>, Query)),
    Json = jsone:encode(Funs),

    lager:debug("Returning ~b functions matching phrase \"~s\"",
                [length(Funs), Query]),

    Req2 = cowboy_req:reply(200,
                            #{<<"content-type">> =>
                              <<"application/json">>},
                            Json,
                            Req),
    {ok, Req2, State};

handle_req(<<"mon_start">>, Req, State) ->
    Query = get_query(Req),
    lager:info("Starting monitoring via web on '~s'~n", [Query]),

    ResReq =
        case xprof_tracer:monitor(Query) of
            ok ->
                cowboy_req:reply(204, ?HDR_NO_CONTENT, Req);
            {error, already_traced} ->
                cowboy_req:reply(204, ?HDR_NO_CONTENT, Req);
            _Error ->
                cowboy_req:reply(400, Req)
        end,
    {ok, ResReq, State};


handle_req(<<"mon_stop">>, Req, State) ->
    MFA = {M,F,A} = get_mfa(Req),

    lager:info("Stopping monitoring via web on ~w:~w/~w~n",[M,F,A]),

    xprof_tracer:demonitor(MFA),
    ResReq = cowboy_req:reply(204, ?HDR_NO_CONTENT, Req),
    {ok, ResReq, State};

handle_req(<<"mon_get_all">>, Req, State) ->
    Funs = xprof_tracer:all_monitored(),
    FunsArr = [[Mod, Fun, Arity, Query]
               || {{Mod, Fun, Arity}, Query} <- Funs],
    Json = jsone:encode(FunsArr),
    ResReq = cowboy_req:reply(200,
                              #{<<"content-type">> =>
                                <<"application/json">>},
                              Json, Req),
    {ok, ResReq, State};

handle_req(<<"data">>, Req, State) ->
    MFA = get_mfa(Req),
    LastTS = maps:get(<<"last_ts">>, cowboy_req:match_qs([<<"last_ts">>], Req), <<"0">>),

    ResReq =
        case xprof_tracer:data(MFA, binary_to_integer(LastTS)) of
            {error, not_found} ->
                cowboy_req:reply(404, Req);
            Vals ->
                Json = jsone:encode([{Val} || Val <- Vals]),

                cowboy_req:reply(200,
                                 #{<<"content-type">> =>
                                   <<"application/json">>},
                                 Json, Req)
        end,
    {ok, ResReq, State};

handle_req(<<"trace_set">>, Req, State) ->
    Spec = maps:get(<<"spec">>, cowboy_req:match_qs([<<"spec">>], Req)),

    ResReq = case lists:member(Spec, [<<"all">>, <<"pause">>]) of
                 true ->
                     xprof_tracer:trace(list_to_atom(binary_to_list(Spec))),
                     cowboy_req:reply(204, ?HDR_NO_CONTENT, Req);
                 false ->
                     lager:info("Wrong spec for tracing: ~p",[Spec]),
                     cowboy_req:reply(400, Req)
             end,
    {ok, ResReq, State};

handle_req(<<"trace_status">>, Req, State) ->
    {_, Status} = xprof_tracer:trace_status(),
    Json = jsone:encode({[{status, Status}]}),
    ResReq = cowboy_req:reply(200,
                              #{<<"content-type">> =>
                                <<"application/json">>},
                              Json, Req),

    {ok, ResReq, State};

handle_req(<<"capture">>, Req, State) ->
    MFA = {M,F,A} = get_mfa(Req),
    ThresholdStr = maps:get(<<"threshold">>, cowboy_req:match_qs([<<"threshold">>], Req)),
    LimitStr = maps:get(<<"limit">>, cowboy_req:match_qs([<<"limit">>], Req)),
    Threshold = binary_to_integer(ThresholdStr),
    Limit = binary_to_integer(LimitStr),

    lager:info("Capture ~b calls to ~w:~w/~w~n exceeding ~b ms",
               [Limit, M, F, A, Threshold]),

    {ok, CaptureId} = xprof_tracer_handler:capture(MFA, Threshold, Limit),
    Json = jsone:encode({[{capture_id, CaptureId}]}),

    ResReq = cowboy_req:reply(200,
                              #{<<"content-type">> =>
                                <<"application/json">>}, Json, Req),
    {ok, ResReq, State};

handle_req(<<"capture_stop">>, Req, State) ->
    MFA = get_mfa(Req),

    lager:info("Stopping slow calls capturing for ~p", [MFA]),

    ResReq =
        case xprof_tracer_handler:capture_stop(MFA) of
            ok ->
                cowboy_req:reply(204, ?HDR_NO_CONTENT, Req);
            {error, not_found} ->
                cowboy_req:reply(404, Req)
        end,
    {ok, ResReq, State};

handle_req(<<"capture_data">>, Req, State) ->
    MFA  = get_mfa(Req),
    OffsetStr = maps:get(<<"offset">>, cowboy_req:match_qs([<<"offset">>], Req)),
    Offset = binary_to_integer(OffsetStr),

    ResReq =
        case xprof_tracer_handler:get_captured_data(MFA, Offset) of
            {error, not_found} ->
                cowboy_req:reply(404, Req);
            {ok, {Id, Threshold, Limit, OriginalLimit}, Items} ->
                ModeCb = xprof_lib:get_mode_cb(),
                ItemsJson = [{args_res2proplist(Item, ModeCb)} || Item <- Items],
                Json = jsone:encode({[{capture_id, Id},
                                      {threshold, Threshold},
                                      {limit, OriginalLimit},
                                      {items, ItemsJson},
                                      {has_more, Offset + length(Items) < Limit}]}),
                cowboy_req:reply(200,
                                 #{<<"content-type">> =>
                                   <<"application/json">>},
                                 Json, Req)
        end,
    {ok, ResReq, State};

handle_req(<<"mode">>, Req, State) ->
    Mode = xprof_lib:get_mode(),
    Json = jsone:encode({[{mode, Mode}]}),
    ResReq = cowboy_req:reply(200,
                              #{<<"content-type">> =>
                                <<"application/json">>},
                              Json, Req),
    {ok, ResReq, State}.

%% Helpers

-spec get_mfa(cowboy:req()) -> xprof:mfa_id().
get_mfa(Req) ->
    Params = cowboy_req:parse_qs(Req),
    {list_to_atom(binary_to_list(maps:get(<<"mod">>, Params))),
     list_to_atom(binary_to_list(maps:get(<<"fun">>, Params))),
     case maps:get(<<"arity">>, Params) of
         <<"_">> -> '_';
         Arity -> binary_to_integer(Arity)
     end}.

-spec get_query(cowboy:req()) -> string().
get_query(Req) ->
    Params = cowboy_req:parse_qs(Req),
    binary_to_list(maps:get(<<"query">>, Params)).

args_res2proplist([Id, Pid, CallTime, Args, Res], ModeCb) ->
    [{id, Id},
     {pid, ModeCb:fmt_term(Pid)},
     {call_time, CallTime},
     {args, ModeCb:fmt_term(Args)},
     {res, ModeCb:fmt_term(Res)}].
