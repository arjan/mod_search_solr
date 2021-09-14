%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2010-2021 Arjan Scherpenisse
%% @doc Store Zotonic rsc documents in Solr.
-module(solr_store).

-export([put/3, delete/3]).

-include_lib("zotonic_core/include/zotonic.hrl").


put(Id, Context, Solr) ->
    case convert(Id, Context) of
        undefined ->
            nop;
        Doc ->
            Doc1 = lists:filter(fun({_,undefined})->false; ({_,[]})->false; (_)->true end, Doc),
            %%?DEBUG(Doc1),
            esolr:add([{doc, Doc1}], Solr)
    end.


delete(Id, _Context, Solr) ->
    ok = esolr:delete({id, z_convert:to_list(Id)}, Solr).



%% See schema.xml to see which fields need to be put into Solr.
convert(Id, Context) ->
    All = m_rsc:get(Id, Context),

    StrVal = fun(Name) -> z_convert:to_list(first_trans(maps:get(Name, All, <<>>))) end,
    Bool = fun(Name) -> case z_convert:to_binary(maps:get(Name, All, false)) of
                            true -> "1";
                            false -> "0"
                        end
           end,

    Date = fun(Name) -> case maps:get(Name, All) of
                            undefined -> "9999-08-17T12:00:00Z";
                            ?ST_JUTTEMIS -> "9999-08-17T12:00:00Z";
                            {{9999,_,_},{_,_,_}} -> "9999-08-17T12:00:00Z";
                            D -> to_isotime(D)
                        end
           end,

    case pivot_text(Id, Context) of
        {ok, {TA, TB, TC, TD}} ->
            %% Gather all text in the rsc, using the pivot routines.
            AllText = iolist_to_binary([ TA, " ", TB, " ", TC, " ", TD ]),
            IsA = m_rsc:is_a(Id, Context),

            %% Module-based fields (starting with x_)
            ModuleProps = lists:flatten(z_notifier:foldl({solr_props, Id, IsA}, [], Context)),

            %% The returned document.
            []
                ++

                %% Regular rsc fields
                [{F, StrVal(F)} || F <- [<<"id">>, <<"version">>, <<"uri">>, <<"name">>, <<"page_path">>, <<"category_id">>, <<"modifier_id">>, <<"creator_id">>, <<"visible_for">>]]
                ++
                [{F, Bool(F)} || F <- [<<"is_authoritative">>, <<"is_published">>, <<"is_featured">>, <<"is_protected">>]]
                ++
                [{F, Date(F)} || F <- [<<"modified">>, <<"created">>, <<"publication_start">>, <<"publication_end">>, <<"date_start">>, <<"date_end">>]]
                ++

                %% rsc category name
                [{category, z_convert:to_list(C)} || C <- IsA]
                ++
                %% rsc category id
                [{category, z_convert:to_list(m_rsc:name_to_id_check(C, Context))} || C <- IsA]
                ++

                %% Text fields
                [{pivot_title, StrVal(<<"title">>)}]
                ++
                [{F, StrVal(F)} || F <- [<<"title">>, <<"summary">>, <<"body">>]]
                ++

                %% All combined text
                [{text, AllText}]
                ++

                %% Some more fields
                [{F, StrVal(F)} || F <- [<<"first_name">>, <<"surname">>, <<"gender">>, <<"street">>,
                                        <<"city">>, <<"postcode">>, <<"state">>, <<"country">>, <<"geocode">>]]
                ++

                %% Edges
                [{o, z_convert:to_list(C)} || C <- m_edge:objects(Id, Context)]
                ++
                [{s, z_convert:to_list(C)} || C <- m_edge:subjects(Id, Context)]
                ++
                lists:flatten(
                  [[{list_to_atom("s_" ++ atom_to_list(Pred)), z_convert:to_list(Edg)}
                    || Edg <- m_edge:subjects(Id, Pred, Context)]
                   || Pred <- m_edge:subject_predicates(Id, Context)])
                ++
                lists:flatten(
                  [[{list_to_atom("o_" ++ atom_to_list(Pred)), z_convert:to_list(Edg)}
                   || Edg <- m_edge:objects(Id, Pred, Context)]
                  || Pred <- m_edge:object_predicates(Id, Context)])
                ++

                %% Module-based solr_props
                ModuleProps;
        {error, _} ->
            []
    end.


pivot_text(Id, Context0) ->
    Lang = z_pivot_rsc:stemmer_language_config(Context0),
    Context = z_context:set_language(Lang,
                 z_context:set_tz(<<"UTC">>,
                    z_acl:sudo(Context0))),
    case m_rsc:exists(Id, Context) of
        true ->
            RscProps = get_pivot_rsc(Id, Context),
            Vars = #{
                id => Id,
                props => RscProps,
                z_language => Lang
            },
            case z_template_compiler_runtime:map_template({cat, <<"pivot/pivot.tpl">>}, Vars, Context) of
                {ok, Template} ->
                    TextA = render_block(a, Template, Vars, Context),
                    TextB = render_block(b, Template, Vars, Context),
                    TextC = render_block(c, Template, Vars, Context),
                    TextD = render_block(d, Template, Vars, Context),
                    {ok, {TextA, TextB, TextC, TextD}};
                {error, enoent} ->
                    lager:error("[~p] Missing 'pivot/pivot.tpl' template", [z_context:site(Context)]),
                    ok
            end;
        false ->
            {error, eexist}
    end.

render_block(Block, Template, Vars, Context) ->
    {Output, _RenderState} = z_template:render_block_to_iolist(Block, Template, Vars, Context),
    iolist_to_binary(Output).

get_pivot_rsc(Id, Context) ->
    case z_db:qmap_props_row(
        "select * from rsc where id = $1",
        [ Id ],
        [ {keys, binary} ],
        Context)
    of
        {ok, FullRecord} ->
            z_notifier:foldl(#pivot_rsc_data{ id = Id }, FullRecord, Context);
        {error, _} ->
            undefined
    end.


%% @doc Get the first translation from a text for use in a string field.
first_trans(#trans{ tr = [] }) ->
    <<>>;
first_trans(#trans{ tr = [ {_, Text} | _ ] }) ->
    Text;
first_trans(X) ->
    X.


to_isotime(DateTime={D,_}) when D =< {1970, 1, 1}->
    z_convert:to_list(z_dateformat:format(DateTime, "Y-m-d\\TH:i:s\\Z", en));
to_isotime(DateTime) ->
    z_convert:to_list(z_dateformat:format(hd(calendar:local_time_to_universal_time_dst(DateTime)), "Y-m-d\\TH:i:s\\Z", en)).
