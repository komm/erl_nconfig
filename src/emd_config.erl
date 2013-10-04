-module(emd_config).
-export([parse/1,file/1]).
-compile(nowarn_unused_vars).
-compile({nowarn_unused_function,[p/4, p/5, p_eof/0, p_optional/1, p_not/1, p_assert/1, p_seq/1, p_and/1, p_choose/1, p_zero_or_more/1, p_one_or_more/1, p_label/2, p_string/1, p_anything/0, p_charclass/1, line/1, column/1]}).



file(Filename) -> {ok, Bin} = file:read_file(Filename), parse(Bin).

parse(List) when is_list(List) -> parse(list_to_binary(List));
parse(Input) when is_binary(Input) ->
  setup_memo(),
  Result = case 'config'(Input,{{line,1},{column,1}}) of
             {AST, <<>>, _Index} -> AST;
             Any -> Any
           end,
  release_memo(), Result.

'config'(Input, Index) ->
  p(Input, Index, 'config', fun(I,D) -> (p_zero_or_more(p_choose([fun 'freeline'/2, fun 'crlf'/2, fun 'section'/2, fun 'commandline'/2, fun 'comment'/2, fun 'freeline'/2, fun 'crlf'/2])))(I,D) end, fun(Node, Idx) -> 
    [X || X <- lists:flatten(Node), X /= {none}, X /= {crlf}]
 end).

'section'(Input, Index) ->
  p(Input, Index, 'section', fun(I,D) -> (p_seq([p_zero_or_more(p_choose([fun 'freeline'/2, fun 'space'/2])), p_label('block', fun 'word'/2), p_zero_or_more(p_choose([fun 'freeline'/2, fun 'space'/2])), p_string(<<"{">>), p_zero_or_more(p_choose([fun 'freeline'/2, fun 'space'/2])), p_zero_or_more(fun 'crlf'/2), p_zero_or_more(p_choose([p_seq([p_zero_or_more(p_choose([fun 'freeline'/2, fun 'space'/2])), p_label('cwd', fun 'commandline'/2)]), fun 'comment'/2, p_seq([p_label('sec', fun 'section'/2), p_zero_or_more(fun 'crlf'/2)])])), p_string(<<"}">>), p_zero_or_more(p_choose([fun 'freeline'/2, fun 'space'/2])), p_zero_or_more(fun 'crlf'/2)]))(I,D) end, fun(Node, Idx) -> 
    {binary_to_atom(proplists:get_value(block, Node), latin1),
        [X || {cwd,X} <-lists:flatten(Node) ] ++
        [ X|| {sec, X} <- lists:flatten(Node)]
    }

 end).

'commandline'(Input, Index) ->
  p(Input, Index, 'commandline', fun(I,D) -> (p_seq([p_zero_or_more(p_choose([fun 'freeline'/2, fun 'space'/2])), p_label('comm', fun 'command'/2), p_zero_or_more(fun 'space'/2), p_string(<<"=">>), p_zero_or_more(fun 'space'/2), p_choose([p_label('param', fun 'argv'/2), p_label('param', fun 'parameter'/2)]), p_zero_or_more(fun 'space'/2), fun 'delimeter'/2]))(I,D) end, fun(Node, Idx) -> 
    {
        binary_to_atom(proplists:get_value(comm,Node),latin1),
        binary_to_atom(proplists:get_value(param,Node),latin1)
    }
 end).

'freeline'(Input, Index) ->
  p(Input, Index, 'freeline', fun(I,D) -> (p_choose([p_seq([p_zero_or_more(fun 'space'/2), fun 'comment'/2]), p_seq([p_zero_or_more(fun 'space'/2), fun 'crlf'/2])]))(I,D) end, fun(Node, Idx) -> 
    {none}
 end).

'command'(Input, Index) ->
  p(Input, Index, 'command', fun(I,D) -> (fun 'word'/2)(I,D) end, fun(Node, Idx) -> Node end).

'parameter'(Input, Index) ->
  p(Input, Index, 'parameter', fun(I,D) -> (p_choose([fun 'ipv'/2, fun 'path'/2, fun 'word'/2]))(I,D) end, fun(Node, Idx) -> Node end).

'path'(Input, Index) ->
  p(Input, Index, 'path', fun(I,D) -> (p_seq([p_optional(p_seq([fun 'word'/2, p_string(<<":">>)])), p_zero_or_more(p_string(<<"\/">>)), p_zero_or_more(p_seq([fun 'word'/2, p_optional(p_seq([p_string(<<":">>), fun 'word'/2])), p_optional(p_string(<<"\/">>))])), p_optional(fun 'word'/2)]))(I,D) end, fun(Node, Idx) -> 
    iolist_to_binary(Node)
 end).

'ipv'(Input, Index) ->
  p(Input, Index, 'ipv', fun(I,D) -> (p_seq([p_one_or_more(p_charclass(<<"[0-9]">>)), p_string(<<".">>), p_one_or_more(p_charclass(<<"[0-9]">>)), p_string(<<".">>), p_one_or_more(p_charclass(<<"[0-9]">>)), p_string(<<".">>), p_one_or_more(p_charclass(<<"[0-9]">>)), p_optional(p_seq([p_string(<<":">>), p_one_or_more(p_charclass(<<"[0-9]">>))]))]))(I,D) end, fun(Node, Idx) -> 
    iolist_to_binary(Node)
 end).

'word'(Input, Index) ->
  p(Input, Index, 'word', fun(I,D) -> (p_one_or_more(p_charclass(<<"[a-zA-Z0-9@_.-]">>)))(I,D) end, fun(Node, Idx) -> 
    iolist_to_binary(Node)
 end).

'argv'(Input, Index) ->
  p(Input, Index, 'argv', fun(I,D) -> (p_seq([p_string(<<"\"">>), p_zero_or_more(p_charclass(<<"[a-zA-Z0-9.:=_\\s\t\\/-]">>)), p_string(<<"\"">>)]))(I,D) end, fun(Node, Idx) -> 
    list_to_binary(binary_to_list(iolist_to_binary(Node))--[34,34])
 end).

'delimeter'(Input, Index) ->
  p(Input, Index, 'delimeter', fun(I,D) -> (p_zero_or_more(p_seq([p_string(<<";">>), p_zero_or_more(fun 'space'/2)])))(I,D) end, fun(Node, Idx) -> Node end).

'space'(Input, Index) ->
  p(Input, Index, 'space', fun(I,D) -> (p_charclass(<<"[\\s\t]">>))(I,D) end, fun(Node, Idx) -> Node end).

'comment'(Input, Index) ->
  p(Input, Index, 'comment', fun(I,D) -> (p_seq([p_string(<<"#">>), p_zero_or_more(p_seq([p_not(p_string(<<"\n">>)), p_anything()]))]))(I,D) end, fun(Node, Idx) -> 
    {comment}
 end).

'crlf'(Input, Index) ->
  p(Input, Index, 'crlf', fun(I,D) -> (p_seq([p_optional(p_charclass(<<"[\r]">>)), p_one_or_more(p_charclass(<<"[\n]">>))]))(I,D) end, fun(Node, Idx) -> 
    {crlf}
 end).




p(Inp, Index, Name, ParseFun) ->
  p(Inp, Index, Name, ParseFun, fun(N, _Idx) -> N end).

p(Inp, StartIndex, Name, ParseFun, TransformFun) ->
  case get_memo(StartIndex, Name) of      % See if the current reduction is memoized
    {ok, Memo} -> %Memo;                     % If it is, return the stored result
      Memo;
    _ ->                                        % If not, attempt to parse
      case ParseFun(Inp, StartIndex) of
        {fail,_} = Failure ->                       % If it fails, memoize the failure
          Result = Failure;
        {Match, InpRem, NewIndex} ->               % If it passes, transform and memoize the result.
          Transformed = TransformFun(Match, StartIndex),
          Result = {Transformed, InpRem, NewIndex}
      end,
      memoize(StartIndex, Name, Result),
      Result
  end.

setup_memo() ->
  put(parse_memo_table, ets:new(?MODULE, [set])).

release_memo() ->
  ets:delete(memo_table_name()).

memoize(Index, Name, Result) ->
  Memo = case ets:lookup(memo_table_name(), Index) of
              [] -> [];
              [{Index, Plist}] -> Plist
         end,
  ets:insert(memo_table_name(), {Index, [{Name, Result}|Memo]}).

get_memo(Index, Name) ->
  case ets:lookup(memo_table_name(), Index) of
    [] -> {error, not_found};
    [{Index, Plist}] ->
      case proplists:lookup(Name, Plist) of
        {Name, Result}  -> {ok, Result};
        _  -> {error, not_found}
      end
    end.

memo_table_name() ->
    get(parse_memo_table).

p_eof() ->
  fun(<<>>, Index) -> {eof, [], Index};
     (_, Index) -> {fail, {expected, eof, Index}} end.

p_optional(P) ->
  fun(Input, Index) ->
      case P(Input, Index) of
        {fail,_} -> {[], Input, Index};
        {_, _, _} = Success -> Success
      end
  end.

p_not(P) ->
  fun(Input, Index)->
      case P(Input,Index) of
        {fail,_} ->
          {[], Input, Index};
        {Result, _, _} -> {fail, {expected, {no_match, Result},Index}}
      end
  end.

p_assert(P) ->
  fun(Input,Index) ->
      case P(Input,Index) of
        {fail,_} = Failure-> Failure;
        _ -> {[], Input, Index}
      end
  end.

p_and(P) ->
  p_seq(P).

p_seq(P) ->
  fun(Input, Index) ->
      p_all(P, Input, Index, [])
  end.

p_all([], Inp, Index, Accum ) -> {lists:reverse( Accum ), Inp, Index};
p_all([P|Parsers], Inp, Index, Accum) ->
  case P(Inp, Index) of
    {fail, _} = Failure -> Failure;
    {Result, InpRem, NewIndex} -> p_all(Parsers, InpRem, NewIndex, [Result|Accum])
  end.

p_choose(Parsers) ->
  fun(Input, Index) ->
      p_attempt(Parsers, Input, Index, none)
  end.

p_attempt([], _Input, _Index, Failure) -> Failure;
p_attempt([P|Parsers], Input, Index, FirstFailure)->
  case P(Input, Index) of
    {fail, _} = Failure ->
      case FirstFailure of
        none -> p_attempt(Parsers, Input, Index, Failure);
        _ -> p_attempt(Parsers, Input, Index, FirstFailure)
      end;
    Result -> Result
  end.

p_zero_or_more(P) ->
  fun(Input, Index) ->
      p_scan(P, Input, Index, [])
  end.

p_one_or_more(P) ->
  fun(Input, Index)->
      Result = p_scan(P, Input, Index, []),
      case Result of
        {[_|_], _, _} ->
          Result;
        _ ->
          {fail, {expected, Failure, _}} = P(Input,Index),
          {fail, {expected, {at_least_one, Failure}, Index}}
      end
  end.

p_label(Tag, P) ->
  fun(Input, Index) ->
      case P(Input, Index) of
        {fail,_} = Failure ->
           Failure;
        {Result, InpRem, NewIndex} ->
          {{Tag, Result}, InpRem, NewIndex}
      end
  end.

p_scan(_, [], Index, Accum) -> {lists:reverse( Accum ), [], Index};
p_scan(P, Inp, Index, Accum) ->
  case P(Inp, Index) of
    {fail,_} -> {lists:reverse(Accum), Inp, Index};
    {Result, InpRem, NewIndex} -> p_scan(P, InpRem, NewIndex, [Result | Accum])
  end.

p_string(S) when is_list(S) -> p_string(list_to_binary(S));
p_string(S) ->
    Length = erlang:byte_size(S),
    fun(Input, Index) ->
      try
          <<S:Length/binary, Rest/binary>> = Input,
          {S, Rest, p_advance_index(S, Index)}
      catch
          error:{badmatch,_} -> {fail, {expected, {string, S}, Index}}
      end
    end.

p_anything() ->
  fun(<<>>, Index) -> {fail, {expected, any_character, Index}};
     (Input, Index) when is_binary(Input) ->
          <<C/utf8, Rest/binary>> = Input,
          {<<C/utf8>>, Rest, p_advance_index(<<C/utf8>>, Index)}
  end.

p_charclass(Class) ->
    {ok, RE} = re:compile(Class, [unicode, dotall]),
    fun(Inp, Index) ->
            case re:run(Inp, RE, [anchored]) of
                {match, [{0, Length}|_]} ->
                    {Head, Tail} = erlang:split_binary(Inp, Length),
                    {Head, Tail, p_advance_index(Head, Index)};
                _ -> {fail, {expected, {character_class, binary_to_list(Class)}, Index}}
            end
    end.

line({{line,L},_}) -> L;
line(_) -> undefined.

column({_,{column,C}}) -> C;
column(_) -> undefined.

p_advance_index(MatchedInput, Index) when is_list(MatchedInput) orelse is_binary(MatchedInput)-> % strings
  lists:foldl(fun p_advance_index/2, Index, unicode:characters_to_list(MatchedInput));
p_advance_index(MatchedInput, Index) when is_integer(MatchedInput) -> % single characters
  {{line, Line}, {column, Col}} = Index,
  case MatchedInput of
    $\n -> {{line, Line+1}, {column, 1}};
    _ -> {{line, Line}, {column, Col+1}}
  end.
