-module(bwt).

-author("take.this.2.your.grave@gmail.com").

-export([bwtrans_encode/1,
         bwtrans_decode/1]).

-define(UCHAR_MAX, 16#100).
-define(STOP_CHAR, $$).

-spec bwtrans_encode(binary()) -> binary().
bwtrans_encode(String) ->
    list_to_binary([binary:last(X) || X <- circulant_matrix(<<String/binary, ?STOP_CHAR>>)]).

-spec bwtrans_decode(binary()) -> binary().
bwtrans_decode(BWString) ->
    {Pos, Count} = counting_sort(BWString),
    list_to_binary(lfmapping(BWString, Pos, Count)).

%% ============================================================
%% internal function
%% ============================================================

circulant_matrix(Bin) ->
    circulant_matrix0(Bin, [], byte_size(Bin)).

circulant_matrix0(_Bin, Acc, 0) ->
    lists:sort(Acc);
circulant_matrix0(<<H:1/binary, T/binary>> = Bin, [], Length) ->
    circulant_matrix0(<<T/binary, H/binary>>, [Bin], Length - 1);
circulant_matrix0(<<H:1/binary, T/binary>> = Bin, Acc, Length) ->
    circulant_matrix0(<<T/binary, H/binary>>, [Bin|Acc], Length - 1).


list_update([], _, _, Acc) ->
    lists:reverse(Acc);
list_update([H|T], I, O, Acc) when I == length(Acc) ->
    list_update(T, I, O, [H + O|Acc]);
list_update([H|T], I, O, Acc) ->
    list_update(T, I, O, [H|Acc]).


counting_sort(Bin) ->
    Blank = [0 || _ <- lists:seq(0, ?UCHAR_MAX)],
    case counting_sort0(Bin, Blank, -1, byte_size(Bin)) of
        {Pos, Count0} ->
            case counting_sort1(0, Count0, []) of
                Count ->
                    {Pos, Count}
            end
    end.

counting_sort0(<<>>, Count, Pos, _Length) ->
    {Pos, Count};
counting_sort0(Bin, Count, Pos, Length) ->
    Size = size(Bin) - 1,
    <<C, Rest:Size/binary>> = Bin,
    case C of
        ?STOP_CHAR ->
            Idx = Length - Size,
            counting_sort0(Rest, list_update(Count, C, 1, []), Idx, Length);
        _ ->
            counting_sort0(Rest, list_update(Count, C, 1, []), Pos, Length)
    end.

counting_sort1(_, [], Acc) ->
    lists:reverse(Acc);
counting_sort1(Last, [Head|Tail], Acc) ->
    counting_sort1(Head + Last, Tail, [Last + Head|Acc]).


lfmapping(Bin, Pos, Count) ->
    LFMapping = [0 || _ <- lists:seq(1, byte_size(Bin))],
    case lfmapping0(Count, binary_to_list(Bin), byte_size(Bin), LFMapping) of
        List ->
            lfmapping1(binary_to_list(Bin), List, Pos, [])
    end.

lfmapping0(_Count, _ListOfBin, Pos, Acc) when Pos =< 0 ->
    Acc;
lfmapping0(Count, ListOfBin, Pos, Acc) ->
    Ord = lists:nth(Pos, ListOfBin),
    Idx = lists:nth(Ord+1, Count),
    lfmapping0(list_update(Count, Ord, -1, []), ListOfBin, Pos-1, list_update(Acc, Idx-1, Pos, [])).

lfmapping1(ListOfBin, _, _, [_|Tail] = Acc) when length(ListOfBin) =:= length(Acc) ->
    lists:reverse(Tail);
lfmapping1(ListOfBin, LFMapping, Pos, Acc) ->
    NewPos = lists:nth(Pos, LFMapping),
    lfmapping1(ListOfBin, LFMapping, NewPos, [lists:nth(NewPos, ListOfBin)|Acc]).
