-module(bwt).

-author("take.this.2.your.grave@gmail.com").

-export([bwtrans_encode/1,
         bwtrans_decode/1]).

-define(UCHAR_MAX, 16#100).

-spec bwtrans_encode(binary()) -> binary().
bwtrans_encode(String) ->
    [binary:last(X) || X <- circulant_matrix(<<String/binary, $$>>)].

-spec bwtrans_decode(binary()) -> binary().
bwtrans_decode(BWString) ->
    {Pos, Count} = counting_sort(BWString),
    lfmapping(BWString, Pos, Count).

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

counting_sort(Bin) ->
    Blank = [0 || _ <- lists:seq(0, ?UCHAR_MAX)],
    case counting_sort0(Bin, Blank, -1, byte_size(Bin)) of
        {Pos, Count0} ->
            case counting_sort1(Count0, 0) of
                Count ->
                    {Pos, Count}
            end
    end.

counting_sort0(<<>>, Count, Pos, _Length) ->
    {Pos, Count};
counting_sort0(Bin, Count, Pos, Length) ->
    Size = byte_size(Bin) - 1,
    <<C, Rest:Size/binary>> = Bin,
    {Prefix, [Nth|Suffix]} = lists:split(C, Count),
    case C of
        $$ ->
            Idx = Length - Size,
            counting_sort0(Rest, Prefix ++ [Nth + 1] ++ Suffix, Idx, Length);
        _ ->
            counting_sort0(Rest, Prefix ++ [Nth + 1] ++ Suffix, Pos, Length)
    end.

counting_sort1(Count, Pos) when Pos =:= length(Count) ->
    Count;
counting_sort1(Count, Pos) ->
    case lists:split(Pos, Count) of
        {[], [Nth|Suffix]} ->
            counting_sort1([Nth] ++ Suffix, Pos + 1);
        {Prefix, [Nth|Suffix]} ->
            counting_sort1(Prefix ++ [lists:last(Prefix) + Nth] ++ Suffix, Pos + 1)
    end.

lfmapping(Bin, Pos, Count) ->
    case lfmapping0(Count,
                    binary_to_list(Bin),
                    byte_size(Bin),
                    [0 || _ <- lists:seq(0, byte_size(Bin) - 1)]) of
        List ->
            lfmapping2(binary_to_list(Bin), List, Pos, [])
    end.

lfmapping0(_Count, _ListOfBin, Pos, Acc) when Pos =< 0 ->
    Acc;
lfmapping0(Count, ListOfBin, Pos, Acc) ->
    Ord = lists:nth(Pos, ListOfBin),
    case lists:split(Ord, Count) of
        {[], [Idx|Suffix]} ->
            case lists:split(Idx - 1, Acc) of
                {[], [_|Suf]} ->
                    lfmapping0([Idx - 1] ++ Suffix, ListOfBin, Pos - 1, [Pos] ++ Suf);
                {Pre, [_|Suf]} ->
                    lfmapping0([Idx - 1] ++ Suffix, ListOfBin, Pos - 1, Pre ++ [Pos] ++ Suf)
            end;
        {Prefix, [Idx|Suffix]} ->
            case lists:split(Idx - 1, Acc) of
                {[], [_|Suf]} ->
                    lfmapping0(Prefix ++ [Idx - 1] ++ Suffix, ListOfBin, Pos - 1, [Pos] ++ Suf);
                {Pre, [_|Suf]} ->
                    lfmapping0(Prefix ++ [Idx - 1] ++ Suffix, ListOfBin, Pos - 1, Pre ++ [Pos] ++ Suf)
            end
    end.

lfmapping2(ListOfBin, _LfMap, _Pos, Acc) when length(ListOfBin) =:= length(Acc) ->
    lists:reverse(Acc);
lfmapping2(ListOfBin, LfMap, Pos, Acc) ->
    NewPos = lists:nth(Pos, LfMap),
    lfmapping2(ListOfBin, LfMap, NewPos, [lists:nth(NewPos, ListOfBin)|Acc]).
