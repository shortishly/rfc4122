%% Copyright (c) 2013-2014, Peter Morgan <peter.james.morgan@gmail.com>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(rfc4122_uuid).

-export([v4/0,
	 v5/2,
	 iolist/1,
	 external/1]).

-spec v4() -> rfc4122:uuid().
v4() ->
    <<TimeLow:32, TimeMid:16, _:4, TimeHi:12, _:2, ClockHi:6, ClockLow:8, Node:48>> = crypto:strong_rand_bytes(16),
    uuid(TimeLow, TimeMid, 4, TimeHi, ClockHi, ClockLow, Node).


-spec v5(rfc4122:uuid(), iodata()) -> rfc4122:uuid().
v5(Space, Name) ->
    <<TimeLow:32, TimeMid:16, _:4, TimeHi:12, _:2, ClockHi:6, ClockLow:8, Node:48, _/binary>> = crypto:sha([iolist(Space), Name]),
    uuid(TimeLow, TimeMid, 5, TimeHi, ClockHi, ClockLow, Node).

-spec external(rfc4122:uuid()) -> binary().
external(UUID) ->
    iolist_to_binary(iolist(UUID)).

-spec iolist(rfc4122:uuid()) -> iolist().
iolist(<<TimeLow:32, TimeMid:16, TimeHi:16, Clock:16, Node:48>>) ->
    io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~4.16.0b-~12.16.0b", [TimeLow, TimeMid, TimeHi, Clock, Node]).

-spec uuid(integer(), integer(), 1..5, integer(), integer(), integer(), integer()) -> rfc4122:uuid().
uuid(TimeLow, TimeMid, Version, TimeHi, ClockHi, ClockLow, Node) ->
    <<TimeLow:32, TimeMid:16, Version:4, TimeHi:12, 2:2, ClockHi:6, ClockLow:8, Node:48>>.
    

    
