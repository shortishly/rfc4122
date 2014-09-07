%% Copyright (c) 2014 Peter Morgan <peter.james.morgan@gmail.com>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(rfc4122).

-export([
	 v4/0,
	 v5/2,
	 iolist/1,
	 external/1,
	 version/1
	]).

-export_type([
	      uuid/0
	     ]).

-opaque uuid() :: <<_:128>>.


-spec v4() -> uuid().
v4() ->
    <<TimeLow:32, TimeMid:16, _:4, TimeHi:12, _:2, ClockHi:6, ClockLow:8, Node:48>> = crypto:strong_rand_bytes(16),
    uuid(TimeLow, TimeMid, 4, TimeHi, ClockHi, ClockLow, Node).


-spec v5(uuid(), iodata()) -> uuid().
v5(Space, Name) ->
    <<TimeLow:32, TimeMid:16, _:4, TimeHi:12, _:2, ClockHi:6, ClockLow:8, Node:48, _/binary>> = crypto:hash(sha512, [iolist(Space), Name]),
    uuid(TimeLow, TimeMid, 5, TimeHi, ClockHi, ClockLow, Node).


-spec uuid(integer(), integer(), 1..5, integer(), integer(), integer(), integer()) -> uuid().
uuid(TimeLow, TimeMid, Version, TimeHi, ClockHi, ClockLow, Node) ->
    <<TimeLow:32, TimeMid:16, Version:4, TimeHi:12, 2:2, ClockHi:6, ClockLow:8, Node:48>>.


-spec external(uuid()) -> binary().
external(UUID) ->
    iolist_to_binary(iolist(UUID)).

-spec iolist(uuid()) -> iolist().
iolist(<<TimeLow:32, TimeMid:16, TimeHi:16, Clock:16, Node:48>>) ->
    io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~4.16.0b-~12.16.0b", [TimeLow, TimeMid, TimeHi, Clock, Node]).

-spec version(uuid()) -> pos_integer().
version(<<_TimeLow:32, _TimeMid:16, Version:4, _TimeHi:12, 2:2, _ClockHi:6, _ClockLow:8, _Node:48>>) ->
    Version.

