-module(addressBook).
-export([addressBook/2]).

-import(lists, [foreach/2]).
-import(io, [format/2, format/1]).



%% Cities are stored in a list in a following format: {X, Y, Pid}

%% Address book helps to handle all intercity communications by mapping coordinates to PIDs.
%% First all cities register at the address book by sending their coordinates and PIDs;
%% then cities form a list of neigbours' PIDs by sending its coordinates to address book.
%% Address book also keeps PIDs for all cities to perform countries broadcast.
%% Broadcasts are made in a form {broadcastCities, Message} and {broadcastCountries, Messages}
%% for cities and countries respectively.

addressBook(Cities, Countries) ->
    receive
        {regCity, X, Y, Pid} ->
            addressBook([{X, Y, Pid} | Cities], Countries);
            
        {lookup, From, X, Y} ->
%            format("Looking up ~p, ~p~n", [X, Y]),                  % DEBUG
            From ! {address, lookup(X, Y, Cities)},
            addressBook(Cities, Countries);

        {regCountry, Pid} ->
            addressBook(Cities, [Pid | Countries]);
            
        listall ->                                                   % DEBUG
%            format("~nNow showing the full list...~n"),             % DEBUG
            foreach(fun({X, Y, Pid}) -> format("X: ~p, Y: ~p, PID: ~p;~n", [X, Y, Pid]) end, Cities),
            addressBook(Cities, Countries);
            
        {broadcastCities, Message} ->
%            format("Broadcasting: [~p]~n", [Message]),              % DEBUG
            foreach(fun({_, _, Pid}) -> Pid ! Message end, Cities),
            addressBook(Cities, Countries);
            
        {broadcastCountries, Message} ->
            foreach(fun(Pid) -> Pid ! Message end, Countries),
            addressBook(Cities, Countries);
            
        stop ->
            addressBook_stopped
    end.



%% Look up city's PID given its coordinates.

lookup(_, _, []) ->
    not_found;
lookup(X, Y, [{NextX, NextY, Pid} | _]) when {X, Y} =:= {NextX, NextY} ->
    Pid;
lookup(X, Y, [_ | T]) ->
    lookup(X, Y, T).
