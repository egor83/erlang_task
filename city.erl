-module(city).
-export([cityInit/5]).

-import(io, [format/2, format/1]).
-import(lists, [foreach/2, map/2, all/2]).



%% When the city is created, it should register at the address book by sending a message with its coordinates and PID.
%% A signal is received after that; it means that all cities finished registration and it's possible to start looking up
%% neighbours now.
%% Then recursive function city() is started.

cityInit(X, Y, CountryPid, Coins, AddressBook) ->
    AddressBook ! {regCity, X, Y, self()},
    CountryPid ! city_created,

    receive
        lookup_neighbours ->
%            format("Lookup is started~n"),                     % DEBUG
            AddressBook ! {lookup, self(), X, Y+1},
            AddressBook ! {lookup, self(), X+1, Y},
            AddressBook ! {lookup, self(), X, Y-1},
            AddressBook ! {lookup, self(), X-1, Y}
    end,
    
    NeighList = makeNeighboursList(4),
    
%    format("cityInit: X: ~p, Y: ~p; PID: ~p; coins: ~p~nNeighbours: ~p~n~n", [X, Y, self(), Coins, NeighList]),         % DEBUG
    city(CountryPid, Coins, NeighList).



%% City process. Handles all intercity messages; reports completeness to the country process.

city(CountryPid, Coins, Neighbours) ->

    IsCityComplete = all(fun(X) -> X =/= 0 end, Coins),
    CountryPid ! {cityComplete, IsCityComplete},
%    format("City ~p, coins: ~p, complete: ~p~n", [self(), NewCoins, IsCityComplete]),      % DEBUG

    receive
        stop ->
            city_process_terminated;
        
        newTurn ->
            CoinsWereSent = sendCoins(Coins, Neighbours),
            NewCoins = getCoins(CoinsWereSent, length(Neighbours)),
            city(CountryPid, NewCoins, Neighbours)
    end.



%% Four queries were sent to the address book.
%% This function receives answers for these queries and forms a list of neighbouring cities

makeNeighboursList(0) -> [];
makeNeighboursList(ResponseCount) ->
    receive
        {address, not_found} ->
%            format("City not found, ~p left~n", [ResponseCount-1]),     % DEBUG
            makeNeighboursList(ResponseCount-1);
            
        {address, Pid} ->
%            format("City found, ~p left~n", [ResponseCount-1]),         % DEBUG
            [Pid | makeNeighboursList(ResponseCount-1)]
    end.



%% Prepare a parcel which will be sent out to neighbour cities - 1 coins out of every 1000.
%% Send parcels to all neighbours and make a new list by subtracting the coins that were sent.

sendCoins(Coins, Neighbours) ->
    Parcel = map(fun(X) -> X div 1000 end, Coins),
    foreach(fun(Neigh) -> Neigh ! {parcel, Parcel} end, Neighbours),
    SentCoins = map(fun(X) -> -X * length(Neighbours) end, Parcel),
    makeSum(Coins, SentCoins).
    


%% Receive coins from all neighbours.
%% Second argument is a counter showing how many cities haven't sent their coins yet.

getCoins(Coins, 0) ->
    Coins;

getCoins(Coins, LeftToGet) ->
    receive
        {parcel, Parcel} ->
            getCoins(makeSum(Coins, Parcel), LeftToGet - 1)
    end.



%% Add up two lists, ie
%% for two given lists L, M make a list N where Ni = Li + Mi
%% Sums first min(length(L), length(M)) elements (i.e. works even for lists of different lengths)

makeSum([], _) ->
    [];
makeSum(_, []) ->
    [];
makeSum([H1 | T1], [H2 | T2]) ->
    [H1 + H2 | makeSum(T1, T2)].
