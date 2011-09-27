-module(country).
-export([countryInit/6]).

-import(city, [cityInit/5]).

-import(io, [format/2, format/1]).
-import(lists, [seq/2]).



%% Initiates country process; a country is registered in address book;
%% all values are set and a function spawning processes for all cities belonging to this country is run.
%% When all cities for this country are created, a recursive function country() is started.

countryInit(CountryName, CountryNum, CountriesQty, {Xl, Yl, Xh, Yh} = Limits, AbPid, ControlCenterPid) ->
    AbPid ! {regCountry, self()},
    Coins = makeCoins(CountryNum, CountriesQty),
    createCities(Limits, self(), Coins, AbPid),
    CitiesQty = (Xh-Xl+1) * (Yh-Yl+1),
    waitForCities(CitiesQty),
    ControlCenterPid ! cities_ready,
    
    country(CountryName, CitiesQty, ControlCenterPid),
    ok.



%% Get completeness reports from all cities; report completeness to control center.

country(CountryName, CitiesQty, ControlCenterPid) ->
    case checkCompleteness(true, CitiesQty) of
        {countryComplete, IsCountryComplete} ->
            ControlCenterPid ! {countryComplete, CountryName, IsCountryComplete},
%            format("country #~s is complete: ~p~n", [CountryName, IsCountryComplete]),     % DEBUG
            country(CountryName, CitiesQty, ControlCenterPid);
        stop ->
            country_process_terminated
    end.



%% Receive city messages and check country completeness.
%% First argument is a completeness so far - country is considered complete until the first incomplete city is found.
%% Second argument is the amount of cities that haven't reported yet.

checkCompleteness(Result, 0) ->
    {countryComplete, Result};
checkCompleteness(false, CitiesQty) ->
    %% It doesn't matter what cities report once it is clear that country is incomplete
    %% (ie when the first argument, "status so far" is false).
    receive
        {cityComplete, _} ->
            checkCompleteness(false, CitiesQty - 1);
        stop ->
            stop
    end;
checkCompleteness(true, CitiesQty) ->
    receive
        {cityComplete, Result} ->
            checkCompleteness(Result, CitiesQty - 1);
        stop ->
            stop
    end.



%% Given country coordinates (Xl, Yl and Xh, Yh), create (Xh-Xl+1)*(Yh-Yl+1) city processes.

createCities({Xl, Yl, Xh, Yh}, CountryPid, Coins, AddressBook) ->
    for(Xl, Xh, fun(X) -> citiesX(X, Yl, Yh, CountryPid, Coins, AddressBook) end),

%    receive                      % DEBUG
%    after 1 ->                   % DEBUG
%        AddressBook ! listall    % DEBUG
%    end,                         % DEBUG
    
    create_ok.



%% Start city processes for all cities with a given absciss X and ordinates ranging from Yl to Yh inclusively.

citiesX(X, Yl, Yh, CountryPid, Coins, AddressBook) ->
    for(Yl, Yh, fun(Y) -> 
                        spawn(fun() -> cityInit(X, Y, CountryPid, Coins, AddressBook) end)
                    end).



%% Returns a list in which elements are the results of Fun() applied to every integer from Start to Stop inclusively.

for(Start, Stop, Fun) ->
    List = seq(Start, Stop),
    [Fun(A) || A <- List].



%% Create coins list for all cities in a given country.
%% First argument is a country number (cities in a country #1 will have 1M of coins #1 etc).
%% Second argument is a total number of countries.

makeCoins(1, CountriesQty) ->
    [1000000 | makeCoins(0, CountriesQty-1)];
makeCoins(_, 0) ->
    [];
makeCoins(CountryNum, CountriesQty) ->
    [0 | makeCoins(CountryNum-1, CountriesQty-1)].



%% Wait for all cities in this country to be created.

waitForCities(0) ->
    ok;
waitForCities(CitiesQty) ->
    receive
        city_created ->
%            format("City created, ~p remaining~n", [CitiesQty-1]),     % DEBUG
            waitForCities(CitiesQty - 1)
    end.
