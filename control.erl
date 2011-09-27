-module(control).
-export([controlInit/3]).

-import(io, [format/2, format/1]).
-import(lists, [foreach/2, sort/1, all/2]).



%% Wait for all cities in this test case to be created; broadcast signal announcing this and allowing to start looking up neighbours.

controlInit(AbPid, MainPid, CountriesStateList) ->
    receive
        cities_ready ->
%            format("Starting lookup~n"),                     % DEBUG
            AbPid ! {broadcastCities, lookup_neighbours}
    end,
    control(AbPid, MainPid, CountriesStateList, 0).



%% Every turn countries report to control center by sending message of the following format:
%% {countryComplete, CountryNum, IsCountryComplete}, IsCountryComplete may be true or false.
%% CountriesStateList format is: [ {CountryName, Completeness} ]

%% Control center prints out a message when country becomes complete (country's completeness goes from false to true).
%% When all countries are complete, control center broadcasts terminating messages and stops.

control(AbPid, MainPid, CountriesStateList, TurnNum) ->
    
%% get message
%% look up country
%% compare states
%% if the state has changed - print the message
%% make a new list

    NewCountriesStateList = handleMessages(CountriesStateList, [], [], TurnNum),
    case all(fun({_, State}) -> State end, CountriesStateList) of
        false -> % Not every country is complete
%            format("Turn ~p~n", [TurnNum]),                         % DEBUG
            AbPid ! {broadcastCities, newTurn},
            control(AbPid, MainPid, NewCountriesStateList, TurnNum + 1);
        true -> % All countries are complete, stop the process
            AbPid ! {broadcastCities, stop},
            AbPid ! {broadcastCountries, stop},
            AbPid ! stop,
            MainPid ! caseFinished
    end.

% DEBUG SECTION
%    receive                 % DEBUG
%        stop ->             % DEBUG
%            control_stopped % DEBUG
%    end.                    % DEBUG



%% Process messages from all countries and build a new countries state list.
%% Build a list of countries that became complete this turn; when all countries have reported, this list will be sorted and printed out.

handleMessages(OldStateList, NewStateList, BecameComplete, TurnNum) when length(OldStateList) =:= length(NewStateList) ->
    
    %% NewStateList is as long as an OldStateList - ie all countries have reported already,
    %% now we can sort the list of countries that have become complete this turn and print it out.
    
    SortedList = sort(BecameComplete),
    foreach(fun(Name) -> format("    ~s    ~p~n", [Name, TurnNum]) end, SortedList),
    NewStateList;

handleMessages(OldStateList, NewStateList, BecameComplete, TurnNum) ->
    receive
        {countryComplete, CountryName, IsCountryComplete} ->

            case Country = lookupCountry(CountryName, OldStateList) of
            %% get the previous state of a given country...
                {CountryName, false} ->

                    case IsCountryComplete of
                    %%...and compare it with a new state
                        false ->
                            handleMessages(OldStateList, [Country | NewStateList], BecameComplete, TurnNum);

                        true ->
                            %% If the country became complete, add it to the respective list.
                            %% Also add country to NewStateList with a new state.
                            handleMessages(OldStateList, [{CountryName, true} | NewStateList], [CountryName | BecameComplete], TurnNum)
                    end;
                    
                {CountryName, true} ->
                    %% Country is already complete, so there's no need to change anything, just add the country to the NewStateList.
                    handleMessages(OldStateList, [Country | NewStateList], BecameComplete, TurnNum)
            end
    end.



lookupCountry(LookUpName, [Country = {CountryName, _} | _]) when LookUpName =:= CountryName ->
    Country;
lookupCountry(LookUpName, [_ | StateList]) ->
    lookupCountry(LookUpName, StateList).
