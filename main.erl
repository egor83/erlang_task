-module(main).
-export([main/0]).

-import(parseInput, [parse/0]).
-import(country, [countryInit/6]).
-import(addressBook, [addressBook/2]).
-import(control, [controlInit/3]).

-import(lists, [foreach/2]).
-import(io, [format/2, format/1]).



%% Parse input, get a list of test cases and start a recursive function mainLoop which will run these cases in turn.

main() ->
    CasesList = parse(),
    mainLoop(CasesList, 1).

%    DEBUG SECTION - used for manual turn generation and processes termination
%    spawn(fun() -> main_loop_debug(AbPid, ControlCenterPid) end).



%% Start all services (address book, control center) and all countries for a given test case.
%% When this case is finished - recurse, running the next test case.

mainLoop([], _) ->
    ok;

mainLoop([{CountriesDataList, CountriesStateList} | CasesList], CaseNum ) ->

%    format("MainLoop:~n~p~n~p~n~n", [CountriesDataList, CountriesStateList]), % DEBUG
    format("Case Number ~p~n", [CaseNum]),
    AbPid = spawn(fun() -> addressBook([], []) end),
    CountriesQty = length(CountriesDataList),
    MainPid = self(),
    ControlCenterPid = spawn(fun() -> controlInit(AbPid, MainPid, CountriesStateList) end),
    runCountryInit(CountriesDataList, CountriesQty, AbPid, ControlCenterPid),
    
    receive
        caseFinished ->
%            format("Case is finished~n"), % DEBUG
            mainLoop(CasesList, CaseNum+1)
    end.



%% Spawn all country processes.
%% Ñan't use map() here - must have an increasing counter for each country.

runCountryInit([], _, _, _) ->
    [];
runCountryInit([{CountryName, CountryNum, Limits} | T], CountriesQty, AbPid, ControlCenterPid) ->
    [spawn(fun() ->
                countryInit(CountryName, CountryNum, CountriesQty, Limits, AbPid, ControlCenterPid)
            end)
            | runCountryInit(T, CountriesQty, AbPid, ControlCenterPid) ].
