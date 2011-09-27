-module(parseInput).
-export([parse/0]).

-import(io, [fread/3, format/2, format/1]).
-import(file, [open/2, close/1]).



%% Parse an input file and return a list of tuples;
%% each tuple is a test case description consisting of two list members:
%% [ {CountriesDataList, CountriesStateList} ]
%% - country data list for country generation,
%% - countries' state list for state tracking by control center.

%% CountriesDataList format: [ {CountryName, CountryNum, Limits = {XL, YL, XH, YH} } ]
%% CountriesStateList format: [ {CountryName, Completeness} ]

parse() ->
    {ok, FileIn} = open("euro.in", read),
    Cases = getCases(FileIn),
    close(FileIn),
    Cases.



%% Read test cases from the file, one at a time.
%% End of file is marked with 0.

getCases(FileIn) ->
    {ok, [Qty]} = fread(FileIn, "", "~d"),
    
    if
        Qty =:= 0 ->
            [];
        true ->  % Qty is not zero
%            format("Qty is ~B~n", [Qty]),                                 % DEBUG
            CountriesDataList = getCountryList(FileIn, 1, Qty),
            CountriesStateList = getCountryState(CountriesDataList),
%            format("Countries data:~n~p~n", [CountriesDataList]),         % DEBUG
%            format("Countries state:~n~p~n", [CountriesStateList]),       % DEBUG
            [{CountriesDataList, CountriesStateList} | getCases(FileIn)]
    end.



%% Read all countries' data from the file one at a time; form a list containing all data.
%% Num is a country number; Qty is the amount of countries that have to be read yet.

getCountryList(_, _, 0) ->
    [];
getCountryList(FilePtr, Num, Qty) ->
    {ok, [CountryName, XL, YL, XH, YH]} = fread(FilePtr, "", "~s~d~d~d~d"),
    CountriesDataList = {CountryName, Num, _Limits = {XL, YL, XH, YH}},
    [CountriesDataList | getCountryList(FilePtr, Num+1, Qty-1)].



%% Form a list of countries for control center.

getCountryState([]) ->
    [];
getCountryState([ {CountryName, _Num, _Limits} | T]) ->
    [{CountryName, _Completeness = false} | getCountryState(T)].
