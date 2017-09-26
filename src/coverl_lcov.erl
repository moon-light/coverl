-module(coverl_lcov).

-export([output/2]).

-type options() :: #{out => file:name()}.

-spec output(coverl:report_data(), options()) -> coverl:report_data().
output(ReportData, Opt) ->
    OutFile = maps:get(out, Opt, "lcov.info"),
    FileInfo = summ_data(maps:to_list(ReportData), #{}),
    ok = file:write_file(OutFile, [format_data(maps:to_list(FileInfo))]).

summ_data([{_, FileDataMap} | DataTail], SummData) ->
    summ_data(DataTail, maps:fold(fun summ_file_info/3, SummData, FileDataMap));
summ_data([], SummData) ->
    SummData.

summ_file_info(File, AddFileInfo, Data) ->
    FileInfo = maps:get(File, Data, #{}),
    LineInfo = maps:get(line, FileInfo, #{}),
    AddLineInfo = maps:get(line, AddFileInfo, #{}),
    NextLineInfo = maps:fold(fun summ_count/3, LineInfo, AddLineInfo),
    BranchInfo = maps:get(branch, FileInfo, #{}),
    AddBranchInfo = maps:get(branch, AddFileInfo, #{}),
    NextBranchInfo =
        maps:fold(fun summ_count/3, BranchInfo, AddBranchInfo),
    Data#{File => #{line => NextLineInfo, branch => NextBranchInfo}}.

summ_count(ID, Add, Info) ->
    maps:update_with(ID, fun(Count) -> Count + Add end, Add, Info).

format_data(
    [{FileName, #{line := LineInfo, branch := BranchInfo}} | ReportDataTail])
->
    [ "SF:", filename:absname(FileName), "\n"
    , format_branch_report(lists:sort(maps:to_list(BranchInfo)))
    , "BRF:", integer_to_list(maps:size(BranchInfo)), "\n"
    , "BRH:", integer_to_list(maps:fold(fun not_miss/3, 0, BranchInfo)), "\n"
    , format_line_report(lists:sort(maps:to_list(LineInfo)))
    , "LH:", integer_to_list(maps:size(LineInfo)), "\n"
    , "LF:", integer_to_list(maps:fold(fun not_miss/3, 0, LineInfo)), "\n"
    , "end_of_record\n"
    | format_data(ReportDataTail)];
format_data([]) ->
    [].

not_miss(_, 0, Count) -> Count;
not_miss(_, _, Count) -> Count + 1.

format_branch_report([{{Line, ID, _, _, _} = Key, Count} | CDInfoTail]) ->
    [ format_branch(Line, ID, erlang:phash2(Key), Count)
    | format_branch_report(CDInfoTail)];
format_branch_report([]) ->
    [].

format_branch(Line, ID, BID, 0) ->
    [ "BRDA:", integer_to_list(Line), ",", integer_to_list(ID), ","
    , integer_to_list(BID), ",-\n"];
format_branch(Line, ID, BID, Count) ->
    [ "BRDA:", integer_to_list(Line), ",", integer_to_list(ID), ","
    , integer_to_list(BID), ",", integer_to_list(Count), "\n"].

format_line_report([{Line, Count} | LineInfoTail]) ->
    [ "DA:", integer_to_list(Line), ",", integer_to_list(Count), "\n"
    | format_line_report(LineInfoTail)];
format_line_report([]) ->
    [].
