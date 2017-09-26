-module(raw).

-export([output/2]).

-type options() :: #{}.

-spec output(coverl:cover_data(), options()) -> coverl:cover_data().
output(CoverData, _Opt) ->
    CoverData.
