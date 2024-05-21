%%
%% Log macros
%%
-ifndef(__LOG_HRL__).
-define(__LOG_HRL__, true).

-include_lib("kernel/include/logger.hrl").

-define(debug(_Format), ?debug(_Format, [])).
-define(debug(_Format, _Args), ?LOG_DEBUG(_Format, _Args)).

-define(warning(_Format), ?warning(_Format, [])).
-define(warning(_Format, _Args), ?LOG_WARNING(_Format, _Args)).

-define(info(_Format), ?info(_Format, [])).
-define(info(_Format, _Args), ?LOG_INFO(_Format, _Args)).

-define(error(_Format), ?error(_Format, [])).
-define(error(_Format, _Args), ?LOG_ERROR(_Format, _Args)).

-endif.
