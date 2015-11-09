-module(ktn_meta_suite_SUITE).

-include_lib("mixer/include/mixer.hrl").
-mixin([{ ktn_meta_SUITE
        , [ all/0
          , xref/1
          , dialyzer/1
          , elvis/1
          ]
        }]).

-export([init_per_testcase/2]).

-type config() :: [{atom(), term()}].

-spec init_per_testcase(atom(), config()) -> config().
init_per_testcase(_TestCase, Config) ->
  [{application, katana} | Config].
