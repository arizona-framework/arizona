-include_lib("stdlib/include/assert.hrl").

-define(ARIZONA_ASSERT_BODY(Pattern, Resp), (
    ?assertNotEqual(nomatch, string:find(element(3, Resp), Pattern))
)).

-define(ARIZONA_ASSERT_STATUS(Status, Resp), (
    ?assertEqual(Status, element(2, element(1, Resp)))
)).
