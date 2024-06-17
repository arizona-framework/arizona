%{ok, {{Version, Status, ReasonPhrase}, Headers, Body}} =
%    httpc:request("http://localhost:8080")
-define(assertHttpBody(Pattern, Resp), (
    ?assertNotEqual(nomatch, string:find(element(3, Resp), Pattern))
)).
-define(assertHttpStatus(Status, Resp), (
    ?assertEqual(Status, element(2, element(1, Resp)))
)).

