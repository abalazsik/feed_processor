-module(feed_processor_test).

-include_lib("eunit/include/eunit.hrl").

atom_test_() ->
    {ok, Bin} = file:read_file("./test/testAtomFeed.xml"),
    [Result] = feed_processor:extract(Bin),
    [{Title, Url, Category, PubDate}, _] = Result,
    [
        ?_assertEqual(2, length(Result)),
        ?_assertEqual("Test Article", Title),
        ?_assertEqual("https://www.test.xyz/test", Url),
        ?_assertEqual({{2021, 11, 7}, {17, 0, 0}}, PubDate)
    ].


rss_test_() ->
    {ok, Bin} = file:read_file("./test/testRssFeed.xml"),
    [Result] = feed_processor:extract(Bin),
    [{Title, Url, Category, PubDate}, _] = Result,
    [
        ?_assertEqual(2, length(Result)),
        ?_assertEqual("Test Article", Title),
        ?_assertEqual("https://www.test.xyz/test", Url),
        ?_assertEqual("Test Category", Category),
        ?_assertEqual({{2021, 11, 7}, {17, 0, 0}}, PubDate)
    ].

parsePubDate_test_() ->
    [
        ?_assertEqual({{2021, 11, 03}, {14, 00, 00}}, feed_processor:parsePubDate(pubDate, <<"Wed, 03 Nov 2021 14:00:00 GMT">>)),
        ?_assertEqual({{2021, 9, 5}, {3, 12, 26}}, feed_processor:parsePubDate(pubDate, <<"Fri, 05 Sep 2021 03:12:26 GMT">>)),
        ?_assertEqual({{2021, 11, 6}, {8, 51, 15}}, feed_processor:parsePubDate(pubDate, <<"Sat, 06 Nov 2021 08:51:15 +0100">>)),
        
        ?_assertEqual({{2021, 10, 24}, {18, 48, 12}}, feed_processor:parsePubDate(iso, <<"2021-10-24T18:48:12+00:00">>)),
        ?_assertEqual({{2021, 9, 09}, {0, 32, 20}}, feed_processor:parsePubDate(iso, <<"2021-09-09T00:32:20+00:00">>)),
        ?_assertEqual({{2003, 12, 13}, {18, 30, 2}}, feed_processor:parsePubDate(iso, <<"2003-12-13T18:30:02Z">>))
    ].