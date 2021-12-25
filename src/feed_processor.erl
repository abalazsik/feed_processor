-module(feed_processor).
-export([extract/1]).

-ifdef(TEST).
-export([parsePubDate/2]).
-endif.

-include_lib("xmerl/include/xmerl.hrl").

-spec extract(string() | binary()) -> [[
    {
        Title :: string(),
        Url :: string(),
        Categories :: [string()],
        PubDate :: calendar:datetime()
    }]].

extract(Binary) when is_binary(Binary) ->
    extract(erlang:binary_to_list(Binary));
extract(String) when is_list(String) ->
    Contents = getDoc(xmerl_scan:string(String)),
    lists:map(
        fun(Channel) ->
            Items = Channel#xmlElement.content,
            lists:map(
                fun
                    (Item) when Item#xmlElement.name == item -> %% process rss feed
                        Elems = Item#xmlElement.content,
                        Title = asText(getNodeContent(Elems, title)),
                        Url = asText(getNodeContent(Elems, link)),
                        Categories = getCategories(Elems),
                        PubDate = parsePubDate(pubDate, asText(getNodeContent(Elems, pubDate))),
                        {Title, Url, Categories, PubDate};
                    (Entry) when Entry#xmlElement.name == entry -> %%process atom feed
                        Elems = Entry#xmlElement.content,
                        Title = asText(getNodeContent(Elems, title)),
                        UrlElem = getNode(Elems, link),
                        Url = asText(getAttrib(UrlElem#xmlElement.attributes, href)),
                        PubDate = parsePubDate(iso, asText(getNodeContent(Elems, updated))),
                        {Title, Url, [], PubDate}
                end,
                getItems(Items)
            )
        end,
        getChannels(Contents)
    ).

getDoc({Doc, _}) when is_record(Doc, xmlElement) ->
    if
        Doc#xmlElement.name == feed -> [Doc]; %%single feed
        true -> Doc#xmlElement.content
    end.

getAttrib([], Name) ->
    throw({attribute_not_exists, Name});
getAttrib([Elem | Elements], Name) ->
    case (Elem) of
        Elem when is_record(Elem, xmlAttribute) andalso Elem#xmlAttribute.name == Name ->
            Elem#xmlAttribute.value;
        _ ->
            getAttrib(Elements, Name)
    end.

getNode([], Name) ->
    throw({elem_not_exists, Name});
getNode([Elem | Elements], Name) ->
    case (Elem) of
        Elem when is_record(Elem, xmlElement) andalso Elem#xmlElement.name == Name ->
            Elem;
        _ ->
            getNode(Elements, Name)
    end.

getNodeContent(Elements, Name) ->
    Node = getNode(Elements, Name),
    Node#xmlElement.content.

getItems(Channel) -> 
    lists:filter(
        fun
            (Content) when is_record(Content, xmlElement) ->
                Name = Content#xmlElement.name,
                Name == item orelse Name == entry;
            (_) ->
                false
        end,
        Channel).

getChannels(Elements) ->
    lists:filter(
        fun
            (Content) when is_record(Content, xmlElement) ->
                Name = Content#xmlElement.name,
                Name == channel orelse Name == feed;
            (_) ->
                false
        end,
        Elements).

asText([]) ->
    [];
asText(Text) when length(Text) > 1 ->
    unicode:characters_to_list(Text);
asText([TextNode]) when is_record(TextNode, xmlText) ->
    unicode:characters_to_list(TextNode#xmlText.value).

getCategories(Elems) ->
    [asText(getNodeContent([Elem], category)) || Elem <- Elems, Elem#xmlElement.name == category].

monthNumber(Month) ->
    maps:get(Month, #{
        "Jan" => 1,
        "Feb" => 2,
        "Mar" => 3,
        "Apr" => 4,
        "May" => 5,
        "Jun" => 6,
        "Jul" => 7,
        "Aug" => 8,
        "Sep" => 9,
        "Oct" => 10,
        "Nov" => 11,
        "Dec" => 12
    }). 

timestampRegex(pubDate) ->
    "(Mon|Tue|Wed|Thu|Fri|Sat|Sun),\\s(?<day>([012]\\d)|(3[01]))\\s(?<month>Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)\\s(?<year>\\d{4})\\s(?<hour>[0-2]\\d):(?<min>[0-5]\\d):(?<sec>[0-5]\\d)\\s(([A-Z]{3})|((\\+|\\-)\\d{4}))";
timestampRegex(iso) ->
    "(?<year>\\d{4})-(?<month>[0-1]\\d)-(?<day>([012]\\d)|(3[01]))T(?<hour>[0-2]\\d):(?<min>[0-6]\\d):(?<sec>[0-6]\\d)(([A-Z]+)|((\\+|\\-)\\d{2}:\\d{2}))".

parsePubDate(ExpectedFormat, Bin) ->
    Regex = timestampRegex(ExpectedFormat),
    
    MonthFn = case ExpectedFormat of
        pubDate -> fun monthNumber/1;
        iso -> fun erlang:list_to_integer/1
    end,

    case re:run(Bin, Regex,[{capture, [year, month, day, hour, min, sec], list}]) of
        {match, [Year, Month, Day, Hour, Min, Sec]} ->
            {
                {erlang:list_to_integer(Year), MonthFn(Month), erlang:list_to_integer(Day)},
                {erlang:list_to_integer(Hour), erlang:list_to_integer(Min), erlang:list_to_integer(Sec)}
            };
        nomatch -> {wrong_format, ExpectedFormat, Bin}
    end.
