-module(feed_processor).
-export([extract/1]).

-ifdef(TEST).
-export([parsePubDate/2]).
-endif.

-include_lib("xmerl/include/xmerl.hrl").

-spec extract(string() | binary()) -> [[
    {
        Title :: binary(),
        Url :: binary(),
        Categories :: [binary()],
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
                        LinkElem = getLink(Elems),
                        Url = asText(getAttrib(LinkElem#xmlElement.attributes, href, [])),
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

getAttrib([], _, Default) ->
    Default;
getAttrib([Attribute | Attributes], Name, Default) ->
    case (Attribute) of
        Attribute when is_record(Attribute, xmlAttribute) andalso Attribute#xmlAttribute.name == Name ->
            Attribute#xmlAttribute.value;
        _ ->
            getAttrib(Attributes, Name, Default)
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


getLink(Elems) ->
    getLink(Elems, unknown).

%%return the link element where rel == "alternate" if it exists otherwise the first link element
getLink([], unknown) ->
    throw({no_link_element});
getLink([], Candidate) ->
    Candidate;
getLink([Elem | Elements], Candidate) ->
    case (Elem) of
        Elem when is_record(Elem, xmlElement) andalso Elem#xmlElement.name == link ->
            case getAttrib(Elem#xmlElement.attributes, rel, unknown) of
                "alternate" ->
                    Elem;
                _ ->
                    NewCandidate = case Candidate of
                        unknown -> Elem;
                        _ -> Candidate
                    end,
                    getLink(Elements, NewCandidate)
            end;
        _ ->
            getLink(Elements, Candidate)
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
    <<>>;
asText(Text) when length(Text) > 1 ->
    unicode:characters_to_binary(Text);
asText([TextNode]) when is_record(TextNode, xmlText) ->
    unicode:characters_to_binary(TextNode#xmlText.value).

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
    "(?<year>\\d{4})-(?<month>(0\\d|1[012]))-(?<day>([012]\\d)|(3[01]))T(?<hour>[0-2]\\d):(?<min>[0-6]\\d):(?<sec>[0-6]\\d)(.[\\d]{0,3}){0,1}(([A-Z]+)|((\\+|\\-)\\d{2}:\\d{2}))".

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
