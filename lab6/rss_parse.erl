%% @doc Модуль для парсинга и анализа RSS лент.
%%
%% Этот модуль содержит функции для проверки соответствия XML документа формату RSS 2.0,
%% извлечения элементов ленты, получения времени публикации элемента и сравнения элементов ленты.
-module(rss_parse).

%% @doc Использование внешних типов данных и функций из модулей xmerl и inets.
-include("C:/Program Files/erl6.1/lib/xmerl-1.3.7/include/xmerl.hrl").
-include("C:/Program Files/erl6.1/lib/inets-5.10.2/include/httpd.hrl").

%% @doc Экспорт функций модуля.
-export ([is_rss2_feed/1]).
-export ([get_feed_items/1]).
-export ([get_item_time/1]).
-export ([compare_feed_items/2]).

%% @doc Проверяет, соответствует ли XML документ формату RSS 2.0.
%% Возвращает `ok` если версия 2.0, иначе `false`.
is_rss2_feed(XML) ->
    %% XPath запрос для получения атрибута версии.
    VerXPath = "@version",
    case XML#xmlElement.name of
        rss -> case (lists:nth(1, xmerl_xpath:string(VerXPath, XML)))#xmlAttribute.value of
            "2.0" -> ok;
            _ -> false
        end; 
        _ -> false
    end.

%% @doc Извлекает элементы ленты из RSS 2.0 документа.
get_feed_items(RSS2Feed) ->
    %% XPath запрос для извлечения всех элементов `item`.
    ItemsXPath = "//item",
    xmerl_xpath:string(ItemsXPath, RSS2Feed).

%% @doc Получает время публикации элемента ленты.
%% Возвращает количество секунд в григорианском формате.
get_item_time(Item) ->
    %% XPath запрос для извлечения времени публикации.
    PubDateXPath = "//pubDate",
    [PubDateNode | _] = xmerl_xpath:string(PubDateXPath, Item),
    [PubDate | _] = PubDateNode#xmlElement.content,
    DateTime = httpd_util:convert_request_date(PubDate#xmlText.value), % Возвращает `bad_time` при ошибке
    calendar:datetime_to_gregorian_seconds(DateTime).

%% @doc Рекурсивно очищает xmlElement от внешних ссылок, позиции и родителей.
extract_xml(Elem = #xmlElement{}) ->
    Elem#xmlElement{parents=[], pos=0,
        content=lists:map(fun extract_xml/1, Elem#xmlElement.content),
        attributes=lists:map(fun extract_xml/1, Elem#xmlElement.attributes)};
extract_xml(Attr = #xmlAttribute{}) ->
    Attr#xmlAttribute{parents=[], pos=0};
extract_xml(Text = #xmlText{}) ->
    Text#xmlText{parents=[], pos=0};
extract_xml(Comment = #xmlComment{}) ->
    Comment#xmlComment{parents=[], pos=0};
extract_xml(Other) ->
    Other.

%% @doc Сравнивает два элемента ленты на предмет идентичности или обновления.
%% Возвращает `same`, `updated` или `different`.
compare_feed_items(OldItemRow, NewItemRow) ->
    OldItem = extract_xml(OldItemRow),
    NewItem = extract_xml(NewItemRow),

    case OldItem =:= NewItem of
        true -> same;
        _ ->
            Handler = fun(E, Acc) ->
                Acc or is_updated(OldItem, NewItem, E)
            end,
            Acc = false,
            ListXPathAttrs = ["//guid", "//title", "//link"],

            case lists:foldl(Handler, Acc, ListXPathAttrs) of
                true -> updated;
                _ -> different
            end
    end.

%% @doc Проверяет, обновлен ли элемент ленты по определенному XPath атрибуту.
is_updated(OldItem, NewItem, XPathAttr) ->
    OldAttrValue = xmerl_xpath:string(XPathAttr, OldItem),
    case OldAttrValue of
        [] -> false;
        _ -> NewAttrValue = xmerl_xpath:string(XPathAttr, NewItem),
            case NewAttrValue of
                [] -> false;
                _ ->
                    [OldGUID] = OldAttrValue,
                    [NewGUID] = NewAttrValue,
                    OldGUID == NewGUID
            end
    end.
