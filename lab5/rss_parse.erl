-module(rss_parse).
-include("C:/Program Files/erl6.1/lib/xmerl-1.3.7/include/xmerl.hrl").
-include("C:/Program Files/erl6.1/lib/inets-5.10.2/include/httpd.hrl").

-export([result/0, is_rss2_feed/1, get_feed_items/2, get_item_time/1, compare_feed_items/2]).

%% @doc Проверяет, является ли предоставленный XML документ лентой RSS 2.0.
%%
%% Функция анализирует корневой элемент XML документа, полученного
%% в результате выполнения `xmerl_scan:file/1` или `xmerl_scan:string/1`,
%% и проверяет, соответствует ли документ спецификации RSS 2.0.
%% Для этого она проверяет наличие атрибута `version="2.0"` у корневого элемента `rss`.
%%
%% @spec is_rss2_feed(Tuple) -> boolean() где
%%       Tuple = {xmlElement(), _} - кортеж, где первый элемент - корневой элемент XML-документа,
%%       полученный от `xmerl_scan`.
%%
%% @param Data - кортеж, содержащий результат парсинга XML-документа и дополнительные данные.
%%
%% @return `true` если документ является лентой RSS 2.0, иначе `false`.
is_rss2_feed({R, _}) ->
    %% Проверка атрибутов корневого элемента на наличие атрибута version="2.0"
    %% list:any - перебирает элементы списка (список атрибутов корневого элемента XML)
    %% Передается анонимная функция для проверки соответствия условию
    lists:any(fun
                (#xmlAttribute{name = version, value = "2.0"}) -> true;  %% Условие для RSS 2.0
                (_) -> false                                             %% Для всех остальных случаев возвращаем false [(_) - "заглушка"]
              end, R#xmlElement.attributes)
    andalso R#xmlElement.name == rss.  %% Дополнительная проверка имени корневого элемента с использованием "ленивой" конъюнкции

%% @doc Извлекает элементы <item> из XML документа RSS 2.0.
%%
%% Эта функция рекурсивно обходит структуру XML документа,
%% начиная с корневого элемента, и собирает все элементы <item>,
%% которые являются основными составляющими новостной ленты RSS.
%% Функция предназначена для работы со структурой данных, полученной
%% в результате парсинга XML с использованием xmerl_scan:file/1
%% или xmerl_scan:string/1.
%%
%% @spec get_feed_items(xmlElement(), list()) -> list().
%%       xmlElement() - тип, представляющий элемент XML.
%%       list() - аккумулирующий список, в который добавляются найденные элементы <item>.
%%
%% @param Record - текущий обрабатываемый элемент XML. В первом вызове
%%                 это должен быть корневой элемент документа.
%% @param List - аккумулирующий список, используемый для сбора элементов <item>.
%%               В первом вызове это должен быть пустой список [].
%%
%% @return Список, содержащий все элементы <item> и их содержимое из документа RSS.
%%
%% Пример использования:
%%      {Root, _} = xmerl_scan:file("path/to/rss_feed.xml"),
%%      Items = get_feed_items(Root, []).
get_feed_items(Record, List) ->
    %% Оператор case для сопоставления с образцом 
    case Record of
        %% Если текущий элемент является элементом <item>, добавляем его в аккумулирующий список.
        #xmlElement{name = item} -> [Record | List];
        
        %% Если текущий элемент содержит дочерние элементы, рекурсивно продолжаем поиск
        %% элементов <item> в его содержимом.
        #xmlElement{content = Content} -> lists:foldl(fun get_feed_items/2, List, Content);
        
        %% Во всех остальных случаях возвращаем накопленный список без изменений.
        _ -> List
    end.

%% @doc Преобразует элемент RSS-ленты в кортеж с его ключевыми характеристиками.
%%
%% Извлекает и возвращает кортеж с GUID, заголовком, ссылкой и датой публикации 
%% для заданного элемента ленты.
%%
%% @spec get_item_string(xmlElement()) -> {guid(), title(), link(), pubDate()}.
%%
%% @param Item - элемент ленты RSS в формате #xmlElement, из которого 
%%               необходимо извлечь данные.
%%
%% @return Возвращает кортеж {GUID, Title, Link, Time}, содержащий значения 
%%         указанных параметров. Если какой-либо из параметров отсутствует,
%%         в кортеже будет значение `false` для соответствующего параметра.
get_item_string(Item) ->
    {
        get_item_parameter(Item, guid),   %% Вызываем get_item_parameter для извлечения guid и сохраняем результат.
        get_item_parameter(Item, title),  %% Вызываем get_item_parameter для извлечения title и сохраняем результат.
        get_item_parameter(Item, link),   %% Вызываем get_item_parameter для извлечения link и сохраняем результат.
        get_item_parameter(Item, pubDate) %% Вызываем get_item_parameter для извлечения pubDate и сохраняем результат.
    }.

%% @doc Преобразует список элементов RSS-ленты в список кортежей с их характеристиками.
%%
%% Применяет функцию `get_item_string/1` к каждому элементу списка `Feed`, 
%% возвращая список кортежей с данными элементов ленты.
%%
%% @spec get_items_string(list(xmlElement())) -> list(tuple()).
%%
%% @param Feed - список элементов ленты RSS в формате #xmlElement.
%%
%% @return Возвращает список кортежей, где каждый кортеж содержит GUID, заголовок, 
%%         ссылку и дату публикации одного элемента ленты.
get_items_string(Feed) -> 
    % Используем lists:map для применения get_item_string ко всем элементам Feed.
    % Результат каждого вызова добавляется в новый список, который возвращается.
    lists:map(fun get_item_string/1, Feed).

%% @doc Извлекает значение заданного параметра из элемента ленты RSS.
%%
%% Функция ищет в содержимом элемента `Item` подэлемент с именем `Parameter`
%% и возвращает значение этого подэлемента. Поддерживаются параметры 'guid', 
%% 'title', 'link', и 'pubDate'.
%%
%% @spec get_item_parameter(xmlElement(), atom()) -> Value | false.
%%
%% @param Item - элемент ленты RSS в формате #xmlElement, из которого нужно 
%%               извлечь параметр.
%% @param Parameter - атом, указывающий имя параметра ('guid', 'title', 'link', 
%%                    'pubDate'), значение которого нужно получить.
%%
%% @return Возвращает строковое значение параметра, если он найден, иначе `false`.
get_item_parameter(Item, Parameter) ->
    % Используем lists:filter для поиска подэлементов Item, удовлетворяющих условиям.
    % Элемент должен быть xmlElement с именем, соответствующим Parameter.
    case lists:filter(fun(Elem) -> 
                          % Проверяем, соответствует ли Elem заданным критериям.
                          case Elem of
                              #xmlElement{name = Param, content = [Content | _]} when Param == Parameter -> 
                                  % Если да, проверяем, является ли содержимое Content xmlText.
                                  is_record(Content, xmlText);
                              _ -> 
                                  % Если нет, возвращаем false.
                                  false
                          end
                      end, Item#xmlElement.content) of
        % Если нашли подходящие элементы, берем первый из них и его значение.
        [#xmlElement{content = [#xmlText{value = Value} | _]} | _] -> 
            Value; % Возвращаем значение найденного элемента.
        % Если подходящих элементов нет, возвращаем false.
        [] -> 
            false
    end.

%% @doc Извлекает и преобразует время публикации элемента ленты RSS в целое число.
%%
%% Функция извлекает строку времени публикации из элемента `Item`, конвертирует её 
%% в формат {{Year,Month,Day},{Hour,Min,Sec}} и преобразует в количество секунд 
%% отсчитанных с начала григорианского календаря.
%%
%% @spec get_item_time(xmlElement()) -> integer() | bad_date.
%%
%% @param Item - элемент ленты RSS в формате #xmlElement, из которого нужно 
%%               извлечь и преобразовать дату публикации.
%%
%% @return Возвращает целое число, представляющее время публикации в секундах,
%%         или атом `bad_date`, если дата не может быть преобразована.
get_item_time(Item) ->
    % Получаем значение pubDate для Item.
    case get_item_parameter(Item, pubDate) of
        % Если значение не найдено, возвращаем специальный атом bad_date.
        false -> bad_date;
        % Если значение найдено, пытаемся преобразовать его в дату и время.
        Value ->
            case httpd_util:convert_request_date(Value) of
                % Если преобразование не удалось, возвращаем bad_date.
                bad_date -> bad_date;
                % Если преобразование удалось, преобразуем дату и время в секунды
                % с начала григорианского календаря.
                RequestDate -> calendar:datetime_to_gregorian_seconds(RequestDate)
            end
    end.

%% @doc Сравнивает два элемента RSS-ленты и определяет, является ли новый элемент обновлением старого.
%%
%% Функция анализирует элементы RSS-ленты, основываясь на ключевых параметрах:
%% guid, title и link. В зависимости от совпадения значений этих параметров
%% возвращается атом, описывающий отношение между элементами.
%%
%% @spec compare_feed_items(xmlElement(), xmlElement()) -> same | updated | different.
%%
%% @param OldItem - #xmlElement, представляющий старый элемент ленты.
%% @param NewItem - #xmlElement, представляющий новый элемент ленты.
%%
%% @return Возвращает атом updated, если найдено совпадение по любому из ключей,
%%         different, если совпадений не найдено.
compare_feed_items(OldItem, NewItem) ->
    % Проверяем, полностью ли совпадают старый и новый элементы.
    % Для этого необходимо определить функцию полного сравнения, учитывающую всю структуру элемента.
    % Это примерный подход, так как полное сравнение структур может быть непросто из-за
    % возможных различий в несущественных атрибутах, таких как порядок атрибутов, пространства имен и т.д.
    if OldItem == NewItem ->
        same;
    true ->
        % Если элементы не совпадают полностью, продолжаем сравнение по ключам.
        compare_by_keys([guid, title, link], OldItem, NewItem)
    end.

%% @doc Вспомогательная функция для сравнения элементов RSS-ленты по заданным ключам.
%%
%% Рекурсивно обходит список ключей, сравнивая значения соответствующих параметров
%% в старом и новом элементах. Если параметры совпадают, считается, что новый элемент
%% является обновлением старого. Если совпадения не найдено, элементы считаются различными.
%%
%% @spec compare_by_keys([atom()], xmlElement(), xmlElement()) -> updated | different.
%%
%% @param [Key|Keys] - Список ключей (атомов), по которым необходимо сравнить элементы.
%% @param OldItem - Старый элемент ленты для сравнения.
%% @param NewItem - Новый элемент ленты для сравнения.
%%
%% @return Возвращает атом updated, если параметры совпадают по любому из ключей,
%%         иначе продолжает сравнение по следующим ключам, пока список не исчерпан,
%%         возвращая different, если совпадений не найдено.
compare_by_keys([Key|Keys], OldItem, NewItem) ->
    % Извлекаем значения параметров для текущего ключа из старого и нового элементов.
    OldParam = get_item_parameter(OldItem, Key),
    NewParam = get_item_parameter(NewItem, Key),
    % Определяем действие на основе сравнения извлеченных параметров.
    case {OldParam, NewParam} of
        % Если параметр отсутствует в обоих элементах, продолжаем сравнение по следующему ключу.
        {false, false} -> 
            compare_by_keys(Keys, OldItem, NewItem); % Продолжаем сравнение по следующему ключу.
        % Если параметры совпадают, возвращаем updated, указывая на обновление элемента.
        {Param, Param} -> 
            updated; % Возвращаем updated, так как параметры совпадают.
        % Если текущий параметр не приводит к классификации как updated, продолжаем сравнение.
        _ -> 
            compare_by_keys(Keys, OldItem, NewItem) % Продолжаем сравнение.
    end;
% Если список ключей исчерпан, и совпадений не найдено, возвращаем different.
compare_by_keys([], _, _) ->
    different. % Возвращаем different, так как совпадений не найдено.

%% @doc Создает уникальный упорядоченный список элементов из двух RSS-лент, удаляя дубликаты.
%%
%% Эта функция объединяет элементы из двух списков RSS-лент, удаляет дублирующиеся элементы,
%% и возвращает итоговый список элементов, отсортированный по времени публикации.
%%
%% @spec build_unique_feed(list(xmlElement()), list(xmlElement())) -> list(xmlElement()).
%%
%% @param Feed1Items - список элементов первой RSS-ленты.
%% @param Feed2Items - список элементов второй RSS-ленты.
%%
%% @return Список уникальных и отсортированных по времени публикации элементов,
%%         объединенных из двух исходных списков.
build_unique_feed(Feed1Items, Feed2Items) ->
    % Используем lists:filter для фильтрации списка Feed1Items. Для каждого элемента
    % этого списка проверяем, существует ли аналогичный элемент в списке Feed2Items.
    UniqueFeed1Items = lists:filter(fun(Item1) ->
        % Используем lists:any и функцию compare_feed_items для проверки, является ли
        % элемент Item1 дубликатом любого элемента в Feed2Items. Если не является,
        % элемент считается уникальным и остается в фильтрованном списке.
        not lists:any(fun(Item2) -> compare_feed_items(Item1, Item2) == same end, Feed2Items)
    end, Feed1Items),

    % Объединяем отфильтрованные уникальные элементы из Feed1Items с элементами Feed2Items,
    % формируя итоговый список элементов.
    CombinedFeed = UniqueFeed1Items ++ Feed2Items,

    % Сортируем итоговый список элементов по времени публикации, используя функцию
    % get_item_time для извлечения временной метки каждого элемента. Результатом
    % является упорядоченный список уникальных элементов из обоих исходных списков.
    lists:sort(fun(Item1, Item2) -> get_item_time(Item1) < get_item_time(Item2) end, CombinedFeed).

%% @doc Выводит информацию о валидности RSS-лент и отображает содержимое лент.
result() ->
    %% Загрузка и проверка RSS-лент на валидность формата RSS 2.0.
    RSS1 = xmerl_scan:file("digg-science-rss1.xml"),
    RSS2 = xmerl_scan:file("digg-science-rss2.xml"),
    io:format("Feed 1 is valid RSS 2.0: ~p~nFeed 2 is valid RSS 2.0: ~p~n", [is_rss2_feed(RSS1), is_rss2_feed(RSS2)]),

    %% Извлечение и обработка элементов из каждой ленты.
    {Feed1,_} = RSS1,
    {Feed2,_} = RSS2,
    DisplayFeeds = [
        {"First news feed", get_feed_items(Feed1, [])},
        {"Second news feed", get_feed_items(Feed2, [])},
        {"Unique news feed", build_unique_feed(get_feed_items(Feed1, []), get_feed_items(Feed2, []))}
    ],

    %% Вывод информации о каждой ленте.
    lists:foreach(fun({Title, Items}) ->
        DisplayItems = get_items_string(Items),
        io:format("~s (~p articles):~n~p~n", [Title, length(DisplayItems), DisplayItems])
    end, DisplayFeeds).