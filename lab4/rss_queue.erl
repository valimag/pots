%% @doc Модуль `rss_queue` предназначен для управления очередью RSS-лент.
%%
%% Этот модуль включает в себя функциональность для создания сервера очереди,
%% добавления элементов в очередь, извлечения всех элементов из очереди,
%% и выполнения тестирования функционала.
%%
%% @end
-module(rss_queue).

%% Подключаем необходимые заголовочные файлы для работы с XML и HTTP.
-include("/opt/homebrew/Cellar/erlang/26.2.2/lib/erlang/lib/xmerl-1.3.34/include/xmerl.hrl").
-include("/opt/homebrew/Cellar/erlang/26.2.2/lib/erlang/lib/inets-9.1/include/httpd.hrl").

%% Экспортируем функции, доступные для вызова из других модулей.
-export([start/0, add_item/2, add_feed/2, get_all/1, server/1, test/0]).

%% Определяем таймаут для операций, требующих ожидания.
-define(TIMEOUT, 1000).

%% @doc Запускает новый сервер очереди RSS и возвращает PID созданного процесса.
%%
%% @spec start() -> {ok, pid()}.
%%
start() ->
    %% Создаем новый процесс сервера очереди с пустым начальным состоянием (списком).
    %% ?MODULE имя текущего модуля, в виде атома
    %%  server - это функция, которую нужно выполнить в новом процессе.
    %% [[]] - это список аргументов, передаваемых функции server.
    Pid = spawn(?MODULE, server, [[]]),
    %% Возвращаем PID созданного процесса.
    {ok, Pid}.

%% @doc Добавляет элемент в очередь RSS.
%%
%% @spec add_item(pid(), any()) -> ok.
%%
%% @param QPid PID процесса сервера очереди.
%% @param Item Элемент, который нужно добавить в очередь.
%%
%% Условие when is_pid(QPid) гарантирует, что QPid действительно является PID.
add_item(QPid, Item) when is_pid(QPid) ->
    %% Отправляем сообщение процессу сервера очереди с элементом для добавления.
    QPid ! {add_item, Item},
    %% Возвращаем атом `ok` как подтверждение успешного выполнения.
    ok.

%% @doc Добавляет все элементы RSS-ленты в очередь.
%%
%% @spec add_feed(pid(), list()) -> ok.
%%
%% @param QPid PID процесса сервера очереди.
%% @param RSS2Feed Список элементов RSS-ленты.
%%
%% Условие when is_pid(QPid) гарантирует, что QPid действительно является PID.
add_feed(QPid, RSS2Feed) when is_pid(QPid) ->
    %% Извлекаем элементы ленты с помощью функции `get_feed_items` из модуля `rss_parse`.
    Items = rss_parse:get_feed_items(RSS2Feed, []),
    %% Для каждого элемента ленты вызываем функцию `add_item`, чтобы добавить его в очередь.
    lists:foreach(fun(Item) -> add_item(QPid, Item) end, Items),
    %% Возвращаем атом `ok` после добавления всех элементов.
    ok.

%% @doc Извлекает все элементы из очереди RSS и возвращает их.
%%
%% @spec get_all(pid()) -> {ok, list()} | {error, timeout}.
%%
%% @param QPid PID процесса сервера очереди.
%%
%% Условие when is_pid(QPid) гарантирует, что QPid действительно является PID.
get_all(QPid) when is_pid(QPid) ->
    %% Отправляем запрос на получение всех элементов из очереди.
    QPid ! {get_all, self()},
    %% Ожидаем ответа от сервера очереди с таймаутом.
    receive
        {all_items, Items} -> {ok, Items}
    after ?TIMEOUT ->
        {error, timeout}
    end.


%% @doc Цикл сервера для обработки сообщений, связанных с управлением очередью RSS.
%%
%% @spec server(list()) -> no_return().
%%
%% @param Queue Текущее состояние очереди, список элементов RSS.
%%
server(Queue) ->
    %% Ожидание и обработка входящих сообщений.
    receive
        {add_item, RSSItem} ->
            %% Добавление нового элемента RSS в очередь.
            NewQueue = update_queue(Queue, RSSItem),
            %% Рекурсивный вызов для продолжения обработки сообщений.
            server(NewQueue);
        {get_all, ReqPid} ->
            %% Отправка всех элементов очереди запрашивающему процессу.
            ReqPid ! {all_items, Queue},
            %% Продолжение обработки входящих сообщений.
            server(Queue)
    end.

%% @doc Обновляет очередь, добавляя новый элемент или заменяя существующий.
%%
%% @spec update_queue(list(), any()) -> list().
%%
%% @param Queue Текущее состояние очереди.
%% @param RSSItem Новый элемент для добавления в очередь.
%%
%% @return Новое состояние очереди после добавления или обновления элемента.
%%
update_queue(Queue, RSSItem) ->
    %% Определение действий на основе сравнения нового элемента с элементами в очереди.
    case find_item(Queue, RSSItem) of
        {same, _} ->
            %% Элемент уже присутствует в очереди, игнорируем его.
            Queue;
        {updated, OldItem} ->
            %% Новый элемент обновляет существующий, удаляем старый и добавляем новый.
            %% Список сортируется с учетом времени публикации после добавления.
            sort_queue_by_time([RSSItem | Queue -- [OldItem]]);
        different ->
            %% Новый элемент уникален, просто добавляем его в очередь.
            %% Список сортируется с учетом времени публикации после добавления.
            sort_queue_by_time([RSSItem | Queue])
    end.

%% @doc Сортирует элементы очереди по времени публикации.
%%
%% @spec sort_queue_by_time(list()) -> list().
%%
%% @param Queue Текущее состояние очереди.
%%
%% @return Очередь, отсортированная по времени публикации элементов.
%%
sort_queue_by_time(Queue) ->
    lists:sort(fun(Item1, Item2) ->
        %% Извлечение времени публикации для двух элементов.
        Time1 = rss_parse:get_item_time(Item1),
        Time2 = rss_parse:get_item_time(Item2),
        %% Сравнение времени для определения порядка элементов.
        Time1 < Time2
    end, Queue).

%% @doc Находит элемент в очереди, который соответствует или обновляется новым элементом.
%%
%% @spec find_item(list(), any()) -> {Action, Element} | Action.
%%
%% @param Queue Текущее состояние очереди.
%% @param RSSItem Элемент, который сравнивается с элементами в очереди.
%%
%% @return Возвращает кортеж `{Action, Element}`, где `Action` может быть `same`, `updated` или `different`,
%%         а `Element` - элемент в очереди, который соответствует условиям. Если элемент не найден, возвращается `different`.
%%
find_item(Queue, RSSItem) ->
    %% lists:foldl/3: Это функция свертки списка. Она применяет заданную функцию к каждому элементу списка и аккумулирует результаты.
    lists:foldl(
        %% Item представляет собой текущий элемент очереди, а Acc - аккумулированное значение, которое изменяется с каждой итерацией.
        fun(Item, Acc) ->
            %% Сравнение каждого элемента в очереди с новым элементом.
            case rss_parse:compare_feed_items(Item, RSSItem) of
                same -> {same, Item};
                updated -> {updated, Item};
                different -> Acc
            end
        end,
        different,
        Queue
    ).


%% @doc Функция `test` выполняет серию тестов для проверки функциональности сервера очереди RSS.
%%
%% Эта функция проверяет следующие аспекты:
%% - Запуск сервера очереди RSS.
%% - Чтение и проверка валидности RSS-лент.
%% - Добавление элементов из RSS-лент в очередь.
%% - Извлечение и проверка всех элементов из очереди.
%% - Проверка корректности сортировки элементов в очереди по времени публикации.
%%
%% Вывод результатов теста записывается в файл `testing.txt`.
%%
%% @spec test() -> ok.
%%
test() ->
    %% Открываем файл для записи результатов тестирования.
    {ok, FileHandle} = file:open("testing.txt", [write]),
    
    %% Перенаправляем вывод `io:format` в файл.
    OldGroupLeader = group_leader(),
    group_leader(FileHandle, self()),

    %% Начало тестирования.
    io:format("Starting RSS Queue Server Test...~n"),

    %% Запуск сервера очереди RSS и вывод PID запущенного сервера.
    {ok, QueuePid} = start(),
    io:format("RSS Queue Server started with PID ~p.~n", [QueuePid]),

    %% Чтение и проверка RSS-лент на соответствие формату RSS 2.0.
    io:format("Reading and verifying RSS feeds...~n"),
    RSS1 = xmerl_scan:file("digg-science-rss1.xml"),
    RSS2 = xmerl_scan:file("digg-science-rss2.xml"),
    io:format("Feed 1 is valid RSS 2.0: ~p~nFeed 2 is valid RSS 2.0: ~p~n", [rss_parse:is_rss2_feed(RSS1), rss_parse:is_rss2_feed(RSS2)]),

    %% Добавление элементов из проверенных RSS-лент в очередь.
    {Feed1, _} = RSS1,
    {Feed2, _} = RSS2,
    io:format("Adding items from RSS feeds to the queue...~n"),
    add_feed(QueuePid, Feed1),
    add_feed(QueuePid, Feed2),

    %% Извлечение всех элементов из очереди и их вывод.
    io:format("Retrieving all items from the queue...~n"),
    {ok, Items} = get_all(QueuePid),
    io:format("All items in the queue: ~p~n", [Items]),

    %% Проверка корректности сортировки элементов в очереди по времени публикации.
    check_items_sorted(Items),

    %% Завершение тестирования.
    io:format("RSS Queue Server Test Completed Successfully.~n"),
    
    %% Восстановление первоначального Group Leader для текущего процесса.
    group_leader(OldGroupLeader, self()),

    %% Закрытие файла с результатами тестирования.
    file:close(FileHandle),
    ok.

%% @doc Проверяет, отсортированы ли элементы очереди по времени публикации.
%%
%% Элементы считаются корректно отсортированными, если время публикации каждого
%% последующего элемента не меньше времени публикации предыдущего.
%%
%% @spec check_items_sorted(list()) -> ok.
%%
%% @param Items Список элементов очереди для проверки.
%%
check_items_sorted(Items) ->
    %% Извлечение времени публикации для всех элементов, за исключением тех, что имеют значение времени bad_date.
    Times = [rss_parse:get_item_time(Item) || Item <- Items, rss_parse:get_item_time(Item) /= bad_date],
    %% Сортировка извлеченных времен публикации для сравнения с исходным списком.
    Sorted = lists:sort(Times),
    %% Сравнение отсортированного списка времен публикации с исходным для определения корректности порядка элементов в очереди.
    case Times == Sorted of
        true -> 
            %% Если списки совпадают, элементы отсортированы корректно.
            io:format("Items are correctly sorted by publication time.~n");
        false -> 
            %% Если списки не совпадают, порядок элементов некорректен.
            io:format("Error: Items are not correctly sorted by publication time.~n")
    end.