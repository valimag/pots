%% @doc Модуль `rss_queue` предоставляет функциональность для работы с очередью RSS-лент.
%% Он поддерживает создание очередей, добавление элементов в очередь, извлечение всех элементов из очереди,
%% и подписку на обновления других очередей.
-module(rss_queue).

%% Включаем заголовочные файлы для работы с логированием, парсингом XML и HTTP запросами.
-include("logging.hrl").
-include("C:/Program Files/erl6.1/lib/xmerl-1.3.7/include/xmerl.hrl").
-include("C:/Program Files/erl6.1/lib/inets-5.10.2/include/httpd.hrl").

%% Экспортируемые функции модуля.
-export([start/0, start/1, add_item/2, add_feed/2, get_all/1, server/2, test/0]).

%% Определение таймаута для ожидания ответа.
-define(TIMEOUT, 10000).

%% @doc `init/1` инициализирует новую очередь и, опционально, запускает процесс `rss_reader` для чтения RSS-ленты по URL.
%% Функция поддерживает два режима работы: автономный и с загрузкой RSS-ленты по URL.
%%
%% @spec init(Args::list()) -> {ok, pid()}.
%%       Args - список аргументов. Если список пуст, запускается автономный режим.
%%       Если список содержит URL (строку), запускается режим с загрузкой RSS-ленты по этому URL.
%%
%% @return Возвращает кортеж `{ok, pid()}`, где `pid()` - идентификатор процесса сервера очереди.
init(Args) ->
    %% Инициализация пустой очереди.
    Q = [],
    %% Запуск серверного процесса с пустой очередью и пустым набором подписчиков.
    QPid = spawn(?MODULE, server, [Q, sets:new()]),
    %% Определение режима работы на основе аргументов.
    case Args of
        [] ->
            %% В автономном режиме просто возвращаем PID сервера очереди.
            {ok, QPid};
        [Url] ->
            %% В режиме с URL запускаем `rss_reader` для чтения ленты и передаем PID сервера очереди.
            rss_reader:start(Url, QPid),
            %% Возвращаем PID сервера очереди.
            {ok, QPid}
    end.

%% @doc Функция `start/0` инициализирует новую очередь в автономном режиме (без чтения RSS-ленты из URL).
%%
%% @spec start() -> {ok, Pid::pid()}.
%% @return Возвращает кортеж `{ok, Pid}`, где `Pid` - идентификатор процесса сервера очереди.
start() ->
    %% Вызов функции init с пустым списком аргументов для инициализации в автономном режиме.
    init([]).

%% @doc Функция `start/1` инициализирует новую очередь и запускает чтение RSS-ленты из указанного URL.
%%
%% @spec start(Url::string()) -> {ok, Pid::pid()}.
%%       Url - URL RSS-ленты для чтения и добавления в очередь.
%%
%% @return Возвращает кортеж `{ok, Pid}`, где `Pid` - идентификатор процесса сервера очереди.
start(Url)->
    %% Вызов функции init с URL в качестве аргумента для инициализации с чтением RSS-ленты.
    init([Url]).

%% @doc Функция `add_item` добавляет один элемент ленты в очередь.
%%
%% @spec add_item(QPid::pid(), Item::any()) -> ok.
%%       QPid - идентификатор процесса сервера очереди.
%%       Item - элемент ленты для добавления в очередь.
%%
%% @return Возвращает атом `ok` после добавления элемента.
add_item(QPid, Item) when is_pid(QPid) ->
    %% Отправка сообщения с элементом ленты указанному процессу сервера очереди.
    QPid ! {add_item, Item},
    %% Возвращаем атом `ok`.
    ok.

%% @doc Функция `add_feed` добавляет все элементы из RSS-ленты в очередь.
%%
%% @spec add_feed(QPid::pid(), RSS2Feed::any()) -> ok.
%%       QPid - идентификатор процесса сервера очереди.
%%       RSS2Feed - данные RSS-ленты для добавления в очередь.
%%
%% @return Возвращает атом `ok` после добавления всех элементов ленты.
add_feed(QPid, RSS2Feed) when is_pid(QPid) ->
    %% Извлекаем элементы ленты с помощью функции `get_feed_items` из модуля `rss_parse`.
    %% Второй аргумент - пустой список, так как `get_feed_items` рекурсивно обходит XML,
    %% собирая элементы ленты.
    Items = rss_parse:get_feed_items(RSS2Feed, []),
    %% Для каждого элемента ленты вызываем функцию `add_item`, чтобы добавить его в очередь.
    lists:foreach(fun(Item) -> add_item(QPid, Item) end, Items),
    %% Возвращаем атом `ok` после добавления всех элементов.
    ok.

%% @doc Получает все элементы из очереди RSS, отправленные указанному процессу.
%%
%% @spec get_all(pid()) -> {ok, list()} | {error, Reason}.
%% @param QPid PID процесса сервера очереди, от которого запрашиваются элементы.
%% @return Возвращает кортеж `{ok, Items}`, где Items - список элементов очереди, 
%% или `{error, Reason}`, если произошла ошибка.
get_all(QPid) when is_pid(QPid) ->
    %% Отправка запроса на получение всех элементов очереди текущему процессу.
    QPid ! {get_all, self()},
    receive
        %% Ожидание ответа с элементами очереди.
        {all_items, Items} ->
            %% Вывод размера очереди и PID процесса, от которого был получен ответ.
            io:format("Queue size = ~p items from the feed to ~p ~n", [length(Items), QPid]),
            %% Возвращение списка элементов.
            {ok, Items};
        %% Обработка сообщения об ошибке.
        {error, Reason} ->
            %% Возвращение причины ошибки.
            {error, Reason};
        %% Обработка неизвестных сообщений.
        Msg ->
            %% Логирование неизвестного сообщения.
            ?ERROR("Unknown message: ~p~n", [Msg]),
            %% Возвращение сообщения об неизвестной ошибке.
            {unknown_msg, Msg}
    after 
        %% Ограничение времени ожидания ответа.
        ?TIMEOUT -> 
            %% Логирование превышения времени ожидания.
            ?ERROR("Timeout exceeded: module ~p, ~p in line ~p.~n", 
                    [?MODULE_STRING, get_all, ?LINE]),
            %% Возвращение сообщения о тайм-ауте.
            {error, timeout}
    end.

%% @doc Основной цикл сервера для обработки запросов и управления подписчиками.
%%
%% @spec server(list(), set()) -> none().
%% @param Queue Текущая очередь элементов RSS.
%% @param Subscribers Множество подписчиков (PID процессов).
server(Queue, Subscribers) ->
    receive
        %% Добавление нового элемента в очередь.
        {add_item, RSSItem} ->
            %% Обновление очереди и списка подписчиков с добавлением нового элемента.
            {NewQueue, NewSubscribers} = update_queue(Queue, RSSItem, Subscribers),
            %% Продолжение работы сервера с обновленными данными.
            server(NewQueue, NewSubscribers);
        %% Запрос на получение всех элементов очереди.
        {get_all, ReqPid} ->
            %% Отправка всех элементов очереди запрашивающему процессу.
            ReqPid ! {all_items, Queue},
            %% Продолжение работы сервера.
            server(Queue, Subscribers);
        %% Подписка нового подписчика на обновления очереди.
        {subscribe, QPid} ->
            %% Добавление подписчика в множество, если он еще не подписан.
            NewSubscribers = case sets:is_element(QPid, Subscribers) of
                                true -> Subscribers; % Подписчик уже существует.
                                false ->
                                    %% Мониторинг нового подписчика для отслеживания его состояния.
                                    erlang:monitor(process, QPid),
                                    %% Отправка текущих элементов очереди новому подписчику.
                                    [add_item(QPid, Item) || Item <- Queue],
                                    %% Добавление нового подписчика в множество.
                                    sets:add_element(QPid, Subscribers)
                             end,
            %% Продолжение работы сервера с обновленным списком подписчиков.
            server(Queue, NewSubscribers);
        %% Отписка подписчика от обновлений очереди.
        {unsubscribe, QPid} ->
            %% Удаление подписчика из множества подписчиков.
            NewSubscribers = sets:del_element(QPid, Subscribers),
            %% Продолжение работы сервера без удаленного подписчика.
            server(Queue, NewSubscribers);
        %% Обработка сообщения о завершении работы подписчика.
        {'DOWN', _, _, QPid, _Reason} ->
            %% Удаление завершившегося подписчика из множества.
            NewSubscribers = sets:del_element(QPid, Subscribers),
            %% Продолжение работы сервера после удаления подписчика.
            server(Queue, NewSubscribers);
        %% Обработка неизвестных сообщений.
        _Msg ->
            %% Логирование неизвестного сообщения.
            ?ERROR("Unknown message: ~p~n", [_Msg]),
            %% Продолжение работы сервера без изменений.
            server(Queue, Subscribers)
    end.

%% @doc Обновляет очередь RSS-элементов, добавляя новый элемент или обновляя существующий,
%% и оповещает подписчиков об изменениях.
%%
%% Эта функция анализирует, нужно ли добавить новый элемент в очередь или обновить существующий,
%% а также обновляет список подписчиков, отправляя им новый или обновлённый элемент.
%%
%% @spec update_queue(list(), RSSItem, set()) -> {list(), set()}.
%% @param Queue Текущая очередь элементов.
%% @param RSSItem Элемент RSS, который нужно добавить или обновить в очереди.
%% @param Subscribers Множество PID подписчиков на очередь.
%% @return Возвращает кортеж с обновлённой очередью и множеством подписчиков.
update_queue(Queue, RSSItem, Subscribers) ->
    %% Определение действия на основе сравнения нового элемента с элементами в очереди.
    {NewQueue, UpdatedSubscribers} = case find_item(Queue, RSSItem) of
        %% Если элемент уже существует в очереди и не требует обновления.
        {same, _} ->
            %% Очередь и подписчики остаются без изменений.
            {Queue, Subscribers};
        %% Если найденный элемент нуждается в обновлении.
        {updated, OldItem} ->
            %% Создание новой очереди без старого элемента и добавление нового.
            %% Все подписчики уведомляются об обновлении.
            UpdatedQueue = [RSSItem | lists:delete(OldItem, Queue)],
            %% Сортировка очереди по времени публикации.
            SortedQueue = sort_queue_by_time(UpdatedQueue),
            %% Оповещение всех подписчиков о новом элементе.
            lists:foreach(fun(Pid) -> add_item(Pid, RSSItem) end, sets:to_list(Subscribers)),
            {SortedQueue, Subscribers};
        %% Если новый элемент уникален для очереди.
        different ->
            %% Добавление нового элемента в очередь и его сортировка.
            UpdatedQueue = [RSSItem | Queue],
            SortedQueue = sort_queue_by_time(UpdatedQueue),
            %% Оповещение подписчиков о добавлении нового элемента.
            lists:foreach(fun(Pid) -> add_item(Pid, RSSItem) end, sets:to_list(Subscribers)),
            {SortedQueue, Subscribers}
    end,
    %% Возвращение обновлённой очереди и списка подписчиков.
    {NewQueue, UpdatedSubscribers}.

%% @doc Сортирует элементы очереди по времени публикации.
%%
%% @spec sort_queue_by_time(list()) -> list().
%% @param Queue Список элементов очереди.
%% @return Возвращает список элементов, отсортированный по времени публикации.
sort_queue_by_time(Queue) ->
    %% Применение функции сортировки к очереди, используя времена публикации элементов.
    lists:sort(fun(Item1, Item2) ->
        %% Извлечение времени публикации для сравнения.
        Time1 = rss_parse:get_item_time(Item1),
        Time2 = rss_parse:get_item_time(Item2),
        %% Сравнение времён для определения порядка элементов.
        Time1 < Time2
    end, Queue).

%% @doc Определяет, существует ли уже в очереди такой же или обновлённый элемент.
%%
%% @spec find_item(list(), RSSItem) -> {action(), RSSItem | none}.
%% @param Queue Очередь, в которой производится поиск.
%% @param RSSItem Элемент, который необходимо найти или обновить в очереди.
%% @return Возвращает кортеж, указывающий на действие (same, updated, different) и элемент,
%% если он был найден.
find_item(Queue, RSSItem) ->
    %% Проход по всем элементам очереди для определения действия по отношению к новому элементу.
    lists:foldl(
        fun(Item, Acc) ->
            %% Сравнение текущего элемента с новым.
            case rss_parse:compare_feed_items(Item, RSSItem) of
                %% Если элементы идентичны, не требуется никаких действий.
                same -> {same, Item};
                %% Если новый элемент является обновлением существующего.
                updated -> {updated, Item};
                %% Если новый элемент уникален для очереди.
                different -> Acc
            end
        end,
        %% Начальное значение - новый элемент уникален.
        different,
        Queue
    ).

%% @doc Тестирование функциональности сервера очереди RSS с подписками.
%%
%% Функция `test/0` запускает комплексное тестирование, включая запуск двух серверов очереди RSS,
%% подписку одного сервера на другой, добавление элементов RSS-ленты в один из серверов и проверку
%% корректности передачи и сортировки элементов в подписанном сервере. Тест использует локальные файлы
%% вместо загрузки данных из интернета, что позволяет выполнять тестирование в изолированной среде.
%%
%% @spec test() -> ok.
test() ->
    %% Открытие файла для записи результатов тестирования.
    {ok, FileHandle} = file:open("testing.txt", [write]),
    
    %% Изменение group leader процесса для перенаправления вывода в файл.
    OldGroupLeader = group_leader(),
    group_leader(FileHandle, self()),

    %% Вывод начала процесса тестирования.
    io:format("Starting RSS Queue Server Test...~n"),

    %% Запуск двух независимых серверов очереди RSS.
    {ok, QueuePid1} = start(),
    {ok, QueuePid2} = start(),
    io:format("RSS Queue Server 1 started with PID ~p.~n", [QueuePid1]),
    io:format("RSS Queue Server 2 started with PID ~p (subscribed to Server 1).~n", [QueuePid2]),

    %% Подписка второго сервера на обновления от первого.
    QueuePid1 ! {subscribe, QueuePid2},
    io:format("RSS Queue Server 2 subscribed to updates from Server 1.~n"),

    %% Чтение локальных файлов RSS-лент и проверка их на соответствие формату RSS 2.0.
    io:format("Reading and verifying RSS feeds...~n"),
    RSS1 = xmerl_scan:file("digg-science-rss1.xml"),
    RSS2 = xmerl_scan:file("digg-science-rss2.xml"),
    io:format("Feed 1 is valid RSS 2.0: ~p~nFeed 2 is valid RSS 2.0: ~p~n", 
              [rss_parse:is_rss2_feed(RSS1), rss_parse:is_rss2_feed(RSS2)]),

    %% Добавление элементов из лент в первый сервер очереди.
    {Feed1, _} = RSS1,
    {Feed2, _} = RSS2,
    io:format("Adding items from RSS feeds to the queue...~n"),
    add_feed(QueuePid1, Feed1),
    add_feed(QueuePid1, Feed2),
    timer:sleep(1000), %% Пауза для синхронизации.
    %% Сохранение и вывод состояния второго сервера после подписки.
    {ok, ItemsAfterSubscribe} = get_all(QueuePid2),
    io:format("Items in Server 2 after subscribe: ~p~n", [ItemsAfterSubscribe]),

    %% Отписка второго сервера от обновлений первого.
    QueuePid1 ! {unsubscribe, QueuePid2},
    io:format("RSS Queue Server 2 unsubscribed from Server 1.~n"),
     %% Добавление элементов из нового файла RSS-ленты в первый сервер после отписки.
    RSS3 = xmerl_scan:file("digg-science-rss3.xml"),
    io:format("Adding new items from RSS feed to the queue after unsubscribe...~n"),
    {Feed3, _} = RSS3,
    add_feed(QueuePid1, Feed3),
    %% Дождаться передачи элементов между серверами и извлечь элементы из второго сервера.
    timer:sleep(1000), %% Пауза, чтобы элементы успели передаться.

    %% Сохранение и вывод состояния второго сервера после отписки.
    {ok, ItemsAfterUnsubscribe} = get_all(QueuePid2),
    io:format("Items in Server 2 after unsubscribe: ~p~n", [ItemsAfterUnsubscribe]),

    %% Сравнение состояний очередей на втором сервере.
    if ItemsAfterSubscribe == ItemsAfterUnsubscribe ->
        io:format("Items in Server 2 are identical before and after unsubscribe. Test Succeeded.~n");
    true ->
        io:format("Items in Server 2 are different before and after unsubscribe. Test Failed.~n")
    end,

    %% Проверка, что элементы в подписанном сервере отсортированы по времени публикации.
    check_items_sorted(ItemsAfterUnsubscribe),

    %% Завершение теста и вывод сообщения об успешном выполнении.
    io:format("RSS Queue Server Test with Subscription Completed Successfully.~n"),
    
    %% Восстановление первоначального вывода для текущего процесса.
    group_leader(OldGroupLeader, self()),

    %% Закрытие файла с результатами тестирования.
    file:close(FileHandle),
    ok.

%% @doc Проверяет, отсортированы ли элементы в списке по времени публикации в возрастающем порядке.
%%
%% Элементы считаются отсортированными, если времена их публикации увеличиваются от одного элемента к следующему.
%% В случае успеха выводится сообщение об успешной проверке, иначе - ошибка.
%%
%% @spec check_items_sorted(list()) -> ok.
%%
%% @param Items список элементов RSS-ленты.
check_items_sorted(Items) ->
    Times = [rss_parse:get_item_time(Item) || Item <- Items, rss_parse:get_item_time(Item) /= bad_date],
    Sorted = lists:sort(Times),
    case Times == Sorted of
        true -> io:format("Items are correctly sorted by publication time.~n");
        false -> io:format("Error: Items are not correctly sorted by publication time.~n")
    end.