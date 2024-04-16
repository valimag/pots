%% @doc Модуль `rss_reader` предназначен для чтения RSS-ленты по HTTP(S) и отправки данных в очередь.
%%
%% Данный модуль содержит функции для запуска процесса чтения RSS-ленты из заданного URL и его дальнейшей обработки.
%% После извлечения и обработки, данные отправляются в указанный процесс очереди.

-module(rss_reader).
-include("logging.hrl"). % Подключение файла с макросами для логирования

%% @spec start(string(), pid()) -> {ok, pid()}.
%% @brief Запускает процесс чтения RSS-ленты.
%%
%% @param Url URL RSS-ленты для чтения.
%% @param QPid PID процесса очереди для отправки извлеченных данных.
-export([start/2, server/2]).
-define(RETRIEVE_INTERVAL, 60000). % Интервал времени в миллисекундах между запросами к RSS-ленте

start(Url, QPid) ->
    ssl:start(), % Запуск подсистемы SSL для поддержки HTTPS-запросов
    inets:start(), % Запуск INETS для использования HTTP-клиента
    Pid = spawn(?MODULE, server, [Url, QPid]), % Создание нового процесса для чтения RSS-ленты
    {ok, Pid}. % Возвращаем PID нового процесса

%% @spec server(string(), pid()) -> no_return().
%% @brief Основная функция сервера для чтения RSS-ленты и отправки данных в очередь.
%%
%% @param Url URL RSS-ленты для чтения.
%% @param QPid PID процесса очереди для отправки извлеченных данных.
server(Url, QPid) ->
    ?INFO("Starting RSS feed retrieval from URL: ~p", [Url]), % Логирование начала процесса чтения ленты
    case httpc:request(Url) of % Выполнение HTTP-запроса к URL
        {ok, {{_, 200, _}, _, Body}} -> % Проверка на успешный HTTP-ответ
            case xmerl_scan:string(Body) of % Парсинг тела ответа как строки XML
                {ok, Feed, _} -> % Успешный парсинг XML
                    case rss_parse:is_rss2_feed(Feed) of % Проверка, является ли результат валидным RSS 2.0
                        true -> 
                            ?INFO("RSS Feed is valid. Sending to queue.~n", []), % Логирование успешной проверки RSS
                            rss_queue:add_feed(QPid, Feed), % Добавление данных ленты в очередь
                            ?INFO("RSS Feed processed successfully. Waiting for next interval.~n", []); % Логирование успешной обработки ленты
                        false -> 
                            ?ERROR("Fetched content is not a valid RSS 2.0 feed.~n", []) % Логирование ошибки формата ленты
                    end;
                {error, Err} ->
                    ?ERROR("Error parsing RSS feed: ~p~n", [Err]) % Логирование ошибки парсинга XML
            end;
        {ok, {{_, StatusCode, _}, _, _}} ->
            ?ERROR("Received HTTP error code: ~p~n", [StatusCode]); % Логирование ошибки HTTP-статуса
        {error, {Reason, _}} ->
            ?ERROR("Failed to retrieve RSS feed: ~p~n", [Reason]) % Логирование ошибки получения ленты
    end,
    receive
        after ?RETRIEVE_INTERVAL -> % Ожидание заданного интервала времени
            server(Url, QPid) % Рекурсивный вызов для следующего цикла чтения
    end.