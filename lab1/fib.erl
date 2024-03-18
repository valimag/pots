% Определение модуля с именем fib
-module(fib).
% Экспорт функций, делая их доступными для вызова извне
-export([fib_p/1, fib_g/1, tail_fib/1, print_results/0]).

% Реализация функции Фибоначчи без использования хвостовой рекурсии (паттерн матчинг)
fib_p(0) -> 0; % Базовый случай: fib(0) = 0
fib_p(1) -> 1; % Базовый случай: fib(1) = 1
fib_p(N) -> fib_p(N - 1) + fib_p(N - 2). % Рекурсивный случай

% Реализация функции Фибоначчи без использования хвостовой рекурсии (сторожевые выражения)
fib_g(0) -> 0; % Базовый случай для 0
fib_g(1) -> 1; % Базовый случай для 1
fib_g(N) when N > 1 -> fib_g(N - 1) + fib_g(N - 2). % Рекурсивный случай

% Реализация функции Фибоначчи с использованием хвостовой рекурсии
tail_fib(N) -> tail_fib(N, 0, 1). % Начальный вызов с аккумуляторами
tail_fib(0, A, _) -> A; % Возвращает накопленное значение для N = 0
tail_fib(N, A, B) when N > 0 -> tail_fib(N - 1, B, A + B). % Рекурсивный вызов с обновлением аккумуляторов

% Функция для вывода результатов вычислений и времени выполнения в читаемом формате
print_results() ->
    % Проходит через последовательность значений N от 2 до 40
    print_fib_results(2, 40),
    print_tail_result(10000).

% Выводит результаты и время выполнения для каждого N в указанном диапазоне
print_fib_results(Start, Limit) when Start =< Limit ->
    % Измеряет время выполнения и получает результаты для fib_p
    {PTime, PRes} = timer:tc(fib, fib_p, [Start]),
    % Измеряет время выполнения и получает результаты для fib_g
    {GTime, GRes} = timer:tc(fib, fib_g, [Start]),
    % Измеряет время выполнения и получает результаты для tail_fib
    {TailTime, TailRes} = timer:tc(fib, tail_fib, [Start]),
    % Выводит результаты и время выполнения в удобочитаемом формате
    io:format("N = ~w~n", [Start]),
    io:format("p_res = ~w~n", [PRes]),
    io:format("g_res = ~w~n", [GRes]),
    io:format("tail_res = ~w~n", [TailRes]),
    io:format("p_time = ~w~n", [PTime]),
    io:format("g_time = ~w~n", [GTime]),
    io:format("tail_time = ~w~n~n", [TailTime]),
    % Вычисляет следующее значение N и продолжает вывод
    Next = next_N_value(Start),
    print_fib_results(Next, Limit);
print_fib_results(_, _) -> ok.

print_tail_result(N) -> 
    % Измеряет время выполнения и получает результаты для tail_fib
    {TailTime, TailRes} = timer:tc(fib, tail_fib, [N]),
    % Выводит результаты и время выполнения в удобочитаемом формате
    io:format("N = ~w~n", [N]),
    io:format("tail_res = ~w~n", [TailRes]),
    io:format("tail_time = ~w~n~n", [TailTime]).

% Вычисляет следующее значение N
next_N_value(N) -> N + 2. % Увеличивает N на 2