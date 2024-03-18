% Определение модуля proc_sieve
-module(proc_sieve).

% Экспорт функций для внешнего использования
-export([gen_print/1, generate/1, sieve/1]).

% Функция решета, обрабатывающая сообщения с числами
sieve(State) ->
    % Извлечение текущего состояния: простое число N и PID следующего процесса
    {N, NextPid} = State,
    % Ожидание сообщения
    receive
        % Обработка сообщения с числом
        Value when is_integer(Value) ->
            % Если N не определено, то текущее число становится N для этого процесса
            if
                N == undefined ->
                    % Обновляем состояние с новым простым числом N
                    NewState = {Value, NextPid},
                    % Рекурсивно вызываем функцию с новым состоянием
                    sieve(NewState);
                % Если N  определено
                true ->
                    % Вычисляем остаток от деления текущего числа на N
                    Remainder = Value rem N,
                    % Если число делится на N, игнорируем его
                    if
                        Remainder == 0 ->
                            sieve(State);
                        true ->
                            % Если следующий процесс не определен и число не делится на N
                            if
                                NextPid == undefined ->
                                    % Создаем новый процесс решета
                                    InitialState = {undefined, undefined},
                                    NewNextPid = spawn(proc_sieve, sieve, [InitialState]),
                                    % Отправляем число новому процессу
                                    NewNextPid ! Value,
                                    % Обновляем состояние с PID нового процесса
                                    NewState = {N, NewNextPid},
                                    sieve(NewState);
                                true ->
                                    % Пересылаем число следующему процессу в цепочке
                                    NextPid ! Value,
                                    sieve(State)
                            end
                    end
            end;
        % Обработка сообщения о завершении
        {done, RepId} ->
            % Если следующий процесс не существует, это последний процесс в цепочке
            if
                NextPid == undefined ->
                    % Отправляем список с одним простым числом N обратно
                    RepId ! [N],
                    % Завершаем конечный процесс
                    exit(normal);
                true -> 
                    % Если это не последний процесс, передаем сообщение о завершении следующему
                    NextPid ! {done, self()},
                    % Ожидаем список простых чисел от следующего процесса
                    receive
                        Value when is_list(Value) ->
                            % Добавляем наше простое число N к списку и отправляем обратно
                            RepId ! [N|Value],
                            % Завершаем текущий процесс со статусом normal
                            exit(normal)
                    end
            end
    end.

% Генерация списка простых чисел до значения MaxN
generate(MaxN) ->
    % Инициализация состояния для базового процесса решета
    % Атом undefined - для обозначения отсутствующего значения
    InitialState = {undefined, undefined},
    % Создание базового процесса решета
    % spawn() запускает новый процесс, который начинает исполнение указанной функции proc_sieve:sieve(InitialState)
    % BasePid - Базовый идентификатор процесса (сохраняем его)
    BasePid = spawn(proc_sieve, sieve, [InitialState]),
    % Посылаем числа от 2 до MaxN базовому процессу в качестве сообщения с помощью "!"
    % lists:seq(2, MaxN) — создает список чисел от 2 до MaxN
    lists:foreach(fun(X) -> BasePid ! X end, lists:seq(2, MaxN)),
    % Отправляем сообщение о завершении базовому процессу
    % Финальное сообщение
    % done - атом, который указывает на то, что обработка всех чисел завершена
    % self() - функция, которая возвращает PID текущего процесса.
    BasePid ! {done, self()},
    % Ожидаем список простых чисел от базового процесса
    receive
        % получаем и возвращаем этот список в качестве результата функции generate
        Value -> Value
    end.

% Функция для вывода списка простых чисел до MaxN
gen_print(MaxN) -> 
    % Генерация списка простых чисел
    Primes = generate(MaxN),
    % Вывод списка простых чисел
    io:format("Prime numbers up to ~w: ~w~n", [MaxN, Primes]).