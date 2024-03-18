% Определяем модуль с именем mobius
-module(mobius).

% Экспортируем функции для внешнего использования
-export([print_result/0, is_prime/1, prime_factors/1,
         is_square_multiple/1, find_square_multiples/2]).

% Проверка числа N на простоту, начиная проверку с числа M (делитель)
% Если M^2 больше N, то число простое (нужно проверять делители до sqrt(N))
is_prime(N, M) when M*M > N -> true;
is_prime(N, M) -> 
  % Вычисляем остаток от деления N на M
  Remainder = N rem M,
  if
    % Если остаток равен 0, число не простое
    Remainder == 0 -> false;
    % Иначе проверяем следующий делитель
    true -> is_prime(N, M + 1)
  end.

% 1 не является простым числом (частный случай)
is_prime(1) -> false;

% Цель: Определяет, является ли заданное число простым.
% Входные данные: Целое число N.
% Выходные данные: true, если число простое; false в противном случае.
% Общая функция для проверки простоты числа, начинаем с делителя 2
is_prime(N) when N > 1 -> is_prime(N, 2).

% Если M^2 больше N, добавляем N к списку сомножителей
prime_factors(N, Rest, M) when M*M > N -> [N|Rest];

prime_factors(N, Rest, M) -> 
  % Проверяем, является ли M простым числом
  ChkPrime = is_prime(M),
  if
    % Если M простое и делит N без остатка, добавляем M к списку сомножителей и продолжаем с N/M
    ChkPrime == true, (N rem M) == 0 -> prime_factors(N div M, [M|Rest], M);
    % После проверки делителя 2 переходим к 3 (так как потом увеличиваем на 2)
    M == 2 -> prime_factors(N, Rest, 3);
    % Для всех других случаев увеличиваем делитель на 2 (нет смысла увеличивать на 1)
    true -> prime_factors(N, Rest, M + 2)
  end.

% Цель: Возвращает список простых сомножителей числа.
% Входные данные: Целое число N.
% Выходные данные: Список простых сомножителей числа N.
% Получение списка простых сомножителей числа N
prime_factors(N) -> lists:reverse(prime_factors(N, [], 2)).
% Возвращает список простых сомножителей в обратном порядке

% Цель: Определяет, делится ли число на квадрат простого числа.
% Входные данные: Целое число N.
% Выходные данные: true, если N делится на квадрат простого числа; иначе false.
% Проверка, делится ли число N на квадрат простого числа
is_square_multiple(N) -> 
  % Получаем список простых сомножителей N
  List = prime_factors(N),
  % Сравниваем размеры списка и множества сомножителей
  ListSize = erlang:length(List), 
  SetSize = sets:size(sets:from_list(List)),
  % Если размеры не равны, значит есть повторяющиеся сомножители
  ListSize /= SetSize.

% Поиск чисел, делящихся на квадрат простого числа
find_square_multiples(_N, _MaxN, List, Len, Count) when Len == Count -> 
  NewList = lists:reverse(List), 
  % Если найдена нужная последовательность, возвращаем первое число
  lists:nth(1, NewList);

% Если достигнут MaxN и не найдена последовательность, возвращаем fail
find_square_multiples(N, MaxN, _List, _Len, _Count) when N > MaxN -> fail;

find_square_multiples(N, MaxN, List, Len, Count) -> 
  % Проверяем, делится ли N на квадрат простого числа
  ChkMultiple = is_square_multiple(N),
  if
    % Если да, добавляем N к списку и увеличиваем счетчик
    ChkMultiple == true -> find_square_multiples(N + 1, MaxN, [N|List], Len + 1, Count);
    % Если нет, начинаем поиск заново с следующего числа
    true -> find_square_multiples(N + 1, MaxN, [], 0, Count)
  end.

% Цель: Находит первое число в последовательности заданной длины, где каждое число делится на квадрат простого числа.
% Входные данные: Длина последовательности Count и максимальное значение для поиска MaxN.
% Выходные данные: Первое число в последовательности или fail, если таковая не найдена.
find_square_multiples(Count, MaxN) -> find_square_multiples(2, MaxN, [], 0, Count).
% Инициализирует поиск с начальными параметрами

% Находит все последовательности чисел заданной длины, делящихся на квадрат простого числа, в диапазоне до MaxN
find_all_square_multiples(Count, MaxN) -> find_all_square_multiples(2, MaxN, [], 0, Count, []).

% Вспомогательная функция для поиска с аккумуляторами для текущего числа (N), максимального числа (MaxN), текущей последовательности (List),
% длины текущей последовательности (Len), целевой длины последовательности (Count) и найденных последовательностей (FoundSequences)
find_all_square_multiples(_N, _MaxN, _List, _Len, _Count, FoundSequences) when _N > _MaxN -> 
    % Возвращает список найденных последовательностей, когда N превысит MaxN
    FoundSequences;
find_all_square_multiples(N, MaxN, List, Len, Count, FoundSequences) -> 
    % Проверяет, делится ли текущее число N на квадрат простого числа
    ChkMultiple = is_square_multiple(N),
    if
        % Если текущее число подходит и достигнута целевая длина последовательности, добавляем её в список найденных
        ChkMultiple == true, Len + 1 == Count ->
            find_all_square_multiples(N + 1, MaxN, [], 0, Count, [lists:reverse([N|List])|FoundSequences]);
         % Если текущее число подходит, но целевая длина ещё не достигнута, продолжаем строить последовательность
        ChkMultiple == true ->
            find_all_square_multiples(N + 1, MaxN, [N|List], Len+1, Count, FoundSequences);
        % Если текущее число не подходит, начинаем поиск новой последовательности
        true ->
            find_all_square_multiples(N + 1, MaxN, [], 0, Count, FoundSequences)
    end.

% Цель: Выводит результаты поиска последовательностей чисел, делящихся на квадрат простого числа, заданной длины.
% Вывод результатов поиска последовательностей и замер времени выполнения
print_result() -> 
  Start = os:timestamp(),
  % Записываем начальное время
  io:format("Sequence length 4: ~w~n", [find_square_multiples(4, 30000)]),
  io:format("Sequence length 4 ALL: ~w~n", [lists:reverse(find_all_square_multiples(4, 30000))]),
  io:format("Sequence length 5: ~w~n", [find_square_multiples(5, 30000)]),
  io:format("Sequence length 5 ALL: ~w~n", [lists:reverse(find_all_square_multiples(5, 30000))]),
  io:format("Sequence length 6: ~w~n", [find_square_multiples(6, 30000)]),
  io:format("Sequence length 6 ALL: ~w~n", [lists:reverse(find_all_square_multiples(6, 30000))]),
  % Выводим результаты поиска для последовательностей длиной 4, 5, 6
  io:format("Total time: ~f seconds~n", [timer:now_diff(os:timestamp(), Start) / 1000000]).
  % Выводим общее время выполнения