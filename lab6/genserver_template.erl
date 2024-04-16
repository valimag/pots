%% @doc Шаблон для реализации модулей обратного вызова поведения gen_server.
%%
%% Этот модуль предоставляет базовый шаблон для создания серверов, основанных на gen_server.
-module(genserver_template).

%% @doc Сервер реализует поведение gen_server.
-behaviour(gen_server).

%% @doc Экспорт функций обратного вызова, требуемых поведением gen_server.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% @doc Экспорт дополнительных вспомогательных функций, предоставляемых модулем обратного вызова.
-export([start/1]).

%% @doc Инициализирует gen_server с указанным именем.
%% @spec start(Name :: atom()) -> {ok, Pid} | ignore | {error, Error}
%% Name - атом, представляющий имя сервера.
start(Name) -> gen_server:start({local, Name}, ?MODULE, [], []).

%% @doc Функция инициализации, вызываемая при запуске gen_server.
%% @spec init(Args :: list()) -> {ok, State} | {ok, State, Timeout} | {stop, Reason}
%% Возвращает начальное состояние сервера.
init([]) -> {ok, todo_state}.

%% @doc Обрабатывает синхронные вызовы к серверу.
%% @spec handle_call(Request, From, State) -> {reply, Reply, NewState} | {reply, Reply, NewState, Timeout} | {noreply, NewState} | {noreply, NewState, Timeout} | {stop, Reason, Reply, NewState} | {stop, Reason, NewState}
%% Возвращает todo_reply и текущее состояние как заглушку.
handle_call(_Request, _From, State) -> {reply, todo_reply, State}.

%% @doc Обрабатывает асинхронные сообщения к серверу.
%% @spec handle_cast(Msg, State) -> {noreply, NewState} | {noreply, NewState, Timeout} | {stop, Reason, NewState}
%% Просто возвращает текущее состояние, не делая никаких изменений.
handle_cast(_Msg, State) -> {noreply, State}.

%% @doc Обрабатывает все другие сообщения, не связанные с вызовами или сообщениями cast.
%% @spec handle_info(Info, State) -> {noreply, NewState} | {noreply, NewState, Timeout} | {stop, Reason, NewState}
%% Аналогично handle_cast, просто возвращает текущее состояние.
handle_info(_Info, State) -> {noreply, State}.

%% @doc Вызывается при завершении сервера.
%% @spec terminate(Reason, State) -> ok
%% Ничего не делает при завершении.
terminate(_Reason, _State) -> ok.

%% @doc Вызывается при изменении кода сервера.
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Просто возвращает текущее состояние.
code_change(_OldVsn, State, _Extra) -> {ok, State}.



