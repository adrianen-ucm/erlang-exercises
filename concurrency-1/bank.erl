-module(bank).

-export([
    create_bank/0,
    new_account/2,
    withdraw_money/3,
    deposit_money/3,
    transfer/4,
    balance/2
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 5. Implement in Erlang a simple bank application with the following  %
%%% API:                                                                 %
%%%                                                                      %
%%%   create_bank(): Creates a bank and returns a "bank" handle.         %
%%%                                                                      %
%%%   new_account(Bank, AccountNumber): Creates a new account with       %
%%%   account balance 0. Returns true if the account could be created    %
%%%   (it was new) and false otherwise.                                  %
%%%                                                                      %
%%%   withdraw_money(Bank, AccountNumber, Quantity): Withdraws money     %
%%%   from the account (if Quantity =< account balance) Returns amount   %
%%%   of money withdrawn.                                                %
%%%                                                                      %
%%%   deposit_money(Bank, AccountNumber, Quantity): Increases balance of %
%%%   account by Quantity, returning new balance.                        %
%%%                                                                      %
%%%   transfer(Bank, FromAccount, ToAccount, Quantity): Transfers        %
%%%   Quantity from one account (if the balance is sufficient) to        %
%%%   another account. Returns amount of money transferred.              %
%%%                                                                      %
%%%   balance(Bank,Account): Returns the current balance for the         %
%%%   account.                                                           %
%%%                                                                      %
%%% Implement the bank internally as a process, which listens to sent    %
%%% messages and responds to them.                                       %
%%%                                                                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public bank API. It acts as a wrapper around the bank process        %
%%% communication.                                                       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_bank() ->
    spawn(fun() -> bank_process(#{}) end).

new_account(Bank, AccountNumber) ->
    request(Bank, {new_account, AccountNumber}).

withdraw_money(Bank, AccountNumber, Quantity) ->
    request(Bank, {withdraw_money, AccountNumber, Quantity}).

deposit_money(Bank, AccountNumber, Quantity) ->
    request(Bank, {deposit_money, AccountNumber, Quantity}).

transfer(Bank, FromAccount, ToAccount, Quantity) ->
    request(Bank, {transfer, FromAccount, ToAccount, Quantity}).

balance(Bank, AccountNumber) ->
    request(Bank, {balance, AccountNumber}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Bank process and a couple of communication helper functions.         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bank_process(Accounts) ->
    receive
        {Pid, {new_account, AccountNumber}} when is_pid(Pid) ->
            response(Pid, model_new_account(Accounts, AccountNumber));
        {Pid, {withdraw_money, AccountNumber, Quantity}} when is_pid(Pid) ->
            response(Pid, model_withdraw_money(Accounts, AccountNumber, Quantity));
        {Pid, {deposit_money, AccountNumber, Quantity}} when is_pid(Pid) ->
            response(Pid, model_deposit_money(Accounts, AccountNumber, Quantity));
        {Pid, {transfer, FromAccount, ToAccount, Quantity}} when is_pid(Pid) ->
            response(Pid, model_transfer(Accounts, FromAccount, ToAccount, Quantity));
        {Pid, {balance, AccountNumber}} when is_pid(Pid) ->
            response(Pid, model_balance(Accounts, AccountNumber));
        Other ->
            io:format("Unknown message received: ~p~n", [Other]),
            bank_process(Accounts)
    end.

response(Pid, {Response, Accounts}) ->
    Pid ! Response,
    bank_process(Accounts).

% A dedicated process to handle the single request/response
% communication between the bank and the caller ones.
%
% When the bank response is an error, the caller process raises
% an exception.
request(Bank, Msg) ->
    ClientPid = self(),
    WorkerPid =
        spawn(fun() ->
            WorkerPid = self(),
            Bank ! {WorkerPid, Msg},
            receive
                X -> ClientPid ! {WorkerPid, X}
            end
        end),
    receive
        {Pid, {error, Reason}} when Pid == WorkerPid ->
            exit(Reason);
        {Pid, X} when Pid == WorkerPid ->
            X
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pure model manipulation and result. These functions return a tuple   %
%%% {Response, UpdatedModel} where the Response can be {error, Message}  %
%%% if something bad happened.                                           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
model_new_account(Accounts, AccountNumber) when
    is_map_key(AccountNumber, Accounts)
->
    {false, Accounts};
model_new_account(Accounts, AccountNumber) ->
    {true, maps:put(AccountNumber, 0, Accounts)}.

model_withdraw_money(Accounts, AccountNumber, _) when
    not is_map_key(AccountNumber, Accounts)
->
    {{error, "Account not found"}, Accounts};
model_withdraw_money(Accounts, _, Quantity) when
    not is_integer(Quantity) orelse Quantity < 0
->
    {{error, "Not valid quantity to withdraw"}, Accounts};
model_withdraw_money(Accounts, AccountNumber, Quantity) ->
    case maps:get(AccountNumber, Accounts) - Quantity of
        NewBalance when NewBalance >= 0 ->
            {Quantity, maps:put(AccountNumber, NewBalance, Accounts)};
        _ ->
            {{error, "Insufficient account balance to withdraw"}, Accounts}
    end.

model_deposit_money(Accounts, AccountNumber, _) when
    not is_map_key(AccountNumber, Accounts)
->
    {{error, "Account not found"}, Accounts};
model_deposit_money(Accounts, _, Quantity) when
    not is_integer(Quantity) orelse Quantity < 0
->
    {{error, "Not valid quantity to deposit"}, Accounts};
model_deposit_money(Accounts, AccountNumber, Quantity) ->
    NewBalance = maps:get(AccountNumber, Accounts) + Quantity,
    {NewBalance, maps:put(AccountNumber, NewBalance, Accounts)}.

model_transfer(Accounts, FromAccount, ToAccount, _) when
    not is_map_key(FromAccount, Accounts) orelse not is_map_key(ToAccount, Accounts)
->
    {{error, "Account not found"}, Accounts};
model_transfer(Accounts, _, _, Quantity) when
    not is_integer(Quantity) orelse Quantity < 0
->
    {{error, "Not valid quantity to transfer"}, Accounts};
model_transfer(Accounts, FromAccount, ToAccount, Quantity) ->
    case maps:get(FromAccount, Accounts) - Quantity of
        NewBalance when NewBalance >= 0 ->
            {Quantity,
                maps:update_with(
                    ToAccount,
                    fun(Balance) -> Balance + Quantity end,
                    maps:put(FromAccount, NewBalance, Accounts)
                )};
        _ ->
            {{error, "Insufficient account balance to transfer"}, Accounts}
    end.

model_balance(Accounts, AccountNumber) when
    not is_map_key(AccountNumber, Accounts)
->
    {{error, "Account not found"}, Accounts};
model_balance(Accounts, AccountNumber) ->
    {maps:get(AccountNumber, Accounts), Accounts}.
