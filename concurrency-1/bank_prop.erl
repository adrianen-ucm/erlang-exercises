-module(bank_prop).

-export([
    prop_bank/0,
    initial_state/0,
    weight/2,
    create_bank_pre/1,
    create_bank_args/1,
    create_bank/0,
    create_bank_next/3,
    new_account_pre/1,
    new_account_args/1,
    new_account/2,
    new_account_post/3,
    new_account_next/3,
    balance_pre/1,
    balance_args/1,
    balance/2,
    balance_post/3,
    deposit_money_pre/1,
    deposit_money_args/1,
    deposit_money/3,
    deposit_money_post/3,
    deposit_money_next/3,
    withdraw_money_pre/1,
    withdraw_money_args/1,
    withdraw_money/3,
    withdraw_money_post/3,
    withdraw_money_next/3,
    transfer_pre/1,
    transfer_args/1,
    transfer/4,
    transfer_post/3,
    transfer_next/3
]).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

-record(state, {bank = none, accounts = []}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% I have adapted the tests for my bank exercise implementation, which  %
%%% has a slightly different API:                                        %
%%%                                                                      %
%%%   - Raise exceptions instead of returning ko.                        %
%%%   - The return values are as specified in the exercise statement.    %
%%%                                                                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prop_bank() ->
    ?FORALL(
        Cmds,
        eqc_statem:commands(bank_prop),
        begin
            {H, S, Result} = eqc_statem:run_commands(bank_eqc, Cmds),
            pretty_commands(bank_eqc, Cmds, {H, S, Result}, Result == ok)
        end
    ).

initial_state() -> #state{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Make it more likely that the balance command is generated, and less  %
%%% likely that the new_account command is generated.                    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

weight(_, balance) -> 3;
weight(_, new_account) -> 1;
weight(_, _) -> 2.

create_bank_pre(State) ->
    State#state.bank == none.

create_bank_args(_) ->
    [].

create_bank() ->
    bank:create_bank().

create_bank_next(State, Result, []) ->
    State#state{bank = Result}.

new_account_pre(State) ->
    State#state.bank =/= none.

new_account_args(State) ->
    [State#state.bank, account_id()].

new_account(Bank, AccountId) ->
    bank:new_account(Bank, AccountId).

new_account_post(State, [_, AccountId], Result) ->
    Result == (not account_exists(AccountId, State#state.accounts)).

new_account_next(State, _, [_, AccountId]) ->
    case account_exists(AccountId, State#state.accounts) of
        true ->
            State;
        false ->
            State#state{
                accounts = [{AccountId, 0} | State#state.accounts]
            }
    end.

balance_pre(State) ->
    (State#state.bank =/= none) and (State#state.accounts =/= []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% With a high probability let balance be called with an existing       %
%%% account, and with a small probability a random account.              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

balance_args(State) ->
    [
        State#state.bank,
        frequency([
            {0.1, account_id()},
            {0.9, existing_account_id(State)}
        ])
    ].

balance(Bank, AccountId) ->
    catch bank:balance(Bank, AccountId).

balance_post(State, [_, AccountId], Result) ->
    case account_exists(AccountId, State#state.accounts) of
        false ->
            element(1, Result) == 'EXIT';
        true ->
            Result == account_get_balance(AccountId, State#state.accounts)
    end.

deposit_money_pre(State) ->
    (State#state.bank =/= none) and (State#state.accounts =/= []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Let deposit_money only be called with an account that exists         %
%%% (not a random account).                                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

deposit_money_args(State) ->
    [State#state.bank, existing_account_id(State), nat()].

deposit_money(Bank, AccountId, Amount) ->
    catch bank:deposit_money(Bank, AccountId, Amount).

deposit_money_post(State, [_, AccountId, Amount], Result) ->
    case account_exists(AccountId, State#state.accounts) of
        false ->
            element(1, Result) == 'EXIT';
        true ->
            Result == Amount + account_get_balance(AccountId, State#state.accounts)
    end.

deposit_money_next(State, _, [_, AccountId, Amount]) ->
    case account_exists(AccountId, State#state.accounts) of
        false ->
            State;
        true ->
            State#state{
                accounts = account_update_balance(
                    AccountId,
                    fun(Current) -> Current + Amount end,
                    State#state.accounts
                )
            }
    end.

withdraw_money_pre(State) ->
    State#state.bank =/= none.

withdraw_money_args(State) ->
    [State#state.bank, account_id(), nat()].

withdraw_money(Bank, AccountId, Amount) ->
    catch bank:withdraw_money(Bank, AccountId, Amount).

withdraw_money_post(State, [_, AccountId, Amount], Result) ->
    case account_get_balance(AccountId, State#state.accounts) of
        false ->
            element(1, Result) == 'EXIT';
        Balance when Amount =< Balance ->
            Result == Amount;
        _ ->
            element(1, Result) == 'EXIT'
    end.

withdraw_money_next(State, Result, [_, AccountId, Amount]) ->
    case Result == Amount of
        false ->
            State;
        true ->
            State#state{
                accounts = account_update_balance(
                    AccountId,
                    fun(Current) -> Current - Amount end,
                    State#state.accounts
                )
            }
    end.

transfer_pre(State) ->
    State#state.bank =/= none.

transfer_args(State) ->
    [State#state.bank, account_id(), account_id(), nat()].

transfer(Bank, From, To, Amount) ->
    catch bank:transfer(Bank, From, To, Amount).

transfer_post(State, [_, From, To, Amount], Result) ->
    case
        {
            account_get_balance(From, State#state.accounts),
            account_get_balance(To, State#state.accounts)
        }
    of
        {false, _} ->
            element(1, Result) == 'EXIT';
        {_, false} ->
            element(1, Result) == 'EXIT';
        {Balance, _} when Amount =< Balance ->
            Result == Amount;
        _ ->
            element(1, Result) == 'EXIT'
    end.

transfer_next(State, Result, [_, From, To, Amount]) ->
    case Result == Amount of
        false ->
            State;
        true ->
            State#state{
                accounts = account_update_balance(
                    From,
                    fun(Current) -> Current - Amount end,
                    account_update_balance(
                        To,
                        fun(Current) -> Current + Amount end,
                        State#state.accounts
                    )
                )
            }
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Helper generators and state management functions.                    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

account_id() ->
    oneof([a, b, c]).

existing_account_id(State) ->
    ?SUCHTHAT(A, account_id(), account_exists(A, State#state.accounts)).

account_exists(AccountId, Accounts) ->
    account_get_balance(AccountId, Accounts) =/= false.

account_get_balance(AccountId, Accounts) ->
    case lists:keyfind(AccountId, 1, Accounts) of
        false -> false;
        Result -> element(2, Result)
    end.

account_update_balance(AccountId, F, Accounts) ->
    lists:map(
        fun
            ({Id, Amount}) when Id == AccountId -> {Id, F(Amount)};
            (Other) -> Other
        end,
        Accounts
    ).
