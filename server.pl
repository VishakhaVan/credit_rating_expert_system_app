% server.pl

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_files)).
:- use_module(library(http/mimetype)).
:- use_module(expertsystem).

% Define path alias for static files
:- multifile user:file_search_path/2.
user:file_search_path(static, 'static').

% Route Handlers
:- http_handler(root(.), serve_files, [prefix]).
:- http_handler(root(predict), predict_handler, []).
:- http_handler(root(evaluations), all_evaluations_handler, []).
:- http_handler(root(accuracy), overall_accuracy_handler, []).
:- http_handler(root(predict_params), predict_from_params_handler, []).

:- set_prolog_flag(debug, true).


% Serve static files from the 'static' folder
serve_files(Request) :-
    http_reply_from_files(static('.'), [], Request).

% Handle rating prediction
predict_handler(Request) :-
    http_parameters(Request, [name(Name, [])]),
    ( customer(Name, _, _, _, _, _, _, _, _, _) ->
        predict_rating(Name, Rating),
        evaluate_prediction(Name, Accuracy),
        reply_json(json{status: "success", name: Name, rating: Rating, accuracy: Accuracy})
    ;
        reply_json(json{status: "error", message: "Customer not found"})
    ).

all_evaluations_handler(_) :-
    findall(json{name: Name, accuracy: Accuracy}, (
        customer(Name, _, _, _, _, _, _, _, _, _),
        evaluate_prediction(Name, Accuracy)
    ), List),
    reply_json(json{status: "success", evaluations: List}).

overall_accuracy_handler(_) :-
    general_accuracy(Accuracy),
    reply_json(json{status: "success", accuracy: Accuracy}).

predict_from_params_handler(Request) :-
     http_parameters(Request,
        [ income(IncomeAtom, []),
          debts(DebtsAtom, []),
          creditScore(CreditScoreAtom, []),
          amountOwed(AmountOwedAtom, []),
          creditMix(CreditMixAtom, []),
          creditHistory(CreditHistoryAtom, []),
          newCredit(NewCreditAtom, [])
        ]),
    normalize_space(atom(IncomeTrimmed), IncomeAtom),
    normalize_space(atom(DebtsTrimmed), DebtsAtom),
    normalize_space(atom(CreditScoreTrimmed), CreditScoreAtom),
    normalize_space(atom(AmountOwedTrimmed), AmountOwedAtom),
    normalize_space(atom(CreditMixTrimmed), CreditMixAtom),
    normalize_space(atom(CreditHistoryTrimmed), CreditHistoryAtom),
    normalize_space(atom(NewCreditTrimmed), NewCreditAtom),
    atom_number(IncomeTrimmed, Income),
    atom_number(DebtsTrimmed, Debts),
    atom_number(CreditScoreTrimmed, CreditScore),
    atom_number(AmountOwedTrimmed, AmountOwed),
    atom_number(CreditMixTrimmed, CreditMix),
    atom_number(CreditHistoryTrimmed, CreditHistory),
    atom_number(NewCreditTrimmed, NewCredit),

    predict_from_params(Income, Debts, CreditScore, AmountOwed, CreditMix, CreditHistory, NewCredit, Rating),

    reply_json(json{
    status: "success",
    income: Income,
    debts: Debts,
    creditScore: CreditScore,
    amountOwed: AmountOwed,
    creditMix: CreditMix,
    creditHistory: CreditHistory,
    newCredit: NewCredit,
    rating: Rating
}).



% Entry Point
:- initialization(server).

server :-
    http_server(http_dispatch, [port(8080)]).
