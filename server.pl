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

% Serve static files from the 'static' folder
serve_files(Request) :-
    http_reply_from_files(static('.'), [], Request).

% Handle rating prediction
predict_handler(Request) :-
    http_parameters(Request, [name(Name, [])]),
    ( customer(Name, _, _, _, _) ->
        predict_rating(Name, Rating),
        evaluate_prediction(Name, Accuracy),
        reply_json(json{status: "success", name: Name, rating: Rating, accuracy: Accuracy})
    ;
        reply_json(json{status: "error", message: "Customer not found"})
    ).

% Entry Point
:- initialization(server).

server :-
    http_server(http_dispatch, [port(8080)]).
