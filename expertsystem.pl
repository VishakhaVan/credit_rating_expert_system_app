% expertsystem.pl

:- module(expertsystem, [
    predict_rating/2,
    evaluate_prediction/2,
    customer/5,
    show_all_evaluations/0,
    general_accuracy/1,
    expert_system_loop/0,
    predict_from_params/3
]).

% ------------ KNOWLEDGE BASE ---------------
customer(john, 30, 50000, 2, good).
customer(jane, 25, 30000, 1, fair).
customer(bob, 40, 80000, 3, excellent).
customer(alice, 35, 60000, 2, good).
customer(mike, 22, 25000, 0, poor).
customer(susan, 50, 90000, 4, excellent).
customer(david, 28, 40000, 1, fair).
customer(emily, 45, 70000, 3, good).
customer(peter, 20, 20000, 0, poor).
customer(olivia, 32, 55000, 2, fair).

% ------------ INFERENCE RULES ---------------
predict_rating(Name, Rating) :-
    customer(Name, _, Income, Debts, _),
    (
        (Income >= 75000, Debts >= 3) -> Rating = excellent;
        (Income >= 50000, Income < 75000, Debts >= 2) -> Rating = good;
        (Income >= 30000, Income < 50000, Debts >= 1) -> Rating = fair;
        (Income < 30000, Debts =< 1) -> Rating = poor;
        (Income >= 75000, Debts < 3) -> Rating = excellent;
        (Income >= 50000, Income < 75000, Debts < 2) -> Rating = good;
        (Income >= 30000, Income < 50000, Debts < 1) -> Rating = fair;
        (Income < 30000, Debts > 1) -> Rating = poor;
        Rating = unknown
    ),
    !.

evaluate_prediction(Name, Accuracy) :-
    customer(Name, _, _, _, ActualRating),
    predict_rating(Name, PredictedRating),
    (ActualRating = PredictedRating -> Accuracy = correct ; Accuracy = incorrect).

% ------------ SHOW ALL EVALUATIONS ---------------
show_all_evaluations :-
    customer(Name, _, _, _, _),
    evaluate_prediction(Name, Accuracy),
    format('~w: ~w~n', [Name, Accuracy]),
    fail.
show_all_evaluations.  % end after all fail

% ------------ CALCULATE GENERAL ACCURACY ---------------
general_accuracy(Accuracy) :-
    findall(Name, customer(Name, _, _, _, _), Names),
    include(is_correct, Names, CorrectPredictions),
    length(Names, Total),
    length(CorrectPredictions, Correct),
    Accuracy is (Correct / Total) * 100.

is_correct(Name) :-
    evaluate_prediction(Name, correct).

% Predict based on parameters directly
predict_from_params(Income, Debts, Rating) :-
    (
        (Income >= 75000, Debts >= 3) -> Rating = excellent;
        (Income >= 50000, Income < 75000, Debts >= 2) -> Rating = good;
        (Income >= 30000, Income < 50000, Debts >= 1) -> Rating = fair;
        (Income < 30000, Debts =< 1) -> Rating = poor;
        (Income >= 75000, Debts < 3) -> Rating = excellent;
        (Income >= 50000, Income < 75000, Debts < 2) -> Rating = good;
        (Income >= 30000, Income < 50000, Debts < 1) -> Rating = fair;
        (Income < 30000, Debts > 1) -> Rating = poor;
        Rating = unknown
    ).


% ------------ INTERACTIVE LOOP ---------------
expert_system_loop :-
    write('--- Credit Rating Expert System ---'), nl,
    write('1. Predict rating by name'), nl,
    write('2. Show evaluation accuracy of all customers'), nl,
    write('3. Show overall accuracy'), nl,
    write('4. Exit'), nl,
    write('Enter your choice: '),
    read(Choice),
    handle_choice(Choice).

handle_choice(1) :-
    write('Enter customer name: '),
    read(Name),
    (customer(Name, _, _, _, _) ->
        predict_rating(Name, Rating),
        format('Predicted rating for ~w: ~w~n', [Name, Rating])
    ;
        write('Customer not found.'), nl),
    expert_system_loop.

handle_choice(2) :-
    show_all_evaluations,
    expert_system_loop.

handle_choice(3) :-
    general_accuracy(Acc),
    format('Overall accuracy: ~2f%%~n', [Acc]),
    expert_system_loop.

handle_choice(4) :-
    write('Exiting expert system. Goodbye!'), nl.

handle_choice(_) :-
    write('Invalid choice. Try again.'), nl,
    expert_system_loop.