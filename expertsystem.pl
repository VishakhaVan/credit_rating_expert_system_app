% expertsystem.pl

:- module(expertsystem, [
    predict_rating/2,
    evaluate_prediction/2,
    customer/5
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
