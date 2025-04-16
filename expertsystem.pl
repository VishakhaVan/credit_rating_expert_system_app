% expertsystem.pl

:- module(expertsystem, [
    predict_rating/2,
    evaluate_prediction/2,
    customer/10,
    show_all_evaluations/0,
    general_accuracy/1,
    expert_system_loop/0,
    predict_from_params/8
]).

% ------------ MODIFIED KNOWLEDGE BASE ---------------

% customer(Name, Age, Income, Debts, PaymentHistoryScore, AmountOwed, CreditMixScore, CreditLengthYears, NewCreditInquiries, ExpectedRating).

customer(john, 30, 50000, 2, 3, 15000, 1, 6, 1, good).
customer(jane, 25, 30000, 1, 2, 10000, 1, 3, 1, fair).
customer(bob, 40, 80000, 3, 5, 9000, 2, 15, 0, excellent).
customer(alice, 35, 60000, 2, 4, 12000, 1, 8, 1, good).
customer(mike, 22, 25000, 0, 1, 5000, 0, 2, 2, poor).
customer(susan, 50, 90000, 4, 5, 8000, 2, 20, 0, excellent).
customer(david, 28, 40000, 1, 3, 17000, 1, 4, 1, fair).
customer(emily, 45, 70000, 3, 4, 25000, 2, 12, 1, good).
customer(peter, 20, 20000, 0, 1, 4000, 0, 1, 2, poor).
customer(olivia, 32, 55000, 2, 3, 16000, 1, 5, 1, fair).

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

% Define factor evaluation rules

good_income(Income) :- Income >= 75000.
average_income(Income) :- Income >= 50000, Income < 75000.
low_income(Income) :- Income < 50000.

high_debt(Debts) :- Debts >= 3.
moderate_debt(Debts) :- Debts >= 1, Debts < 3.
low_debt(Debts) :- Debts < 1.

excellent_payment_history(Score) :- Score >= 4.
good_payment_history(Score) :- Score >= 2, Score < 4.
poor_payment_history(Score) :- Score < 2.

low_amount_owed(Amount) :- Amount < 10000.
moderate_amount_owed(Amount) :- Amount >= 10000, Amount < 30000.
high_amount_owed(Amount) :- Amount >= 30000.

diverse_credit_mix(MixScore) :- MixScore >= 2.
some_credit_mix(MixScore) :- MixScore =:= 1.
poor_credit_mix(MixScore) :- MixScore =:= 0.

long_credit_history(Years) :- Years >= 10.
medium_credit_history(Years) :- Years >= 5, Years < 10.
short_credit_history(Years) :- Years < 5.

no_new_credit(Inquiries) :- Inquiries =:= 0.
some_new_credit(Inquiries) :- Inquiries =:= 1.
lots_of_new_credit(Inquiries) :- Inquiries > 1.

% Inference rule for predicting rating
predict_from_params(Income, Debts, PaymentHistory, AmountOwed, CreditMix, CreditLength, NewCredit, Rating) :-
    good_income(Income),
    low_debt(Debts),
    excellent_payment_history(PaymentHistory),
    low_amount_owed(AmountOwed),
    diverse_credit_mix(CreditMix),
    long_credit_history(CreditLength),
    no_new_credit(NewCredit),
    Rating = excellent.

predict_from_params(Income, Debts, PaymentHistory, AmountOwed, CreditMix, CreditLength, NewCredit, Rating) :-
    average_income(Income),
    moderate_debt(Debts),
    good_payment_history(PaymentHistory),
    moderate_amount_owed(AmountOwed),
    some_credit_mix(CreditMix),
    medium_credit_history(CreditLength),
    some_new_credit(NewCredit),
    Rating = good.

predict_from_params(Income, Debts, PaymentHistory, AmountOwed, CreditMix, CreditLength, NewCredit, Rating) :-
    low_income(Income),
    high_debt(Debts),
    poor_payment_history(PaymentHistory),
    high_amount_owed(AmountOwed),
    poor_credit_mix(CreditMix),
    short_credit_history(CreditLength),
    lots_of_new_credit(NewCredit),
    Rating = poor.

predict_from_params(_, _, _, _, _, _, _, Rating) :-
    Rating = fair.


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