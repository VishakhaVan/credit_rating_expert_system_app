% expertsystem.pl

:- module(expertsystem, [
    predict_rating/2,
    evaluate_prediction/2,
    customer/10,
    show_all_evaluations/0,
    general_accuracy/1,
    predict_from_params/8,
    explain_rating/2,
    explain_from_params/8,
    improvement_tips/2,
    improvement_tips_from_params/8
]).

% ------------ KNOWLEDGE BASE ---------------

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
    customer(Name, _, Income, Debts, _, _, _, _, _, _),
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
    customer(Name, _, _, _, _, _, _, _, _, ActualRating),
    predict_rating(Name, PredictedRating),
    (ActualRating = PredictedRating -> Accuracy = correct ; Accuracy = incorrect).

% ------------ SHOW ALL EVALUATIONS ---------------
show_all_evaluations :-
    customer(Name, _, _, _, _, _, _, _, _, _),
    evaluate_prediction(Name, Accuracy),
    format('~w: ~w~n', [Name, Accuracy]),
    fail.
show_all_evaluations.

% ------------ GENERAL ACCURACY ---------------
general_accuracy(Accuracy) :-
    findall(Name, customer(Name, _, _, _, _, _, _, _, _, _), Names),
    include(is_correct, Names, CorrectPredictions),
    length(Names, Total),
    length(CorrectPredictions, Correct),
    Accuracy is (Correct / Total) * 100.

is_correct(Name) :-
    evaluate_prediction(Name, correct).

% ------------ FACTOR RULES ---------------
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

% ------------ PREDICT BASED ON PARAMETERS ---------------
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

% ------------ EXPLANATION RULES ---------------

explain_rating(Name, Explanation) :-
    customer(Name, _, Income, Debts, PaymentHistory, AmountOwed, CreditMix, CreditLength, NewCredit, _),
    findall(Reason, (
        income_reason(Income, Reason);
        debt_reason(Debts, Reason);
        payment_history_reason(PaymentHistory, Reason);
        amount_owed_reason(AmountOwed, Reason);
        credit_mix_reason(CreditMix, Reason);
        credit_length_reason(CreditLength, Reason);
        new_credit_reason(NewCredit, Reason)
    ), Reasons),
    atomic_list_concat(Reasons, ', ', Explanation).

explain_from_params(Income, Debts, PaymentHistory, AmountOwed, CreditMix, CreditLength, NewCredit, Explanation) :-
    findall(Reason, (
        income_reason(Income, Reason);
        debt_reason(Debts, Reason);
        payment_history_reason(PaymentHistory, Reason);
        amount_owed_reason(AmountOwed, Reason);
        credit_mix_reason(CreditMix, Reason);
        credit_length_reason(CreditLength, Reason);
        new_credit_reason(NewCredit, Reason)
    ), Reasons),
    atomic_list_concat(Reasons, ', ', Explanation).


% ----- REASON MAPPINGS -----

income_reason(Income, 'high income') :- good_income(Income).
income_reason(Income, 'average income') :- average_income(Income).
income_reason(Income, 'low income') :- low_income(Income).

debt_reason(Debts, 'high debt-to-income ratio') :- high_debt(Debts).
debt_reason(Debts, 'moderate debt') :- moderate_debt(Debts).
debt_reason(Debts, 'low debt') :- low_debt(Debts).

payment_history_reason(Score, 'excellent payment history') :- excellent_payment_history(Score).
payment_history_reason(Score, 'good payment history') :- good_payment_history(Score).
payment_history_reason(Score, 'poor payment history') :- poor_payment_history(Score).

amount_owed_reason(Amount, 'low amount owed') :- low_amount_owed(Amount).
amount_owed_reason(Amount, 'moderate amount owed') :- moderate_amount_owed(Amount).
amount_owed_reason(Amount, 'high amount owed') :- high_amount_owed(Amount).

credit_mix_reason(Mix, 'diverse credit mix') :- diverse_credit_mix(Mix).
credit_mix_reason(Mix, 'some credit mix') :- some_credit_mix(Mix).
credit_mix_reason(Mix, 'poor credit mix') :- poor_credit_mix(Mix).

credit_length_reason(Years, 'long credit history') :- long_credit_history(Years).
credit_length_reason(Years, 'medium credit history') :- medium_credit_history(Years).
credit_length_reason(Years, 'short credit history') :- short_credit_history(Years).

new_credit_reason(Inquiries, 'no new credit inquiries') :- no_new_credit(Inquiries).
new_credit_reason(Inquiries, 'some new credit inquiries') :- some_new_credit(Inquiries).
new_credit_reason(Inquiries, 'many new credit inquiries') :- lots_of_new_credit(Inquiries).

% ------------ IMPROVEMENT TIPS ---------------

improvement_tips_from_params(_, Debts, PaymentHistory, _, _, _, NewCredit, Tips) :-
    findall(Tip, (
        debt_improvement(Debts, Tip);
        payment_history_improvement(PaymentHistory, Tip);
        new_credit_improvement(NewCredit, Tip)
    ), TipsList),
    atomic_list_concat(TipsList, ', ', Tips).

improvement_tips(Name, Tips) :-
    customer(Name, _, _, Debts, PaymentHistory, _, _, _, NewCredit, _),
    findall(Tip, (
        debt_improvement(Debts, Tip);
        payment_history_improvement(PaymentHistory, Tip);
        new_credit_improvement(NewCredit, Tip)
    ), TipsList),
    atomic_list_concat(TipsList, ', ', Tips).

% Improvement rules for debts
debt_improvement(Debts, 'Pay off at least 30% of outstanding dues') :-
    high_debt(Debts).
debt_improvement(Debts, 'Work on reducing your debt-to-income ratio') :-
    moderate_debt(Debts).

% Improvement rules for payment history
payment_history_improvement(Score, 'Try to improve your payment history by making payments on time') :-
    poor_payment_history(Score).
payment_history_improvement(Score, 'Maintain a good payment history for a better credit score') :-
    good_payment_history(Score).

% Improvement rules for new credit inquiries
new_credit_improvement(Inquiries, 'Avoid applying for multiple loans at once') :-
    lots_of_new_credit(Inquiries).
new_credit_improvement(Inquiries, 'Be mindful of your new credit inquiries') :-
    some_new_credit(Inquiries).


  
    
