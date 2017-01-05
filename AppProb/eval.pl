%% eval.pl - Untyped lambda calculus interpreter written in Prolog.
%%
%% Because it's Prolog it naturally also runs in ProbLog.
%% Author: Alexander Vandenbroucke (alexander.vandenbroucke@kuleuven.be)

% value(T) succeeds if T is a value (lambda-abstraction or literal)
value(lam(_)).
value(t).
value(f).

% shift(T,K,C,Tn) shifts all variables in T greater than cutoff C by K places.
shift(var(I),_,C,var(I)) :- I < C.
shift(var(I),K,C,var(J)) :-
    I >= C,
    J is I + K.
shift(lam(T),K,C,lam(Tn)) :-
    D is C + 1,
    shift(T,K,D,Tn).
shift(app(T1,T2),K,C,app(T1n,T2n)) :-
    shift(T1,K,C,T1n),
    shift(T2,K,C,T2n).
shift(and(T1,T2),K,C,and(T1n,T2n)) :-
    shift(T1,K,C,T1n),
    shift(T2,K,C,T2n).
shift(or(T1,T2),K,C,or(T1n,T2n)) :-
    shift(T1,K,C,T1n),
    shift(T2,K,C,T2n).
shift(not(T),K,C,not(Tn)) :-
    shift(T,K,C,Tn).
shift(ite(TC,T1,T2),K,C,ite(TCn,T1n,T2n)) :-
    shift(TC,K,C,TCn),
    shift(T1,K,C,T1n),
    shift(T2,K,C,T2n).
shift(t,_,_,t).
shift(f,_,_,f).

% subst(T,S,J,Tn) substitutes S for every variable J in T.
subst(var(I),S,J,S) :- I == J.
subst(var(I),_,J,var(I)) :- I \== J.
subst(lam(T),S,J,lam(Tn)) :-
    Jn is J + 1,
    shift(S,1,0,Sn),
    subst(T,Sn,Jn,Tn).
subst(app(T1,T2),S,J,app(T1n,T2n)) :-
    subst(T1,S,J,T1n),
    subst(T2,S,J,T2n).
subst(and(T1,T2),S,J,and(T1n,T2n)) :-
    subst(T1,S,J,T1n),
    subst(T2,S,J,T2n).
subst(or(T1,T2),S,J,or(T1n,T2n)) :-
    subst(T1,S,J,T1n),
    subst(T2,S,J,T2n).
subst(not(T),S,J,not(Tn)) :-
    subst(T,S,J,Tn).
subst(ite(TC,T1,T2),S,J,ite(TCn,T1n,T2n)) :-
    subst(TC,S,J,TCn),
    subst(T1,S,J,T1n),
    subst(T2,S,J,T2n).
subst(t,_,_,t).
subst(f,_,_,f).

% step(T,T') succeeds if T -> T' according to small-step semantics.
step(app(T1,T2),app(T1n,T2)) :- step(T1,T1n).
step(app(V1,T2),app(V1,T2n)) :- value(V1), step(T2,T2n).
step(app(lam(T12),V2),T) :- subst(T12,V2,0,T).

step(and(T1,T2),and(T1n,T2)) :- step(T1,T1n).
step(and(V1,T2),and(V1,T2n)) :- step(T2,T2n).
step(and(t,t),t).
step(and(t,f),f).
step(and(f,t),f).
step(and(f,f),f).

step(or(T1,T2),or(T1n,T2)) :- step(T1,T1n).
step(or(V1,T2),or(V1,T2n)) :- step(T2,T2n).
step(or(t,t),t).
step(or(t,f),t).
step(or(f,t),t).
step(or(f,f),f).

step(not(T),not(Tn)) :- step(T,Tn).
step(not(t),f).
step(not(f),t).

step(ite(TC,T1,T2),ite(TCn,T1,T2)) :- step(TC,TCn).
step(ite(VC,T1,T2),ite(VC,T1n,T2)) :- step(T1,T1n).
step(ite(VC,V1,T2),ite(VC,V1,T2n)) :- step(T2,T2n).
step(ite(t,V1,_),V1).
step(ite(f,_,V2),V2).

% eval(T,V) succeeds if T eventually evaluates to a value V, it fails if
% T gets stuck.
eval(T,V) :-
    ( value(T), T = V
     ;
      step(T,Tn),
      eval(Tn,V)
    ).
     
