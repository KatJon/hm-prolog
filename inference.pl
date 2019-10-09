
type(int).
type(bool).
type(fun(X,Y)) :- type(X), type(Y).

empty_subst([]).

extend_subst(Subst, Var, Type, [(Var,Type) | Subst]).

skip_vars(Subst, [], Subst).
skip_vars(Subst, [V|Vs], Subst2) :-
    select((V,_), Subst, Subst1),
    skip_vars(Subst1, Vs, Subst2).
skip_vars(Subst, [_|Vs], Subst1) :- 
    skip_vars(Subst, Vs, Subst1).

% TODO: Write substitution composition.
compose_subst(S1,S2,S3) :- fail.

apply_subst(Subst, var(X), SubX) :-
    member((X,SubX), Subst).
apply_subst(Subst, fun(X,Y), fun(X1,Y1)) :-
    apply_subst(Subst, X, X1),
    apply_subst(Subst, Y, Y1).
apply_subst(_, Type, Type).

apply_subst_scheme(Subst, forall(Vars, Type), forall(Vars, Type1)) :-
    skip_vars(Subst, Vars, Subst1),
    apply_subst(Subst1, Type, Type1).

unify(int, int, []).
unify(bool, bool, []).
unify(fun(L1,R1), fun(L2,R2), Subst) :-
    unify(L1, L2, SubL),
    unify_with_subst(R1, R2, SubL, Subst).

unify_with_subst(L, R, SubIn, SubOut) :-
    apply_subst(SubIn, L, L1),
    apply_subst(SubIn, R, R1),
    unify(L1, R1, SubUnif),
    compose_subst(SubIn, SubUnif, SubOut).