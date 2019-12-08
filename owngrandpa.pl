/*
Jesse Barbieri
Professor Francisco 
CS314
"I am my own Granpa" in PROLOG 

FAMILY STRUCTURE 

Mother-----Father 
        |
       ME-------Widow-------Widow's Ex-Husband 
            |           |
         baby boy   red hair daughter -------Father 
                                        |
                                       Son
*/

%Facts 
male(me).
male(father).
male(baby_boy).
male(son_run). 
female(widow). 
female(redhead). 
child(me, father).
child(baby_boy, me).
child(baby_boy, widow).
child(redhead, widow).
child(son_run, father).
child(son_run, redhead). 
spouse(me, widow).
spouse(widow, me).
spouse(redhead, father). 
spouse(father, redhead).

%Rules
daughter(X, Y) :- 
    child(X, Y), female(X); 
    spouse(Y, Z), child(X, Z), female(X). 
mother(X, Y) :-
    child(Z, X), spouse(Z, Y);
    child(Y, X), female(X). 
son_in_law(X, Y) :- 
    spouse(X, Z), daughter(Z, Y), male(X). 
brother(X, Y) :-
    child(X, Z), child(Y, Z), male(X);
    mother(Z, X), mother(Z, Y), male(X). % Brother-In-Law via Mother 
uncle(X, Y) :-
    brother(X, Z), child(Y, Z).
grandchild(X, Y) :- 
    child(X, Z), mother(W, Z), spouse(W, Y);
    grandmother(Y, X). 
grandmother(X, Y) :- 
    mother(X, Z), child(Y, Z), female(X). 
grandfather(X, Y) :- 
    grandchild(Y, X), male(X). 


proof(ME) :- daughter(A,ME), mother(A,ME), son_in_law(D,ME), brother(B, D), uncle(B,ME), brother(B,A), grandchild(F,ME), mother(C,A), grandmother(C,ME), grandchild(ME,C), grandfather(ME,ME).

