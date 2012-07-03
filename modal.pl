%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                      %
% Verwendetes Prolog-System: "SWI"                                     %
%                                                                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Deklaration von Junktoren als Prolog-Operatoren                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- op(410, fy, all).   % Allquantor (fuer Nachfolgerwelten)
:- op(410, fy, one).   % Existenzquantor (fuer Nachfolgerwelten)
:- op(410, fy, ~).     % Negation
:- op(420, xfy, &).    % Konjunktion
:- op(430, xfy, ?).    % Disjunktion
:- op(440, xfy, -->).  % Implikation


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ihre Loesung                                                         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% modal(+Formula,+Structure,-World)
% berechnet (in verschiedenen erfolgreichen Ableitungen) alle Welten
% "World", in denen die modallogische Formel "Formula" bzgl. der
% Modalstruktur "Structure" erfuellt ist.

gilt(X, [X|_]).
gilt(X, [_|T]) :- gilt(X, T).

% in der uebergebenen Struktur nach der gesuchten welt suchen und ausg.
getworld(World, [world(World,X,Y)|_], world(World,X,Y)).
getworld(World, [_|T], W):- getworld(World, T, W).

% nach Nachfolgewelten suchen in denen X gilt. (one X)
eine(X, [W|R], S) :- logik(X, W, S) ; eine(X, R, S).

% X soll in allen Nachfolgewelten erfuellt sein (all)
alle(_, [], _).
alle(X, [W|R], S) :- logik(X, W, S), alle(X, R, S). 

% die AL auswerten
logik(X --> Y,World,S):- not(logik(X, World, S)) ; logik(Y, World, S).
logik(X ? Y, World, S):- logik(X, World, S) ; logik(Y, World, S).
logik(X & Y, World, S):- logik(X, World, S) , logik(Y, World, S).
logik(~ X, World, S) :-  not(logik(X, World, S)).
logik(one X, World, S):- getworld(World, S, world(_,_,W)), eine(X, W, S).
logik(all X, World, S):- getworld(World, S, world(_,_,W)), alle(X, W, S).
logik(X, World, S):-     getworld(World, S, world(_,P,_)), gilt(X,P).

% durchtesten ob die jeweilige welt F erfuellt usw.
modal(F,S,[world(H,_,_)|_],H):- logik(F,H,S).
modal(F,S,[_|T],H):- modal(F,S,T,H).
modal(F,S,World) :- modal(F,S,S,World).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Einige Eingabeformeln mit ihren jeweiligen erfuellenden Welten         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

structure(1,
  [
    world(w1, [],     [w2, w4]),
    world(w2, [p],    [w3]),
    world(w3, [q],    [w1, w3]),
    world(w4, [p, q], [w1, w5]),
    world(w5, [q],    [])
  ]
).

formula(1, one all p).
formula(2, q --> all p).
formula(3, one p ? all ~ q).
formula(4, ~ one (q --> p)).
formula(5, all ~ p --> one q).
formula(6, one (p ? q) --> all (~ p ? ~ q)).
formula(7, one ~ p & all (q --> p)).

worlds(1, [w3, w4]).
worlds(2, [w1, w2, w5]).
worlds(3, [w1, w5]).
worlds(4, [w2, w5]).
worlds(5, [w1, w2, w3, w4]).
worlds(6, [w2, w3, w4, w5]).
worlds(7, []).

query(1, 1).
query(2, 1).
query(3, 1).
query(4, 1).
query(5, 1).
query(6, 1).
query(7, 1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Testpraedikate                                                         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% test_all/0: Testet alle vordefinierten Eingaben                        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_all :- test_all([1,2,3,4,5,6,7]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% test_all(+TestCases): Testet alle Eingaben in TestCases                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_all([]).
test_all([TestCase | TestCases]) :-
  write('Testcase '), write(TestCase), write(': '),
  test(TestCase), nl, test_all(TestCases).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% test(+TestCase): Testet eine einzelne Eingabe                          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test(TestCase) :-
  query(TestCase,StructureID),
  formula(TestCase,Formula),
  structure(StructureID,Structure),
  worlds(TestCase,SortedWorlds),
  findall(World,modal(Formula,Structure,World),Worlds),
  ((ground(Worlds) , sort(Worlds,SortedWorlds)) ->
      write(success)
    ;
      write(failure)
  ).
