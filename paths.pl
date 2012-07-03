%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                      %
% Verwendetes Prolog-System: "SWI"                                     %
%                                                                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% paths(+Start,+Goal,+Graph,-Paths)
% berechnet die Anzahl "Paths" der Pfade gerader Laenge (gerade Anzahl
% an Kanten) von dem Knoten "Start" zu dem Knoten "Goal" in dem durch
% die Liste "Graph" repraesentierten gerichteten azyklischen Graphen.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% sucht den Knoten in dem Graph und gibt die Nachbarn                  %
% sowie den restlichen Graph (ohne den gesuchten Knoten) zurueck       %
% knoten(+Graph,+Knoten,-Nachbarn,-GraphRest)                          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

knoten([],_,[],[]).
knoten([node(Knoten,Nachbarn)|GraphRest],Knoten,Nachbarn,GraphRest).
knoten([H|T],Knoten,Nachbarn,[H|GraphRest]) :- 
    knoten(T,Knoten,Nachbarn,GraphRest).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% nachsehen ob einer der nachbarn der zielknoten ist                   %
% und ob die Anzahl der bis dahin unternommenen Schritte gerade ist.   %
% Ansonsten Die Nachbarknoten einzeln als Startknoten in paths geben   %
% goal(+Nachbarn,+Zielknoten,+Graph,-Paths,+Kanten)                    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

goal([],_,_,0,_).
goal([Goal|Rest],Goal,Graph,Paths,K):-
    goal(Rest,Goal,Graph,P,K),
    (0 is (K mod 2), Paths is P+1 ; Paths is P).
goal([H|T],Goal,Graph,Paths,K):-
    paths(H,Goal,Graph,P1,K),
    goal(T,Goal,Graph,P2,K),
    Paths is P1+P2.
    
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Startknoten suchen, Nachbarn durchsuchen usw.                        %
% paths(+Start,+Goal,+Graph,-Paths,+Kanten)                            %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

paths(_,_,[],0,_).
paths(Start,Goal,Graph,Paths,Kanten):- 
    knoten(Graph,Start,Nachbarn,GraphNeu),
    K is Kanten+1,
    goal(Nachbarn,Goal,GraphNeu,Paths,K).

% fuer den Fall das Start=Zielknoten
paths(Start,Start,_,1).

% anfang und so
paths(Start,Goal,Graph,Paths):-
    paths(Start,Goal,Graph,Paths,0).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Einige Testeingaben zusammen mit den erwarteten Anzahlen an Pfaden     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

testcase(1,a,a,[node(a,[b,c]),node(b,[e]),node(c,[d,e]),node(d,[g,h]),
                node(e,[d,f]),node(f,[g]),node(g,[h]),node(h,[])],
         1).
testcase(2,b,d,[node(a,[b,c]),node(b,[e]),node(c,[d,e]),node(d,[g,h]),
                node(e,[d,f]),node(f,[g]),node(g,[h]),node(h,[])],
         1).
testcase(3,c,g,[node(a,[b,c]),node(b,[e]),node(c,[d,e]),node(d,[g,h]),
                node(e,[d,f]),node(f,[g]),node(g,[h]),node(h,[])],
         1).
testcase(4,e,a,[node(a,[b,c]),node(b,[e]),node(c,[d,e]),node(d,[g,h]),
                node(e,[d,f]),node(f,[g]),node(g,[h]),node(h,[])],
         0).
testcase(5,b,g,[node(a,[b,c]),node(b,[e]),node(c,[d,e]),node(d,[g,h]),
                node(e,[d,f]),node(f,[g]),node(g,[h]),node(h,[])],
         0).
testcase(6,a,h,[node(a,[b,c]),node(b,[e]),node(c,[d,e]),node(d,[g,h]),
                node(e,[d,f]),node(f,[g]),node(g,[h]),node(h,[])],
         3).
testcase(7,n7,n5,[node(n1,[]),node(n2,[n5]),node(n3,[n5,n6,n14]),
                  node(n4,[n1,n2,n3,n7]),node(n5,[n1]),node(n6,[n1,n2,n10]),
                  node(n7,[n8,n12]),node(n8,[n9]),node(n9,[n3]),
                  node(n10,[n11]),node(n11,[n1]),node(n12,[n3]),
                  node(n13,[n6]),node(n14,[n13])],
         3).
testcase(8,n4,n11,[node(n1,[]),node(n2,[n5]),node(n3,[n5,n6,n14]),
                   node(n4,[n1,n2,n3,n7]),node(n5,[n1]),node(n6,[n1,n2,n10]),
                   node(n7,[n8,n12]),node(n8,[n9]),node(n9,[n3]),
                   node(n10,[n11]),node(n11,[n1]),node(n12,[n3]),
                   node(n13,[n6]),node(n14,[n13])],
         4).
testcase(9,n4,n5,[node(n1,[]),node(n2,[n5]),node(n3,[n5,n6,n14]),
                  node(n4,[n1,n2,n3,n7]),node(n5,[n1]),node(n6,[n1,n2,n10]),
                  node(n7,[n8,n12]),node(n8,[n9]),node(n9,[n3]),
                  node(n10,[n11]),node(n11,[n1]),node(n12,[n3]),
                  node(n13,[n6]),node(n14,[n13])],
         7).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Testpraedikate                                                         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% test_all/0: Testet alle vordefinierten Eingaben                        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_all :- test_all([1,2,3,4,5,6,7,8,9]).

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
  testcase(TestCase,Start,Goal,Graph,N),
  ((paths(Start,Goal,Graph,Paths), Paths == N) ->
    write(success)
  ;
    write(failure)
  ).
