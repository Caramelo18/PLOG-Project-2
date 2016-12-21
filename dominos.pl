:-use_module(library(clpfd)).
:-use_module(library(lists)).

:-include('table.pl').

piece(line, col).
validPlacement(piece(A, B), Line, Col, Dir).


generate_pieces(Max, Output):- generate_pieces(0, Max, [], Output).

generate_pieces(Col, Max, Input, Output):- Col1 is Col + 1, Col1 =< Max,
                                           generate_piecesRow(0, Col, Input, Intermediate),
                                           generate_pieces(Col1, Max, Intermediate, Output).
generate_pieces(Col, Col, List, Output):- generate_piecesRow(0, Col, List, Output).

generate_piecesRow(Current, Col, Input, Output):- Curr1 is (Current + 1), Current =< Col,
                                                  append(Input, [piece(Current, Col)], Intermediate),
                                                  generate_piecesRow(Curr1, Col, Intermediate, Output).

generate_piecesRow(_, _, List, List).


getRow(Board, Row, Element):- nth0(Row, Board, Element).
getCol(Row, Col, Element):- nth0(Col, Row, Element).

getElement(Row, Col, Element):- table1(Board), getRow(Board, Row, ERow), getCol(ERow, Col, Element). %TODO - change Board?
/*
getValidPlacementVertical(piece(A,B), Places):-
    validPlacement(piece(A,B), Line, Column, 0),
    write('Vaild'),
    domain([Line], 0, 7),
    domain([Column], 0, 15),
    write(Line), write(' '), write(Col), write('\n'),
    getElement(Line, Column, A), Line1 is Line + 1, getElement(Line1, Column, B), %VERTICAl
    labeling([], Places).*/

isValidPlacement(Line, Col, piece(A,B), Dir):- getElement(Line, Col, A), ((Line1 is Line+1, getElement(Line1, Col, B), Dir is 0);
                                                                         ((Col1 is Col+1), getElement(Line, Col1, B), Dir is 1)).

listValidPlacements(piece(A,B), Output):- findall(Line-Col-Dir, isValidPlacement(Line, Col, piece(A,B), Dir), Output).

listPlacement([Pieces|PiecesS], [Intermediate|I]):- listValidPlacements(Pieces, Intermediate),
                                                       listPlacement(PiecesS, I).
listPlacement([], []).


listAllPlacements(Placements,Pieces):-  listAllPlacement(Pieces,Placements),!.


bindSolution(L,C,D,[]):- L #= -1, C #= -1, D #= 2.
bindSolution(L,C,D,S):- member(L-C-D,S).
    


validPlace([],[],[],_):-write('end').
validPlace([L1|Ls],[C1|Cs],[D1|Ds],[S1|Ss]):-
                bindSolution(L1,C1,D1,S1),
                validPlace(Ls,Cs,Ds,Ss).


resolveDomino(Width,Height,N,Pieces):-
    length(Line,N),
    length(Col,N),
    length(Direction,N),

    domain(Line,-1,Width),
    domain(Col,-1,Height),
    domain(Direction,0,2),

    listAllPlacements(Places,Pieces),
    validPlace(Line,Col,Direction,Places),

    labeling([],Line),
    labeling([],Col),
    labeling([],Direction),
    write('\n'),
    write(Line),
    write('\n'),
    write(Col),
    write('\n'),
    write(Direction).



test:-    generate_pieces(8, Pieces),length(Pieces,N), resolveDomino(8,8,N,Pieces).


test2:- generate_pieces(8, Pieces), write(Pieces),
        listAllPlacements(Placements,Pieces),printa(Pieces,Placements).


printa([],[]).
printa([P|Ps],[S|Ss]):-write(P),write('   '), write(S), write('\n'), printa(Ps,Ss).