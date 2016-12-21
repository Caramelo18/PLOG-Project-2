:-use_module(library(clpfd)).
:-use_module(library(lists)).
:-use_module(library(sets)).

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

isValidPlacement(Line, Col, piece(A,B), Dir):- getElement(Line, Col, A), ((Line1 is Line+1, getElement(Line1, Col, B), Dir is 0); %Vertical
                                                                         ((Col1 is Col+1), getElement(Line, Col1, B), Dir is 1)). %Horizontal

listValidPlacements(piece(A,B), Output):- findall(Line-Col-Dir, isValidPlacement(Line, Col, piece(A,B), Dir), Output).

listPlacement([Pieces|PiecesS], [Intermediate|I]):- listValidPlacements(Pieces, Intermediate),
                                                       listPlacement(PiecesS, I).
listPlacement([], []).


listAllPlacements(Placements,Pieces):-  listPlacement(Pieces,Placements),!.% write(Pieces), write('\n').%, write(Placements).


bindSolution(_,_,_,[]).

bindSolution(L,C,D,S):- member(L-C-D,S).


validPlace(_,_,_, [_]).
validPlace([L1|Ls],[C1|Cs],[D1|Ds],[S1|Ss]):-
                bindSolution(L1,C1,D1,S1),
                %write('\n'), write(S1), write(L1), write(' '), write(C1), write(' '), write(D1),
                %write(Ss), write('\n'),
                validPlace(Ls,Cs,Ds,Ss).


resolveDomino(Width,Height,N,Pieces):-
    length(Line,N),
    length(Col,N),
    length(Direction,N),

    domain(Line,0,Width),
    domain(Col,0,Height),
    domain(Direction,0,1),

    listAllPlacements(IPlaces,Pieces),
    delete(IPlaces, [], Places),

    write(Places), write('\n'), write(Pieces),
    validPlace(Line,Col,Direction,Places),
    something(Line, Col, Direction, [], X),
    /*write(X),
    write('\n'),*/
    is_set(X),
    write(X),

    labeling([],Line),
    labeling([],Col),
    labeling([],Direction),
    write('\n'),
    write(Pieces),
    write('\n'),
    write(Line),
    write('\n'),
    write(Col),
    write('\n'),
    write(Direction).

test:-    generate_pieces(4, Pieces), length(Pieces,N), write(N), write('\n'), !, resolveDomino(6,5,14,Pieces).


test2:- generate_pieces(8, Pieces), write(Pieces),
        listAllPlacements(Placements,Pieces),printa(Pieces,Placements).


printa([],[]).
printa([P|Ps],[S|Ss]):-write(P),write('   '), write(S), write('\n'), printa(Ps,Ss).

something([], [], [], L, L).
something([Line|LineS], [Col|ColS], [Dir|DirS], Input, Output):- append(Input, [place(Line, Col)], Intermediate),
                                                                 attach(Line, Col, Dir, Intermediate, Intermediate1),
                                                                 something(LineS, ColS, DirS, Intermediate1, Output).

attach(Line, Col, 0, Input, Output):- Line1 is Line + 1, append(Input, [place(Line1, Col)], Output).
attach(Line, Col, 1, Input, Output):- Col1 is Col + 1, append(Input, [place(Line, Col1)], Output).
