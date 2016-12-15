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

listValidPlacements(piece(A,B), Output):- findall([piece(A,B), Line, Col, Dir], isValidPlacement(Line, Col, piece(A,B), Dir), Output).

listAllPlacements([Pieces|PiecesS], Input, Output):- listValidPlacements(Pieces, Intermediate), append(Input, Intermediate, Intermediate2),
                                                     listAllPlacements(PiecesS, Intermediate2, Output).
listAllPlacements([], I, O):- sort(I, O).

listAllPlacements(Placements):- generate_pieces(4, Pieces), listAllPlacements(Pieces, [], Placements), write(Placements).
