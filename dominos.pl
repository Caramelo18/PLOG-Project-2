:-use_module(library(clpfd)).
:-use_module(library(lists)).

:-include('table.pl').

piece(line, col).
validPlacement(piece(line, col), pline, pcol, pdir).


generate_pieces(Max, Output):- generate_pieces(0, Max, [], Output).

generate_pieces(Col, Max, Input, Output):- Col1 is Col + 1, Col1 =< Max,
                                           generate_piecesRow(0, Col, Input, Intermediate),
                                           generate_pieces(Col1, Max, Intermediate, Output).
generate_pieces(Col, Col, List, Output):- generate_piecesRow(0, Col, List, Output), write(Output).

generate_piecesRow(Current, Col, Input, Output):- Curr1 is (Current + 1), Current =< Col,
                                                  append(Input, [piece(Current, Col)], Intermediate),
                                                  generate_piecesRow(Curr1, Col, Intermediate, Output).

generate_piecesRow(_, _, List, List).


getRow(Board, Row, Element):- nth0(Row, Board, Element).
getCol(Row, Col, Element):- nth0(Col, Row, Element).

getElement(Row, Col, Element):- table(Board), getRow(Board, Row, ERow), getCol(ERow, Col, Element). %TODO - change Board?
