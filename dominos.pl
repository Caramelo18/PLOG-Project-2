:-use_module(library(clpfd)).
:-use_module(library(lists)).

:-include('table.pl').
:-include('interface.pl').

piece(line, col).


generate_pieces(Max, Output):- generate_pieces(0, Max, [], Output).

generate_pieces(Col, Max, Input, Output):- Col1 is Col + 1, Col1 =< Max,
                                           generate_piecesRow(0, Col, Input, Intermediate),
                                           generate_pieces(Col1, Max, Intermediate, Output).
generate_pieces(Col, Col, List, Output):- generate_piecesRow(0, Col, List, Output).

generate_piecesRow(Current, Col, Input, Output):- Curr1 is (Current + 1), Current =< Col,
                                                  append(Input, [piece(Current, Col)], Intermediate),
                                                  generate_piecesRow(Curr1, Col, Intermediate, Output).

generate_piecesRow(_, _, List, List).


getRow(Board, Row, Element):- nth1(Row, Board, Element).
getCol(Row, Col, Element):- nth1(Col, Row, Element).

getElement(Row, Col, Element, Board):- getRow(Board, Row, ERow), getCol(ERow, Col, Element).

isValidPlacement(Line, Col, piece(A,B), Dir, Board):- getElement(Line, Col, A, Board), ((Line1 is Line+1, getElement(Line1, Col, B, Board), Dir is 0); %Vertical
                                                                                       ((Col1 is Col+1), getElement(Line, Col1, B, Board), Dir is 1)). %Horizontal

listValidPlacements(piece(A,B), Board, Result):- findall(Line-Col-Dir, isValidPlacement(Line, Col, piece(A,B), Dir, Board), Output),
                                                 findall(Line-Col-Dir, isValidPlacement(Line, Col, piece(B,A), Dir, Board), Output2),
                                                 append(Output,Output2,R),sort(R,Result).

listPlacement([Pieces|PiecesS], Board, [Intermediate|I]):- listValidPlacements(Pieces, Board, Intermediate),
                                                           listPlacement(PiecesS, Board, I).
listPlacement([], _, []).


listAllPlacements(Pieces, Board, Placements):-  listPlacement(Pieces, Board, Placements).



notEmpty(Line,Col, Board):- getElement(Line,Col,Element, Board),  Element \== e.

/** FIRST RESCTRICTION */

/** FIRST LINE & FIRST COL */
restrictNeighbors(MaxLine,MaxCol,1,1,East,South, Board):- notEmpty(1,1, Board),
                                                          %only rigth and down
                                                          element(1,East,E0),
                                                          element(1,South,S0),
                                                          E0 + S0 #= 1,
                                                          %write('1 '),write('1'), write('\n'),
                                                          restrictNeighbors(MaxLine,MaxCol,1,2,East,South, Board).

/** FIRST LINE & LAST COL */
restrictNeighbors(MaxLine,MaxCol,1,MaxCol,East,South, Board):- notEmpty(1,MaxCol, Board),
                                                               %only left and down
                                                               Left is MaxCol - 1,

                                                               element(Left,East,EastBorder),
                                                               element(MaxCol,South,SouthBorder),
                                                               EastBorder + SouthBorder #= 1,

                                                               %write('1 '),write(MaxCol), write('MAX COL'),write('\n'),
                                                               restrictNeighbors(MaxLine,MaxCol,2,1,East,South, Board).
/** FIRST LINE */
restrictNeighbors(MaxLine,MaxCol,1,Col,East,South, Board):- notEmpty(1,Col, Board),
                                                            %only left, rigth, down
                                                            Left is Col -1,

                                                            element(Left,East,WestBorder),
                                                            element(Col,East,EastBorder),
                                                            element(Col,South,SouthBorder),

                                                            WestBorder + EastBorder + SouthBorder  #=1,

                                                            %write('1 '),write(Col), write('\n'),
                                                            NextCol is Col + 1,
                                                            restrictNeighbors(MaxLine,MaxCol,1,NextCol,East,South, Board).

/** LAST LINE & FIRST COL */
restrictNeighbors(MaxLine,MaxCol,MaxLine,1,East,South,Board):- notEmpty(MaxLine,1, Board),
                                                               %write(MaxLine),write('1'), write('\n'),
                                                               %only up and right
                                                               SpaceIndex is MaxLine * MaxCol - MaxCol + 1,
                                                               Up is SpaceIndex - MaxCol,

                                                               element(Up,South,NorthBorder),
                                                               element(SpaceIndex,East,EastBorder),

                                                               NorthBorder + EastBorder #= 1,

                                                               restrictNeighbors(MaxLine,MaxCol,MaxLine,2,East,South, Board).

/** LAST LINE & LAST COL */
restrictNeighbors(MaxLine,MaxCol,MaxLine,MaxCol,_,_, _).%:- write(MaxLine),write(MaxCol), write('LAST'), write('\n').

/** LAST LINE */
restrictNeighbors(MaxLine,MaxCol,MaxLine,Col,East,South, Board):- notEmpty(MaxLine,Col, Board),
                                                                  %only up , Left, Right
                                                                  SpaceIndex is (MaxLine - 1) * MaxCol + Col ,
                                                                  Up is SpaceIndex - MaxCol,
                                                                  Left is SpaceIndex -1,

                                                                  element(Up,South,NorthBorder),
                                                                  element(SpaceIndex,East,EastBorder),
                                                                  element(Left,East,WestBorder),
                                                                  element(SpaceIndex,South,SouthBorder),

                                                                  SouthBorder #= 0,
                                                                  NorthBorder + EastBorder + WestBorder + SouthBorder#= 1,

                                                                  %write(MaxLine), write(' '), write(Col), write('\n'),
                                                                  NextCol is Col + 1,
                                                                  restrictNeighbors(MaxLine,MaxCol,MaxLine,NextCol,East,South, Board).

/** FIRST COL */
restrictNeighbors(MaxLine,MaxCol,Line,1,East,South, Board):- notEmpty(Line,1, Board),
                                                             %write(Line), write(' 1'), write('\n'),
                                                             % only up down right

                                                             SpaceIndex is (Line-1) * MaxCol + 1,

                                                             Up is SpaceIndex - MaxCol,

                                                             element(Up,South,NorthBorder),
                                                             element(SpaceIndex,East,EastBorder),
                                                             element(SpaceIndex,South,SouthBorder),

                                                             NorthBorder + EastBorder + SouthBorder #= 1,
                                                             restrictNeighbors(MaxLine,MaxCol,Line,2, East,South, Board).
/** LAST COL */
restrictNeighbors(MaxLine,MaxCol,Line,MaxCol,East,South, Board):- notEmpty(Line,MaxCol, Board),
                                                                  %write(Line),write(MaxCol), write(' MAX COL\n'),
                                                                  NextLine is Line + 1,
                                                                  % up, down, left rigth

                                                                  SpaceIndex is Line * MaxCol,
                                                                  Up is SpaceIndex - MaxCol,
                                                                  Left is SpaceIndex - 1,

                                                                  element(Up,South,NorthBorder),
                                                                  element(Left,East,WestBorder),
                                                                  element(SpaceIndex,South,SouthBorder),
                                                                  element(SpaceIndex,East,EastBorder),

                                                                  EastBorder #= 0,
                                                                  NorthBorder + WestBorder + SouthBorder + EastBorder #= 1,

                                                                  restrictNeighbors(MaxLine,MaxCol,NextLine,1,East,South, Board).

/** IN MIDDLE */
restrictNeighbors(MaxLine,MaxCol,Line,Col,East,South, Board):-  notEmpty(Line,Col, Board),
                                                                %write(Line), write(' '),write(Col), write(' \n'),
                                                                % up down left right

                                                                SpaceIndex is (Line - 1) * MaxCol + Col ,

                                                                Up is SpaceIndex - MaxCol,
                                                                Left is SpaceIndex - 1,

                                                                element(Up,South,NorthBorder),
                                                                element(Left,East,WestBorder),
                                                                element(SpaceIndex,South,SouthBorder),
                                                                element(SpaceIndex,East,EastBorder),

                                                                NorthBorder + WestBorder + SouthBorder + EastBorder #= 1,

                                                                NextCol is Col + 1,
                                                                restrictNeighbors(MaxLine,MaxCol,Line,NextCol,East,South, Board).




/** EMPTY BOARD PLACE */

restrictNeighbors(MaxLine,MaxCol,Line,MaxCol,East,South, Board):-  %write(Line),write(MaxCol), write('Vazio'), write('\n'),
                                                                   %up south, left east, south own must be 0
                                                                   SpaceIndex is (Line - 1) * MaxCol + MaxCol,
                                                                   Up is SpaceIndex - MaxCol,
                                                                   Left is SpaceIndex - 1,
                                                                   element(Up, South, 0),
                                                                   element(Left, East, 0),
                                                                   element(SpaceIndex, South, 0),

                                                                   NextLine is Line + 1,
                                                                   restrictNeighbors(MaxLine,MaxCol,NextLine,0,East,South, Board).

restrictNeighbors(MaxLine,MaxCol,Line,Col,East,South, Board):-  %write(Line), write(' '),write(Col), write(' Vazio \n'),

                                                                SpaceIndex is (Line - 1) * MaxCol + Col ,
                                                                Up is SpaceIndex - MaxCol,
                                                                Left is SpaceIndex - 1,
                                                                element(Up, South, 0),
                                                                element(Left, East, 0),
                                                                element(SpaceIndex, South, 0),
                                                                element(SpaceIndex, East, 0),

                                                                NextCol is Col + 1,
                                                                restrictNeighbors(MaxLine,MaxCol,Line,NextCol,East,South, Board).

/** SECOND RESCTRICTION */

placement([],_,_,_,_).
placement([P|Ps],East,South,MaxLine,MaxCol):- restrictPlacement(P,East,South,MaxLine,MaxCol),
                                              %write('NextLine \n'),
                                              placement(Ps,East,South,MaxLine,MaxCol).


restrictPlacement(Ps,East,South,MaxLine,MaxCol):- compileOptions(Ps,East,South,Result,MaxLine,MaxCol),
                                                  sum(Result, #= , 1).

compileOptions([],_,_,[],_,_).%:-write('FIM \n').
%vertical
compileOptions([L-C-0|Ps],East,South,[O|Os],MaxLine,MaxCol):-  SpaceIndex is (L - 1) * MaxCol + C ,
                                                              element(SpaceIndex,South,O),
                                                             % write('vertical \n'),
                                                              compileOptions(Ps,East,South,Os,MaxLine,MaxCol).
%Horizontal
compileOptions([L-C-1|Ps],East,South,[O|Os],MaxLine,MaxCol):-  SpaceIndex is (L - 1) * MaxCol + C ,
                                                              element(SpaceIndex,East,O),
                                                            %  write('Horizontal \n'),
                                                              compileOptions(Ps,East,South,Os,MaxLine,MaxCol).


resolveDomino(Width,Height,Pieces,East,South, Board):-
    Length is Width * Height,
    length(East, Length),
    length(South, Length),

    append(East,South,Solution),
    domain(Solution,0,1),

    restrictNeighbors(Height,Width,1,1,East,South, Board),

    listAllPlacements(Pieces, Board, IPlacements),
    delete(IPlacements, [], Placements),

    %write(Places), write('\n'),

    placement(Placements,East,South,Height,Width),

    labeling([], Solution),
    write(East), write('\n'),
    write(South), write('\n').


/** DISPLAY SOLUTION SECTION */
printSolution([],_,_,_,_):- write('\n').
printSolution([Line|Lines],East,South,Count,Width):-printEast(Line,East,Count),
                                                    printSouth(Line,South,Count),
                                                    NewCount is Count +Width,
                                                    printSolution(Lines,East,South,NewCount,Width).

printEast([],_,_):- write('\n').
% No border
printEast([e|Ps],East,Count):- nth1(Count,East,0), write('    '),
                               NewCount is Count +1,
                               printEast(Ps,East,NewCount).
printEast([P|Ps],East,Count):- nth1(Count,East,0),write(P), write('   '),
                               NewCount is Count +1,
                               printEast(Ps,East,NewCount).
% with border
printEast([P|Ps],East,Count):- nth1(Count,East,1),write(P), write(' | '),
                               NewCount is Count +1,
                               printEast(Ps,East,NewCount).
printSouth([],_,_):- write('\n').
printSouth([_|Ps],South,Count):-nth1(Count,South,1), write('__  '),
                                NewCount is Count +1,
                                printSouth(Ps,South,NewCount).
printSouth([_|Ps],South,Count):-nth1(Count,South,0), write('    '),
                                NewCount is Count +1,
                                printSouth(Ps,South,NewCount).


displayBoard([]).
displayBoard([B|Bs]):- displayBoardLine(B), displayBoard(Bs).

displayBoardLine([]):- write('\n\n').
displayBoardLine([e|Ls]):- write('    '), displayBoardLine(Ls).
displayBoardLine([L|Ls]):- write(L), write('   '), displayBoardLine(Ls).


solveDomino1:- Width is 5, Height is 4, generate_pieces(4, Pieces), !,  table1(Board),
                                        resolveDomino(Width, Height, Pieces, East, South, Board), !, write('\n\n'),
                                        displayBoard(Board),
                                        write('\n\n'), printSolution(Board, East, South, 1, Width).

solveDomino2:- Width is 6,Height is 5, generate_pieces(4, Pieces), !, table2(Board),
                                       resolveDomino(Width, Height, Pieces, East, South, Board), !, write('\n\n'),
                                       displayBoard(Board),
                                       write('\n\n'), printSolution(Board, East, South, 1, Width).

solveDomino3:- Width is 15, Height is 8, generate_pieces(8, Pieces), !, table3(Board),
                                         resolveDomino(Width, Height, Pieces, East, South, Board), !, write('\n'),
                                         displayBoard(Board),
                                         write('\n\n'), printSolution(Board, East, South, 1, Width).
