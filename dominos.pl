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


getRow(Board, Row, Element):- nth1(Row, Board, Element).
getCol(Row, Col, Element):- nth1(Col, Row, Element).

getElement(Row, Col, Element):- table1(Board), getRow(Board, Row, ERow), getCol(ERow, Col, Element). %TODO - change Board?

isValidPlacement(Line, Col, piece(A,B), Dir):- getElement(Line, Col, A), ((Line1 is Line+1, getElement(Line1, Col, B), Dir is 0); %Vertical
                                                                         ((Col1 is Col+1), getElement(Line, Col1, B), Dir is 1)). %Horizontal

listValidPlacements(piece(A,B), Result):- findall(Line-Col-Dir, isValidPlacement(Line, Col, piece(A,B), Dir), Output),
                                          findall(Line-Col-Dir, isValidPlacement(Line, Col, piece(B,A), Dir), Output2),
                                          append(Output,Output2,R),sort(R,Result).
                                                    
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


something([], [], [], L, L).
something([Line|LineS], [Col|ColS], [Dir|DirS], Input, Output):- append(Input, [place(Line, Col)], Intermediate),
                                                                 attach(Line, Col, Dir, Intermediate, Intermediate1),
                                                                 something(LineS, ColS, DirS, Intermediate1, Output).

attach(Line, Col, 0, Input, Output):- Line1 is Line + 1, append(Input, [place(Line1, Col)], Output).
attach(Line, Col, 1, Input, Output):- Col1 is Col + 1, append(Input, [place(Line, Col1)], Output).

createSpaces(L,C,Array):- Total is C * L,
                          length(Array,Total).


notEmpty(Line,Col):- getElement(Line,Col,Element),  Element \== e.

/** FIRST RESCTRICTION */

/** FIRST LINE & FIRST COL */
restrictNeighbors(MaxLine,MaxCol,1,1,East,South):- notEmpty(1,1),
                                                   %only rigth and down
                                                   element(1,East,E0),
                                                   element(1,South,S0),
                                                   E0 + S0 #= 1,
                                                   write('1 '),write('1'), write('\n'),
                                                   restrictNeighbors(MaxLine,MaxCol,1,2,East,South).

/** FIRST LINE & LAST COL */
restrictNeighbors(MaxLine,MaxCol,1,MaxCol,East,South):- notEmpty(1,MaxCol),
                                                        %only left and down
                                                        Left is MaxCol - 1,

                                                        element(Left,East,EastBorder),
                                                        element(MaxCol,South,SouthBorder),
                                                        EastBorder + SouthBorder #= 1,

                                                        write('1 '),write(MaxCol), write('MAX COL'),write('\n'),
                                                        restrictNeighbors(MaxLine,MaxCol,2,1,East,South).
/** FIRST LINE */
restrictNeighbors(MaxLine,MaxCol,1,Col,East,South):- notEmpty(1,Col),
                                                     %only left, rigth, down
                                                     Left is Col -1,

                                                     element(Left,East,WestBorder),
                                                     element(Col,East,EastBorder),
                                                     element(Col,South,SouthBorder),

                                                     WestBorder + EastBorder + SouthBorder  #=1,

                                                     write('1 '),write(Col), write('\n'),
                                                     NextCol is Col + 1,
                                                     restrictNeighbors(MaxLine,MaxCol,1,NextCol,East,South).

/** LAST LINE & FIRST COL */
restrictNeighbors(MaxLine,MaxCol,MaxLine,1,East,South):- notEmpty(MaxLine,1),
                                                         write(MaxLine),write('1'), write('\n'),
                                                         %only up and right
                                                         SpaceIndex is MaxLine * MaxCol - MaxCol + 1,
                                                         Up is SpaceIndex - MaxCol,

                                                         element(Up,South,NorthBorder),
                                                         element(SpaceIndex,East,EastBorder),

                                                         NorthBorder + EastBorder #= 1,

                                                         restrictNeighbors(MaxLine,MaxCol,MaxLine,2,East,South).

/** LAST LINE & LAST COL */
restrictNeighbors(MaxLine,MaxCol,MaxLine,MaxCol,_,_):-write(MaxLine),write(MaxCol), write('LAST'), write('\n').

/** LAST LINE */
restrictNeighbors(MaxLine,MaxCol,MaxLine,Col,East,South):- notEmpty(MaxLine,Col),
                                                            %only up , Left, Right
                                                             SpaceIndex is (MaxLine - 1) * MaxLine + Col + (MaxLine - 1),
                                                             Up is SpaceIndex - MaxCol,
                                                             Left is SpaceIndex -1,

                                                             element(Up,South,NorthBorder),
                                                             element(SpaceIndex,East,EastBorder),
                                                             element(Left,East,WestBorder),
                                                             element(SpaceIndex,South,SouthBorder),

                                                             SouthBorder #= 0,     

                                                             NorthBorder + EastBorder + WestBorder + SouthBorder#= 1,

                                                            write(MaxLine), write(' '), write(Col), write('\n'),
                                                            NextCol is Col + 1,
                                                            restrictNeighbors(MaxLine,MaxCol,MaxLine,NextCol,East,South).
/** FIRST COL */
restrictNeighbors(MaxLine,MaxCol,Line,1,East,South):- notEmpty(Line,1),
                                                      write(Line), write(' 1'), write('\n'),
                                                      % only up down right

                                                      SpaceIndex is (Line-1) * MaxLine + 1,
                                                      Up is SpaceIndex - Line,

                                                       element(Up,South,NorthBorder),
                                                       element(SpaceIndex,East,EastBorder),
                                                       element(SpaceIndex,South,SouthBorder),

                                                       NorthBorder + EastBorder + SouthBorder #= 1,
                                                       restrictNeighbors(MaxLine,MaxCol,Line,2, East,South).
/** LAST COL */
restrictNeighbors(MaxLine,MaxCol,Line,MaxCol,East,South):- notEmpty(Line,MaxCol),
                                                           write(Line),write(MaxCol), write(' MAX COL\n'),
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

                                                           restrictNeighbors(MaxLine,MaxCol,NextLine,1,East,South).

/** IN MIDLE */

restrictNeighbors(MaxLine,MaxCol,Line,Col,East,South):-  notEmpty(Line,Col),
                                                         write(Line), write(' '),write(Col), write(' \n'),
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
                                                         restrictNeighbors(MaxLine,MaxCol,Line,NextCol,East,South).




/** EMPTY BOARD PLACE */

restrictNeighbors(MaxLine,MaxCol,Line,MaxCol,East,South):-  write(Line),write(MaxCol), write('Vazio'), write('\n'),
                                                            NextLine is Line + 1,
                                                            restrictNeighbors(MaxLine,MaxCol,NextLine,0,East,South).

restrictNeighbors(MaxLine,MaxCol,Line,Col,East,South):-  write(Line), write(' '),write(Col), write(' Vazio \n'),
                                                         NextCol is Col + 1,
                                                         restrictNeighbors(MaxLine,MaxCol,Line,NextCol,East,South).

/** SECOND RESCTRICTION */

placement([],_,_,_,_).
placement([P|Ps],East,South,MaxLine,MaxCol):- restrictPlacement(P,East,South,MaxLine,MaxCol),
                                               write('NextLine \n'),
                                              placement(Ps,East,South,MaxLine,MaxCol).


restrictPlacement(Ps,East,South,MaxLine,MaxCol):- compileOptions(Ps,East,South,Result,MaxLine,MaxCol),
                                                  sum(Result, #= , 1).

compileOptions([],_,_,[],_,_):-write('FIM \n').
%vertical
compileOptions([L-C-0|Ps],East,South,[O|Os],MaxLine,MaxCol):-
                                          SpaceIndex is (L - 1) * MaxLine + C + (L - 1),
                                          element(SpaceIndex,South,O),
                                          write('vertical \n'),
                                          compileOptions(Ps,East,South,Os,MaxLine,MaxCol).
%Horizontal
compileOptions([L-C-1|Ps],East,South,[O|Os],MaxLine,MaxCol):-
                                          SpaceIndex is (L - 1) * MaxLine + C + (L - 1),
                                          element(SpaceIndex,East,O),
                                          write('Horizontal \n'),
                                          compileOptions(Ps,East,South,Os,MaxLine,MaxCol).

% TODO O dominio de  line e COl ta certo ???  nao é  Width -1 e ...

resolveDomino(Width,Height,N,Pieces):-
    length(Line,N),
    length(Col,N),
    length(Direction,N),


    domain(Line,0,Width),
    domain(Col,0,Height),
    domain(Direction,0,1),


    createSpaces(Width,Height,East),
    createSpaces(Width,Height,South),
    domain(East,0,1),
    domain(South,0,1),

    restrictNeighbors(Height,Width,1,1,East,South),


    listAllPlacements(IPlaces,Pieces),
    delete(IPlaces, [], Places),

    write(Places), write('\n'),


    placement(Places,East,South,Height,Width),
    write('passou \n'),
    %validPlace(Line,Col,Direction,Places),


    /*labeling([],Line),
    labeling([],Col),
    labeling([],Direction),
    write('\n'),
    write(Pieces),
    write('\n'),
    write(Line),
    write('\n'),
    write(Col),
    write('\n'),
    write(Direction).*/
    append(East,South,Solution),
    labeling([], Solution),
    write(East), write('\n'),
    write(South), write('\n').

test:-    generate_pieces(4, Pieces), length(Pieces,N), write(N), write('\n'),!, resolveDomino(6,5,14,Pieces).


test2:- generate_pieces(4, Pieces), write(Pieces),
        listAllPlacements(Placements,Pieces),printa(Pieces,Placements).

test4:- notEmpty(4,7).

printa([],[]).
printa([P|Ps],[S|Ss]):-write(P),write('   '), write(S), write('\n'), printa(Ps,Ss).


test3:-createSpaces(6,5,East), createSpaces(6,5,South), restrictNeighbors(6,5,5,1,East,South). %restrictNeighbors(6,5, 0, 0, East, South).
