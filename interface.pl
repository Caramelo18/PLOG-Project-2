:- use_module(library(system)).

getInt(Input):-
                get_code(Tinput),
                Input is Tinput - 48.

getChar(Input):-get_char(Input),
                get_char(_).

getCode(Input):- get_code(TempInput),
                 %get_code(_),
              	 Input is TempInput.

clearInput :- get_code(H),
              (
                H =\= 10 -> clearInput;
                true
              ).


optionSelect(Min,Max, Value):- write('Insert an option: '),
                            getInt(H),
                            clearInput,
                            nl,
                            Min =< H,
                            H =< Max,
                            Value is H.

displayStart:- write('-----------------------------'), nl,
               write('| |      || DOMINO ||     | |'), nl,
               write('| |                       | |'), nl,
               write('-----------------------------').

displayMenu:- write('-----------------------------'), nl,
              write('| |          Menu         | |'), nl,
              write('| | 1 Solve Easy Domino   | |'), nl,
              write('| | 2 Solve Medium Domino | |'), nl,
              write('| | 3 Solve Hard Domino   | |'), nl,
              write('| | 4 Exit                | |'), nl,
              write('-----------------------------').


selectBot:- nl,
            displayBotLevel,
            nl.
showMenu(Puzzle):- displayStart,
                               nl,
                               displayMenu,
                               nl,
                               repeat,
                               write('-----------------------------'),
                               nl,
                               optionSelect(1,4,Puzzle),
                               write('-----------------------------\n'),
                               nl.


dominos:- showMenu(Puzzle),
          execute(Puzzle).

execute(1):- solveDomino1.
execute(2):- solveDomino2.
execute(3):- solveDomino3.
execute(4).
