:- dynamic detailed_mode_disabled/0.
:- ensure_loaded('files.pl').


%% TODO
% tile/2
% tile(Index, Tile)
%
% Fiecare soluție a predicatului tile este o corespondență între index
% (numărul piesei în lista din enunț) și reprezentarea internă a piesei
% respective.
%
% Puteți alege orice reprezentare doriți, în așa fel încât să puteți
% reprezenta toate piesele din enunț.
%
% Orice muchie a unei piese este o cetate, un drum, sau o pajiște.
% Pot exista cel mult 1 drum și cel mult 2 castele pe aceeași piesă.
%
% Reprezentarea trebuie să poată fi rotită (vezi predicatul ccw/3 mai
% jos) pentru a produce reprezentarea piesei rotite cu 90 de grade.
%
% Trebuie să definiți reprezentări pentru fiecare dintre cele 16 piese
% din enunțul temei.
%
% Exemplu: apelul tile(1, T1). trebuie să lege T1 la reprezentarea pe
% care o faceți pentru piesa 1. Această reprezentare poate fi transmisă
% celorlalte predicate din temă, pentru a întreba, de exemplu, ce se
% află pe muchia de nord a piesei 1, sau dacă piesa 1 se potrivește cu o
% altă piesă.

tile(1, ([c, c, p, c], 0, 0)).
tile(2, ([c, c, d, c], 0, 0)).
tile(3, ([c, c, p, p], 0, 0)).
tile(4, ([c, c, p, p], 1, 0)).
tile(5, ([c, p, c, p], 1, 0)).
tile(6, ([c, p, c, p], 0, 0)).
tile(7, ([c, p, p, p], 0, 0)).
tile(8, ([c, c, d, d], 0, 0)).
tile(9, ([c, p, d, d], 0, 0)).
tile(10, ([c, d, d, p], 0, 0)).
tile(11, ([c, d, p, d], 0, 0)).
tile(12, ([c, d, d, d], 0, 1)).
tile(13, ([p, p, d, d], 0, 0)).
tile(14, ([p, d, p, d], 0, 0)).
tile(15, ([p, d, d, d], 0, 1)).
tile(16, ([d, d, d, d], 0, 1)).



%% TODO
% at/3
% at(+Tile, +Direction, ?What)
%
% Predicatul este adevărat dacă pe piesa Tile are pe muchia de pe
% direcția Direction o entitate de tipul What.
%
% Directions este una dintre n, e, s, w (vezi predicatul directions/1
% din utils.pl).
%
% Entitatea (What) este una dintre c, d, sau p. reprezentând cetate,
% drum, sau pajiște.
%
% De exemplu, piesa 4 are cetate în nord și în este, și pajiște în sud
% și vest. Iar piesa 10 are cetate în nord, drum în este și sud, și
% pajiște în vest.
%
% Dacă What nu este legat, trebuie legat la entitatea care se află pe
% muchia din direcția Dir.
at((L,_,_), Dir, What) :- directions(D), nth0(Index, D, Dir), nth0(Index, L, What).


%% TODO
% atL/3
% atL(+Tile, +Directions, +What)
%
% Predicatul este adevărat dacă piesa Tile are entitatea what pe toate
% direcțiile din lista Directions, cu aceleași valori pentru entități și
% direcții ca și la predicatul at/3.
%
% De exemplu, predicatul este adevărat pentru reprezentarea piesei 1,
% pentru lista [w,n,e], și pentru entitatea c. Este adevărat de asemenea
% pentru reprezentarea piesei 14, pentru lista [e,w], și pentru
% entitatea d.
%
% Atenție! Pentru ca predicatul să fie adevărat, nu este nevoie ca în
% Directions să fie *toate* direcțiile pe care se află entitatea
% respectivă, pot fi doar o submulțime a acestora.
% De exemplu, la piesa 14, predicatul este adevărat pentru entitatea d
% și pentru oricare dintre listele [w], [e] sau [e,w].
atL(Tile, Directions, What) :- forall(member(Dir, Directions), at(Tile, Dir, What)).


%% TODO
% hasTwoCitadels/1
% hasTwoCitadels(+Tile)
%
% Predicatul întoarce adevărat dacă pe piesă există două cetăți diferite
% (ca în piesele 4 și 5).

hasTwoCitadels((_, 1, _)).



%% TODO
% ccw/3
% ccw(+Tile, +Rotation, -RotatedTile)
% Predicatul este adevărat dacă RotatedTile este reprezentarea piesei cu
% reprezentarea Tile, dar rotită de Rotation ori, în sens trigonometric.
%
% De exemplu, dacă T4 este reprezentarea piesei 4, atunci ccw(4, 1, R)
% va lega R la reprezentarea unei piese care are pajiște la nord și
% vest, și cetate la est și sud.
%
% Pentru piesele cu simetrie, reprezentarea unora dintre rotații este
% identică cu piesa inițială.
% De exemplu, la piesele 5, 6 și 14, rotirea cu Rotation=2 va duce la o
% reprezentare identică cu piesa inițială, și la fel rezultatele pentru
% Rotation=1 și Rotation=3 vor fi identice.
% La piesa 16, orice rotire trebuie să aibă aceeași reprezentare cu
% reprezentarea inițială.

ccw(Tile, Rot, Tile) :- Rot mod 4 =:= 0, !.
ccw(([H | R], C, I), Rot, Tile) :-
    Rot1 is Rot - 1,
    append(R, [H], Tile1),
    ccw((Tile1, C, I), Rot1, Tile).



%% TODO
% rotations/2
% rotations(+Tile, -RotationPairs)
%
% Predicatul leagă RotationPairs la o listă de perechi
% (Rotation, RotatedTile)
% în care Rotation este un număr de rotații între 0 și 3 inclusiv și
% RotatedTile este reprezentarea piesei Tile rotită cu numărul respectiv
% de rotații.
%
% Rezultatul trebuie întotdeauna să conțină perechea (0, Tile).
%
% IMPORTANT:
% Rezultatul nu trebuie să conțină rotații duplicate. De exemplu, pentru
% piesele 5,6 și 14 rezultatul va conține doar 2 perechi, iar pentru
% piesa 16 rezultatul va conține o singură pereche.
%
% Folosiți recursivitate (nu meta-predicate).

rotations(Tile, RotationPairs) :- helper(Tile, 1, [(0, Tile)], RotationPairs).

helper(_, 4, Lista, Lista):- !.
helper(Tile, Rotation, Lista, RotationPairs) :-
    ccw(Tile, Rotation, RotatedTile),
    \+ member((_, RotatedTile), Lista),
    append(Lista, [(Rotation, RotatedTile)], Lista1),
    Rotation1 is Rotation + 1,
    helper(Tile, Rotation1, Lista1, RotationPairs).
helper(Tile, Rotation, Lista, RotationPairs) :-
    ccw(Tile, Rotation, RotatedTile),
    member((_, RotatedTile), Lista),
    Rotation1 is Rotation + 1,
    helper(Tile, Rotation1, Lista, RotationPairs).



%% TODO
% match/3
% match(+Tile, +NeighborTile, +NeighborDirection)
%
% Predicatul întoarce adevărat dacă NeighborTile poate fi pusă în
% direcția NeighborDirection față de Tile și se potrivește, adică muchia
% comună este de același fel.
%
% De exemplu, dacă T2 este reprezentarea piesei 2, iar T16 este
% reprezentarea piesei 16, atunci match(T2, T16, s) este adevărat.
%
% Similar, pentru piesele 8 și 10, este adevărat
% ccw(T8, 3, T8R), match(T8R, T10, w).
%
% Puteți folosi predicatul opposite/2 din utils.pl.

match(Tile, NeighborTile, NeighborDirection) :-
    at(Tile, NeighborDirection, What1),
    opposite(NeighborDirection, OppositeDirection),
    at(NeighborTile, OppositeDirection, What2),
    What1 = What2.



%% TODO
% findRotation/3
% findRotation(+Tile, +Neighbors, -Rotation)
%
% Predicatul leagă Rotation la rotația (între 0 și 3 inclusiv) pentru
% piesa cu reprezentarea Tile, astfel încât piesa să se potrivească cu
% vecinii din Neighbors.
%
% Neighbors este o listă de perechi (NeighborTile, NeighborDirection) și
% specifică că pe direcția NeighborDirection se află piesa cu
% reprezentarea NeighborTile. Este posibil ca Neighbors să conțină mai
% puțin de 4 elemente.
%
% Se vor da toate soluțiile care duc la potrivire.
%
% De exemplu, pentru piesa 11, dacă la nord se află piesa 14 rotită o
% dată (drumul este vertical), iar la sud se află piesa 2 rotită de 2
% ori (drumul este spre nord), atunci posibilele rotații pentru piesa 11
% sunt 1 sau 3, deci findRotation trebuie să aibă 2 soluții, în care
% leagă R la 1, și la 3.
% În același exemplu, dacă am avea și piesa 1 ca vecin spre est, atunci
% soluția de mai sus s-ar reduce doar la rotația 3.
%
% Hint: Prolog face backtracking automat. Folosiți match/3.

findRotation(Tile, Neighbors, Rotation) :- 
    rotations(Tile, Rotations),
    member((Rotation, RotatedTile), Rotations),
    forall(member((NeighborTile, NeighborDirection), Neighbors), match(RotatedTile, NeighborTile, NeighborDirection)).




%%%%%%%%%%%%%%%%%%%%%%%%%% Etapa 2


%% TODO
% emptyBoard/1
% emptyBoard(-Board)
%
% Leagă Board la reprezentarea unei table goale de joc (nu a fost
% plasată încă nicio piesă).

% tabla goala este o lista vida
emptyBoard([]).



%% TODO
% boardSet/4
% boardSet(+BoardIn, +Pos, +Tile, -BoardOut)
%
% Predicatul întoarce false dacă se încearcă plasarea unei piese pe o
% poziție pe care este deja o piesă, pe o poziție fără muchie comună
% cu o piesă existentă, sau într-un loc unde piesa nu se potrivește cu
% vecinii săi.
%
% Pentru o tablă goală, predicatul reușește întotdeauna, și poziția Pos
% devine singura de pe tablă.
%
% Poziția este dată ca un tuplu (X, Y).

% daca tabla este goala, pun prima piesa pe tabla (nerotita)
% o pozitie de pe tabla este reprezentata ca un tuplu de 4 elemente
% (X, Y, Tile, Rotation)
% daca tabla nu este goala, verific daca pozitia este libera, daca da, 
% verific daca piesa se potriveste cu vecinii
% imi creez o functie ajutatoare getNeighbors care imi leaga Neighbors la o lista cu vecinii pozitiei
% pos deja pusi pe tabla 
% findRotation imi leaga Rotation la rotatia piesei Tile astfel incat sa se potriveasca cu vecinii
% ccw imi leaga RotatedTile la piesa Tile rotita de Rotation ori
% adaug la BoardOut o noua pozitie cu piesa RotatedTile si rotatia Rotation

boardSet(BoardIn, (X, Y), Tile, BoardOut) :- 
    canPlaceTile(BoardIn, (X, Y), Tile),
    append(BoardIn, [(X, Y, Tile)], BoardOut).


%% TODO
% boardGet/3
% boardGet(+Board, +Pos, -Tile)
%
% Predicatul leagă Tile la reprezentarea piesei de pe tabla Board, de la
% poziția Pos. Poziția este dată ca un tuplu (X, Y).
%
% Dacă la poziția Pos nu este nicio piesă, predicatul eșuează.

boardGet(Board, (X, Y), Tile) :-
    member((X, Y, Tile), Board).



%% TODO
% boardGetLimits/5
% boardGetLimits(+Board, -XMin, -Ymin, -XMax, -YMax)
%
% Predicatul leagă cele 4 argumente la coordonatele x-y extreme la
% care există piese pe tablă.
%
% Pentru o tablă goală, predicatul eșuează.
%
% Hint: max_list/2 și min_list/2

% creez o lista cu toate coordonatele x de pe tabla
% creez o lista cu toate coordonatele y de pe tabla
% leaga XMax la maximul din lista de coordonate x
% leaga XMin la minimul din lista de coordonate x
% leaga YMax la maximul din lista de coordonate y
% leaga YMin la minimul din lista de coordonate y
boardGetLimits([], _, _, _, _) :- false.
boardGetLimits(Board, XMin, YMin, XMax, YMax) :-
    findall(X, member((X, _, _), Board), XList),
    findall(Y, member((_, Y, _), Board), YList),
    max_list(XList, XMax),
    min_list(XList, XMin),
    max_list(YList, YMax),
    min_list(YList, YMin).


%% TODO
% canPlaceTile/3
% canPlaceTile(+Board, +Pos, +Tile)
%
% Întoarce adevărat dacă este o mișcare validă plasarea piese Tile la
% poziția Pos pe tabla Board. Poziția este dată ca un tuplu (X, Y).
%
% O mișcare este validă dacă tabla este goală sau dacă:
% - poziția este liberă;
% - poziția este adiacentă (are o muchie comună) cu o piesă deja
% existentă pe tablă;
% - piesa se potrivește cu toți vecinii deja existenți pe tablă.
%
% Hint: neighbor/3 și directions/1 , ambele din utils.pl

% daca tabla este goala, putem pune piesa pe tabla
% daca tabla nu este goala, verificam daca pozitia este libera,
% daca da, verificam daca are cel putin 1 vecin pe tabla, 
% daca da, verificam daca piesa nerotita se potriveste cu toti vecinii deja existenti pe tabla
canPlaceTile([], _, _) :- !.
canPlaceTile(Board, (X, Y), Tile) :-
    \+ member((X, Y, _), Board),
    getNeighbors(Board, (X, Y), Neighbors),
    length(Neighbors, Len),
    Len > 0,
    findRotation(Tile, Neighbors, Rotation),
    Rotation is 0.

getNeighbors(Board, (X, Y), Neighbors) :- findall((NeighborTile, NeighborDirection),
(member((XN, YN, NeighborTile), Board), neighbor((X, Y), NeighborDirection, (XN, YN))), 
Neighbors).

% getNeighbors imi leaga Neighbors la o lista de tupluri (NeighborTile, NeighborDirection)
% a vecinilor pozitiei (X,Y) de pe tabla Board (ma folosesc de predicatul neighbor din utils.pl)


%% TODO
% getAvailablePositions/2
% getAvailablePositions(+Board, -Positions)
%
% Predicatul leagă Positions la o listă de perechi (X, Y)
% a tuturor pozițiilor de pe tabla Board unde se pot pune piese (poziții
% libere vecine pe o muchie cu piese existente pe tablă).
%
% Pentru o tablă goală, predicatul eșuează.
%
% Hint: between/3 (predefinit) și neighbor/3 din utils.pl
%
% Atenție! Și în afara limitelor curente există poziții disponibile.

% Positions este o lista de perechi (X, Y) unice a tuturor pozitiilor de pe tabla Board
%getAvailablePositions([], _) :- false, !.
getAvailablePositions(Board, Positions) :-
    \+ emptyBoard(Board),
    findall((X, Y), (boardGetLimits(Board, XMin, YMin, XMax, YMax),
    X1 is XMin - 1, X2 is XMax + 1, Y1 is YMin - 1, Y2 is YMax + 1,
    between(X1, X2, X), between(Y1, Y2, Y),
    \+ member((X, Y, _), Board),
    getNeighbors(Board, (X, Y), Neighbors),
    length(Neighbors, Len), Len > 0), Positions).

% daca tabla este goala, nu exista pozitii disponibile
% daca tabla nu este goala, creez o lista cu toate pozitiile de pe tabla
% care sunt intre limitele curente ale tablei, care nu au fost deja ocupate,
% care au cel putin un vecin pe tabla


%% TODO
% findPositionForTile/4
% findPositionForTile(+Board, +Tile, -Position, -Rotation)
%
% Predicatul are ca soluții toate posibilele plasări pe tabla Board ale
% piesei Tile, legând Position la o pereche (X, Y) care reprezintă
% poziția și Rotation la un număr între 0 și 3 inclusiv, reprezentând de
% câte ori trebuie rotită piesa ca să se potrivească.
%
% Unele piese se pot potrivi cu mai multe rotații pe aceeași poziție și
% acestea reprezintă soluții diferite ale predicatului, dar numai dacă
% rotațiile duc la rezultate diferite.
%
% Dacă tabla este goală, predicatul leagă Position la (0, 0) și Rotation
% la 0.
%
% De exemplu, dacă pe tablă se află doar piesa 11, la vest de ea piesa 9
% se potrivește cu rotația 1 sau 2 - două soluții diferite. Pentru
% plasarea la vest de piesa 11 a piesei 16 însă există o singură soluție
% - rotație 0.
%
% În ieșirea de la teste, rezultatele vor fi asamblate ca
% (X,Y):Rotation.

findPositionForTile([], _, (0, 0), 0).
findPositionForTile(Board, Tile, Position, Rotation) :-
    getAvailablePositions(Board, Positions),
    member(Position, Positions),
    rotations(Tile, Rotations),
    member((Rotation, RotatedTile), Rotations),
    canPlaceTile(Board, Position, RotatedTile).

% luam toate pozitiile disponibile de pe tabla Board
% pentru fieacre pozitie disponibila, luam toate rotatiile piesei Tile diferite
% pentru fiecare rotatie, verificam daca piesa poate fi pusa pe tabla la pozitia respectiva



