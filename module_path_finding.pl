:- module(path_finding,
	  [
	    buscar_plan_desplazamiento/4,
		raiz/1,
		padre/2,
		esMeta/1
	  ]).

:- use_module(module_beliefs_update, [node/5, at/3]).

:- dynamic padre/2, raiz/1, esMeta/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% eliminarPrimero(+Lista, +Elemento)
%
% Elimina el primer elemento de la lista.
%
eliminarPrimero([], []).
eliminarPrimero([_|Xs], Xs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% seleccionar(+Nodo, +Frontera, +FronteraSinNodo)
%
% Selecciona el primer nodo de la lista Frontera.
%
seleccionar(Nodo, [Nodo|RestoLista], RestoLista).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% encontrarCamino(+Meta, -Camino)
%
% Encuentra un camino a un nodo Meta.
% Usa las relaciones padre(Hijo, Padre) que se van agregando a la base de conocimiento
% cuando se agregan nuevos vecinos a la nueva frontera,
% en la busqueda de llegar de un nodo origen a uno destino.
%
encontrarCamino(Nodo, []):- raiz(Nodo), !.
encontrarCamino(Nodo, [P|Camino]):-
	padre(Nodo, P),
	encontrarCamino(P, Camino).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% crearPlan(+Camino, -Plan)
%
% Crea plan de movimientos para un camino elegido.
% Para cada nodo de un camino, crea una lista de acciones de movimiento avanzar(IdNodo)
% donde IdNodo es un identificador de un nodo.
% Camino es una lista conteniendo identificadores de nodos.
%
crearPlan([], []).
crearPlan(Camino, Plan):-
	findall(avanzar(Nodo), member(Nodo, Camino), Plan).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% buscar_plan_desplazamiento(+Metas, -Plan, -Destino, -Costo)
% Agregar todas las metas como hechos esMeta(idNodoMeta)
% Si tiene al menos una meta, pone el nodo actual del agente como raiz del árbol de búsqueda
% y busca el camino desde la posición del agente a un meta
% usando A* (buscarEstrella/5)
%

buscar_plan_desplazamiento(Metas, Plan, Destino, Costo):-
	forall(member(Meta, Metas), assert(esMeta(Meta))),
	at(MyNode, agente, me),
	length(Metas, CantMetas),
	CantMetas > 0,
	!,
	retractall(raiz(_)),
	retractall(padre(_,_)),
	assert(raiz(MyNode)),
	buscarEstrella([[MyNode, 0]], Metas, Camino, Costo, Destino),
	crearPlan(Camino, Plan).

buscar_plan_desplazamiento(_, [], [], 0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% buscarEstrella(+Frontera, +Metas, ?Camino, ?Costo, ?Destino)
%
% Busca el camino optimo desde la frontera hacia la meta mas cercana, utilizando la estrategia de busqueda A*.
%

buscarEstrella(Frontera, Metas, Camino, Costo, Destino):-
	findall(padre(P1, P2), padre(P1, P2), ListaPadres),
	write(ListaPadres), nl,
	buscar(Frontera, [], Metas, Destino),
	findall([X, Y], (padre(X, Y), padre(Y, X)), Bug),writeln(buggggg(Bug)),
	encontrarCamino(Destino, C),
	writeln('ENCONTRO CAMINOOOO'),
	append([Destino], C, C2),
	reverse(C2, C3),
	costoCamino(C3, Costo),
	eliminarPrimero(C3, Camino),
	retractall(esMeta(_)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% buscar(+Frontera, +Visitados, +Metas, -Destino)
%
% Busca el camino optimo desde la frontera hacia la Meta, utilizando la estrategia de busqueda A*.
% No devuelve el camino como un parametro, sino que agrega las relaciones padre(Hijo, Padre)
% que permita luego encontrar el camino y su costo.
%
% Caso 1: Si el nodo es meta, termina la búsqueda.
% Caso 2: Si el nodo no es meta
% Selecciono el primer nodo de la frontera,
% Genera los vecinos,
% Agregar nodo a visitados,
% Agregar vecinos a frontera, con los cuidados necesarios de A*
% y llama recursivmaente con la nueva frontera.

buscar(Frontera, _, M, Nodo):-
	seleccionar([Nodo, _], Frontera, _),
	esMeta(Nodo),
	writeln(meta(Nodo)),
	writeln(metas(M)),
	!.

buscar(Frontera, Visitados, Metas, MM):-
	seleccionar(Nodo, Frontera, FronteraSinNodo), % selecciona primer nodo de la frontera
	generarVecinos(Nodo, Vecinos), % genera los vecinos del nodo - TO-DO
	agregarAVisitados(Nodo, Visitados, VisitadosIntermedio), % agrega el nodo a lista de visitados
	agregar(FronteraSinNodo, Vecinos, NuevaFrontera, VisitadosIntermedio, Nodo, Metas,VisitadosNuevo), % agrega vecinos a la frontera - TO-DO
	buscar(NuevaFrontera, VisitadosNuevo, Metas, MM). % continua la busqueda con la nueva frontera

generarVecinos([Id, Costo], NuevosVecinos):-
	node(Id,_,_,_,Conexiones),
	findall([IdV, CostoTotal], (member([IdV, CostoV],Conexiones),node(IdV,_,_,CostoV,_), CostoTotal is Costo + CostoV), NuevosVecinos).

agregar(Frontera,Vecinos,NuevaFronterOrdenada,Visitados,Nodo,Metas, VisitadosNuevo):-
	filtrarVecinos(Vecinos, Visitados, VecinosNoVisitados, VisitadosNuevo),
	insertarVecinos(VecinosNoVisitados, Frontera, Nodo, NuevaFrontera),
	ordenarPorH(NuevaFrontera, Metas, NuevaFronterOrdenada).

filtrarVecinos([Vecino|RestoVecinos], Visitados, VecinosNuevo, VisitadosNuevo):-
    filtrarUnVecino(Vecino, Visitados, VecinoFiltrado, VisitadosIntermedio),
    filtrarVecinos(RestoVecinos, VisitadosIntermedio, RestoVecinosFiltrados, VisitadosNuevo),
    append(VecinoFiltrado, RestoVecinosFiltrados, VecinosNuevo).
filtrarVecinos([], Visitados, [], Visitados).

filtrarUnVecino(Vecino, [Visitado|RestoVisitados], VecinoFiltrado, VisitadosNuevo):-
    Vecino = [Id, _],
    Visitado = [Id, _], !,
    compararYFiltrar(Vecino, Visitado, VecinoFiltrado, VisitadoFiltrado),
    append(VisitadoFiltrado, RestoVisitados, VisitadosNuevo).
filtrarUnVecino(Vecino, [Visitado|RestoVisitados], VecinoFiltrado, [Visitado|RestoVisitadosNuevo]):-
    filtrarUnVecino(Vecino, RestoVisitados, VecinoFiltrado, RestoVisitadosNuevo).
filtrarUnVecino(Vecino, [], [Vecino], []).

compararYFiltrar(Vecino, Visitado, [Vecino], []):-
    Vecino = [Id, CostoVecino],
    Visitado = [Id, CostoVisitado],
    CostoVecino < CostoVisitado, !, retractall(padre(Id,_)).
compararYFiltrar(_, Visitado, [], [Visitado]).

insertarVecinos([Vecino|RestoVecinos], Frontera, Padre, FronteraResultado):-
    insertarUnVecino(Vecino, Frontera, Padre, FronteraIntermedia),
    insertarVecinos(RestoVecinos, FronteraIntermedia, Padre, FronteraResultado).
insertarVecinos([], Frontera, _Padre, Frontera).

insertarUnVecino(Vecino, [], Padre, [Vecino]):-
    Vecino = [Id, _CostoVecino],
    Padre = [IdPadre, _CostoPadre],
    assert(padre(Id, IdPadre)).
insertarUnVecino(Vecino, [Nodo|RestoFrontera], Padre, [Vecino|RestoFrontera]):-
    Vecino = [Id, CostoVecino],
    Nodo = [Id, CostoNodo],
    Padre = [IdPadre, _CostoPadre],
    CostoVecino < CostoNodo, !, retractall(padre(Id,_)), assert(padre(Id, IdPadre)).
insertarUnVecino(Vecino, [Nodo|RestoFrontera], _Padre, [Nodo|RestoFrontera]):-
    Vecino = [Id, _CostoVecino],
    Nodo = [Id, _CostoNodo].
insertarUnVecino(Vecino, [Nodo|Frontera], Padre, [Nodo|FronteraNueva]):-
    insertarUnVecino(Vecino, Frontera, Padre, FronteraNueva).

ordenarPorH(Frontera, Metas, FronteraOrdenada):- quicksort(Frontera, Metas, FronteraOrdenada).

quicksort([X|Xs], Metas, Ys) :-
  partition(Xs,X,Left,Right, Metas),
  quicksort(Left,Metas, Ls),
  quicksort(Right,Metas, Rs),
  append(Ls,[X|Rs],Ys).
quicksort([],_Metas, []).

partition([X|Xs],Y,[X|Ls],Rs, Metas) :-
    calcularF(X, Metas, FX),
    calcularF(Y, Metas, FY),
    FX =< FY, !,
    partition(Xs,Y,Ls,Rs, Metas).
partition([X|Xs],Y,Ls,[X|Rs], Metas) :-
    partition(Xs,Y,Ls,Rs, Metas).
partition([],_Y,[],[], _Metas).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% agregarAVisitados(+Nodo, +Visitados, ?VisitadosConNodo)
%
% Agrega un nodo a la lista de visitados.
%
agregarAVisitados(Nodo, Visitados, [Nodo | Visitados]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% costoCamino(+Lista, ?Costo)
%
% Calcula el costo del camino,
% como la sumatoria de los costos de los nodos que forma el camino.
% Lista es una lista conteniendo identificadores de nodos, representando el camino.
%
costoCamino([], 0).

costoCamino([X|Xs], R):-
	node(X, _, _, CostoNodo, _),
	costoCamino(Xs, CostoResto),
	R is CostoNodo + CostoResto.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% calcularH(+Nodo, ?Resultado, +Meta)
%
% Calcula el valor de la heurística para el nodo Nodo a una Meta.
% La heurística es la distancia euclidea.
%
calcularH(Nodo, Meta, Resultado):-
	node(Meta, X2, Y2, _, _),
	node(Nodo, X1, Y1, _, _),
	distance([X1, Y1], [X2, Y2], Resultado).

calcularF(Nodo, Metas, Resultado):-
	Nodo = [Id, Costo],
	findall(Distancia, (member(Meta, Metas), calcularH(Id,Meta,Distancia)), Distancias),
	minDistancia(Distancias, ResultadoH),
	Resultado is ResultadoH + Costo.

minDistancia([X], X):- !.
minDistancia([X|Xs], Min):- minDistancia(Xs, MinXs), Min is min(X, MinXs).

distance([X1, Y1], [X2, Y2], Distance):-
	DX is X2 - X1,
	DY is Y2 - Y1,
	Distance is sqrt(DX^2 + DY^2).
