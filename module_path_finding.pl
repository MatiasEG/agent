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
	buscar(Frontera, [], Metas, Destino),
	findall([X, Y], (padre(X, Y), padre(Y, X)), Bug), write('----------------BUG-------------'), writeln(Bug),
	encontrarCamino(Destino, C),
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
	agregarAVisitados(Nodo, Visitados, NuevosVisitados), % agrega el nodo a lista de visitados
	agregar(FronteraSinNodo, Vecinos, NuevaFrontera, NuevosVisitados, Nodo, Metas), % agrega vecinos a la frontera - TO-DO
%	writeln(frontera(Frontera)),
%	writeln(nodo(Nodo)),
%	writeln(fronterasinnodo(FronteraSinNodo)),
%	writeln(nuevafrontera(NuevaFrontera)),
	writeln(''),!,
	buscar(NuevaFrontera, NuevosVisitados, Metas, MM). % continua la busqueda con la nueva frontera

generarVecinos([Id, Costo], NuevosVecinos):-
	node(Id,_,_,_,Conexiones),
	findall([IdV, CostoTotal], (member([IdV, CostoV],Conexiones),node(IdV,_,_,CostoV,_), CostoTotal is Costo + CostoV), NuevosVecinos).

agregar(Frontera,Vecinos,NuevaFronterOrdenada,Visitados,Nodo,Metas):-
	filtrarVisistados(Vecinos, Visitados, VecinosNoVisitados),
%	writeln(vecinos(Vecinos)),
%	writeln(visitados(Visitados)),
%	writeln(novisitados(VecinosNoVisitados)),
	agregarFrontera(VecinosNoVisitados, Frontera, Nodo, NuevaFrontera),
	bubbleSort(NuevaFrontera, Metas, NuevaFronterOrdenada).

filtrarVisistados([Vecino|Vecinos], Visitados, VecinosFiltrados):-
    pertenece(Vecino, Visitados), !, filtrarVisistados(Vecinos, Visitados, VecinosFiltrados).
filtrarVisistados([Vecino|Vecinos], Visitados, [Vecino|VecinosFiltrados]):-
    filtrarVisistados(Vecinos, Visitados, VecinosFiltrados).
filtrarVisistados([], _Visitados, []).

pertenece(_, []):- false.
pertenece(Vecino, [Visitado|_Visitados]):- Vecino = [Id, _], Visitado = [Id, _].
pertenece(Vecino, [_Visitado|Visitados]):- pertenece(Vecino, Visitados).

agregarFrontera([], Frontera, _Nodo, Frontera).
agregarFrontera([Vecino|Vecinos], Frontera, Nodo, NuevaFrontera):-
	agregarVecinoMenorCosto(Vecino, Frontera, Nodo, FronteraIntermedia),
	agregarFrontera(Vecinos, FronteraIntermedia, Nodo,NuevaFrontera).

agregarVecinoMenorCosto(Vecino, [], Padre, [Vecino]):-
	Vecino = [Id, _CostoV],
	Padre = [IdPadre, _CostoPadre],
	agregarPadre(Id, IdPadre).
agregarVecinoMenorCosto(Vecino, [Nodo|Frontera], Padre, [Vecino|Frontera]):-
	Vecino = [Id, CostoV],
	Nodo = [Id, CostoN],
	Padre = [IdPadre, _CostoPadre],
	CostoV =< CostoN,
	retractall(padre(Id,_)),
	agregarPadre(Id, IdPadre).
agregarVecinoMenorCosto(Vecino, [Nodo|Frontera], _Padre, [Nodo|Frontera]):-
	Vecino = [Id, CostoV],
	Nodo = [Id, CostoN],
	CostoV > CostoN.
agregarVecinoMenorCosto(Vecino, [Nodo|Frontera], Padre, [Nodo|NuevaFrontera]):- agregarVecinoMenorCosto(Vecino, Frontera, Padre, NuevaFrontera).

agregarPadre(Hijo, Padre):- \+ padre(Padre, Hijo), !, assert(padre(Hijo, Padre)).
agregarPadre(Hijo, Padre).

bubbleSort( List, Metas, SortedList):-
    swap( List, Metas, List1 ), ! ,
    bubbleSort( List1, Metas, SortedList).
bubbleSort(List, _Metas, List).

swap( [ NodoX, NodoY | Rest ], Metas, [ NodoY, NodoX | Rest ] ) :-
    calcularF(NodoX,Metas,ResultadoX),
    calcularF(NodoY,Metas,ResultadoY),
    ResultadoX > ResultadoY, ! .
swap( [ Nodo | Rest ], Metas, [ Nodo | Rest1 ] ) :-
    swap(Rest, Metas, Rest1 ).

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
