:- module(path_finding,
	  [
	    buscar_plan_desplazamiento/4,
		raiz/1,
		padre/2,
		esMeta/1
	  ]).

:- use_module(module_beliefs_update, [node/5, at/3]).

:- dynamic padre/2, raiz/1, esMeta/1, h/2.

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
crearPlan([], []):- !.
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
	retractall(h(_,_)),
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

buscar(Frontera, _, _M, Nodo):-
	seleccionar([Nodo, _], Frontera, _),
	esMeta(Nodo), !.
%	writeln(metas(M))
%	writeln(metaEncontrada(Nodo))

buscar(Frontera, Visitados, Metas, MM):-
	seleccionar(Nodo, Frontera, FronteraSinNodo), % selecciona primer nodo de la frontera
	generarVecinos(Nodo, Vecinos), % genera los vecinos del nodo
	agregarAVisitados(Nodo, Visitados, VisitadosIntermedio), % agrega el nodo a lista de visitados
	agregar(FronteraSinNodo, Vecinos, NuevaFrontera, VisitadosIntermedio, Nodo, Metas,VisitadosNuevo), % agrega vecinos a la frontera - TO-DO
	buscar(NuevaFrontera, VisitadosNuevo, Metas, MM). % continua la busqueda con la nueva frontera

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% generarVecinos(+Nodo, -Vecinos)
% Genera una lista de pares [IdVecino, CostoVecino] con el Id de los nodos vecinos y el costo total de llegar hasta ellos
generarVecinos([Id, Costo], NuevosVecinos):-
	node(Id,_,_,_,Conexiones),
	findall([IdV, CostoTotal], (member([IdV, CostoV],Conexiones),node(IdV,_,_,CostoV,_), CostoTotal is Costo + CostoV), NuevosVecinos).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% agregar(+Frontera, +Vecinos, -NuevaFronteraOrdenada, +Visitados, +Nodo, +Metas, -VisitadosNuevo)
% Agrega a la frontera los nuevos vecinos, considerando si ya fueron visitados (ya sea por un mejor o peor camino)
agregar(Frontera,Vecinos,NuevaFronterOrdenada,Visitados,Nodo,Metas, VisitadosNuevo):-
	filtrarVecinos(Vecinos, Visitados, VecinosNoVisitados, VisitadosNuevo),
	insertarVecinos(VecinosNoVisitados, Frontera, Nodo, NuevaFrontera),
	ordenarPorF(NuevaFrontera, Metas, NuevaFronterOrdenada).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% filtrarVecinos(+Vecinos, +Visitados, -VecinosNuevo, -VisitadosNuevo)
% Filtra los vecinos que ya hayan sido visitados por un mejor camino, quita de visitados a los vecinos que hayan sido visitados por un peor camino
filtrarVecinos([Vecino|RestoVecinos], Visitados, VecinosNuevo, VisitadosNuevo):-
    filtrarUnVecino(Vecino, Visitados, VecinoFiltrado, VisitadosIntermedio),
    filtrarVecinos(RestoVecinos, VisitadosIntermedio, RestoVecinosFiltrados, VisitadosNuevo),
    append(VecinoFiltrado, RestoVecinosFiltrados, VecinosNuevo).
filtrarVecinos([], Visitados, [], Visitados).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% filtrarUnVecino(+Vecino, +Visitados, -VecinoFiltrado, -VisitadosNuevo)
% Compara un vecino con la lista de visitados
% Si el vecino fue visitado por un mejor camino VecinoFiltrado = []
% Si el vecino fue visitado por un peor camino VecinoFiltrado = [Vecino] y es eliminado de la lista de visitados
% Sino VecinoFiltrado = [Vecino] y la lista de visitados no es alterada
filtrarUnVecino(Vecino, [Visitado|RestoVisitados], VecinoFiltrado, VisitadosNuevo):-
    Vecino = [Id, _],
    Visitado = [Id, _], !,
    compararYFiltrar(Vecino, Visitado, VecinoFiltrado, VisitadoFiltrado),
    append(VisitadoFiltrado, RestoVisitados, VisitadosNuevo).
filtrarUnVecino(Vecino, [Visitado|RestoVisitados], VecinoFiltrado, [Visitado|RestoVisitadosNuevo]):-
    filtrarUnVecino(Vecino, RestoVisitados, VecinoFiltrado, RestoVisitadosNuevo).
filtrarUnVecino(Vecino, [], [Vecino], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% compararYFiltrar(+Vecino, +Visitado, -VecinoFiltrado, -VisitadoFiltrado)
% Compara un vecino que ya fue visitado
% Si fue visitado por un peor camino VecinoFiltrado = [Vecino] y VisitadoFiltrado = []
% Si fue visitado por un mejor camino VecinoFiltrado = [] y VisitadoFiltrado = [Visitado]
compararYFiltrar(Vecino, Visitado, [Vecino], []):-
    Vecino = [Id, CostoVecino],
    Visitado = [Id, CostoVisitado],
    CostoVecino < CostoVisitado, !, retractall(padre(Id,_)).
compararYFiltrar(_, Visitado, [], [Visitado]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% insertarVecinos(+Vecinos, +Frontera, +Padre, -FronteraResultado)
% Agrega la lista de vecinos a la frontera, considerando los casos en que el vecino ya este en la frontera (ya sea por un mejor o peor camino)
insertarVecinos([Vecino|RestoVecinos], Frontera, Padre, FronteraResultado):-
    insertarUnVecino(Vecino, Frontera, Padre, FronteraIntermedia),
    insertarVecinos(RestoVecinos, FronteraIntermedia, Padre, FronteraResultado).
insertarVecinos([], Frontera, _Padre, Frontera).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% insertarUnVecino(+Vecino, +Frontera, +Padre, -FronteraNueva)
% Agrega un vecino a la frontera si no fue visitado por un mejor camino
% Si fue visitado por un peor camino elimina la instancia previa de la frontera
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
    Nodo = [Id, _CostoNodo], !.
insertarUnVecino(Vecino, [Nodo|Frontera], Padre, [Nodo|FronteraNueva]):-
    insertarUnVecino(Vecino, Frontera, Padre, FronteraNueva).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% ordenarPorF(+Fontera, +Metas, -FronteraOrdenada)
% Ordena la frontera en funcion del valor de f(N) para cada nodo
ordenarPorF(Frontera, Metas, FronteraOrdenada):- quicksort(Frontera, Metas, FronteraOrdenada).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Algoritmo de reordenamiento utilizado para la frontera
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
% calcularH(+Nodo, +Meta, ?Resultado)
%
% Calcula el valor de la heurística para el nodo Nodo a una Meta.
% La heurística es la máxima diferencia sobre uno de los dos ejes
calcularH(Nodo, Meta, Resultado):-
	node(Meta, X2, Y2, _, _),
	node(Nodo, X1, Y1, _, _),
	Resultado is max(abs(X1-X2), abs(Y1-Y2)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% calcularF(+Nodo, +Metas, -Resultado)
% Calcula el valor de la función f(N) = g(N) + h(N)
calcularF(Nodo, _Metas, Resultado):-
	Nodo = [Id, Costo],
	h(Id, ResultadoH), !,
	Resultado is ResultadoH + Costo.
calcularF(Nodo, Metas, Resultado):-
	Nodo = [Id, Costo],
	findall(Distancia, (member(Meta, Metas), calcularH(Id,Meta,Distancia)), Distancias),
	minDistancia(Distancias, ResultadoH),
	assert(h(Id, ResultadoH)),
	Resultado is ResultadoH + Costo.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Obtiene el nuemro menor en una lista de numeros, este representa la distancia heuristica a la meta mas cercana.
minDistancia([X], X):- !.
minDistancia([X|Xs], Min):- minDistancia(Xs, MinXs), Min is min(X, MinXs).
