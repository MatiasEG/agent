
:- use_module(module_beliefs_update, [
	update_beliefs/1,
	time/1,
	node/5,
	at/3,
	direction/1
]).

:- use_module(module_path_finding, [
	buscar_plan_desplazamiento/4,
	raiz/1,
	padre/2,
	esMeta/1
]).

:- use_module(extras, [
	append3/4
]).

:- dynamic plandesplazamiento/1, avanzo_random/1, dest/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% run(+Perc, -Action, -Text)
%
% El predicado run/3 implementa el comportamiento del agente.
%
% Es ejecutado automáticamente por el juego una vez por ciclo.
% Implementa la interface con el mundo virtual,
% recibiendo la información que el agente puede percibir del entorno
% y permite hacer que el agente actue en el mundo virtual
% No pueden cambiarse los 3 parámetros, pero si el cuerpo.
%
% El primer parámetro (Perc) recibe una lista con la percepción del agente.
% Los otros parámetros envían información al juego.
% La idea es que el agente actualice su estado interno usando Perc,
% y luego decida que acción hacer instanciándola en el parámetro Action.
% Text es un texto con comillas simples como 'hola' que será mostrado en la pantalla del juego.
%
% El parámetro Perc recibe una lista con el siguiente formato: [N1,...,Nk,A1,...Ap,Time,Dir]
% donde:
% N1 a Nk son k elementos (k>0) de la forma node(Id, PosX, PosY, Costo, Conexiones),
% A1 a Ap son p elementos (p>0) de la forma at(IdNodo, TipoEntidad, IdEntidad),
% Time es el functor time(T), donde T es el tiempo actual (descendente) de la partida.
% Dir es el functor direction(D), donde D ∈ {w, s, a, d}.
% Action debe ser un functor totalmente instanciado (sin variables) que corresponda a una acción valida
%
% Si Action tiene algún error el agente pierde el ciclo de ejecución

run(Perc, Action, Text):-
    update_beliefs(Perc), % implementado en module_beliefs_update
    print_beliefs,
    decide_action(Action, Text).

print_beliefs:- writeln('\n\n---------------------------------------------------------------------------'),
				forall(at(IdNode, TipoEntidad, IdEntidad), (node(IdNode, X, Y, _, _), writeln(at(IdNode, TipoEntidad, IdEntidad, X, Y)))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TO-DO
%
% decide_action(-Action, -Text)
%
% Decide la acción a realizar por el agente, instanciándola en el parámetro Action.
% Text es un texto con comillas simples como 'hola' que será mostrado en la pantalla del juego.
%
% En la implementación siguiente:
% El primer caso (1), sirve de ejemplo para levantar un objeto del terreno.
% El segundo caso (2) del predicado siempre tiene éxito, y hace que el agente se mueva de manera aleatoria.
% El tercer caso (3) permite al agente ejecutar la siguiente acción de movimiento de un plan guardado, calculado previamente.
% El cuarto caso (4) permite obtener un nuevo plan de movimiento usando A*.
% El quinto caso (5) hace que el agente gire en su posición, en sentido horario.
%
% Deberán completar la implementación del algoritmo de búsqueda A* para que funcionen los casos (3) y (4)
% Y eliminar el caso (2), para permitir al agente seguir el plan de movimientos.
%
% Pueden realizar todos los cambios de implementación que consideren necesarios.
% Esta implementación busca ser un marco para facilitar la resolución del proyecto.

% Si estoy en la misma posición que una copa, intento levantarla.
decide_action(Action, 'Quiero levantar una copa...'):-
    at(MyNode, agente, me),
    at(MyNode, copa, IdGold),
    node(MyNode, PosX, PosY, _, _),
    !,
    Action = levantar_tesoro(IdGold, PosX, PosY),
    retractall(at(MyNode, _, IdGold)),
	retractall(plandesplazamiento(_)),
	writeln(accion('LEVANTAR COPA')).

% Si estoy en la misma posición que un cofre, intento levantarlo.
decide_action(Action, 'Quiero levantar un cofre...'):-
    at(MyNode, agente, me),
    at(MyNode, cofre, IdGold),
    node(MyNode, PosX, PosY, _, _),
    !,
    Action = levantar_tesoro(IdGold, PosX, PosY),
    retractall(at(MyNode, _, IdGold)),
	retractall(plandesplazamiento(_)),
	writeln(accion('LEVANTAR COFRE')).

% Si estoy en la misma posición que un reloj, intento levantarlo.
decide_action(Action, 'Quiero levantar un reloj...'):-
    at(MyNode, agente, me),
    at(MyNode, reloj, IdGold),
    node(MyNode, PosX, PosY, _, _),
    !,
    Action = levantar_reloj(IdGold, PosX, PosY),
    retractall(at(MyNode, _, IdGold)),
	retractall(plandesplazamiento(_)),
	writeln(accion('LEVANTAR RELOJ')).

% Si estoy en la misma posición que una pocion, intento levantarla.
decide_action(Action, 'Quiero levantar una pocion...'):-
    at(MyNode, agente, me),
    at(MyNode, pocion, IdGold),
    node(MyNode, PosX, PosY, _, _),
    !,
    Action = levantar_pocion(IdGold, PosX, PosY),
    retractall(at(MyNode, _, IdGold)),
	retractall(plandesplazamiento(_)),
	writeln(accion('LEVANTAR POCION')).

% Si tengo un plan de movimientos, ejecuto la siguiente acción.
decide_action(Action, 'Avanzar...'):-
	plandesplazamiento(Plan),
	length(Plan, LargoPlan),
	LargoPlan > 0,
        checkDestino(),
	!,
	obtenerMovimiento(Plan, Destino, Resto),
	retractall(plandesplazamiento(_)),
	assert(plandesplazamiento(Resto)),
	Action = Destino,
	writeln(accion('AVANZAR', Destino)).

% Si no tengo un plan guardado, busco uno nuevo.
decide_action(Action, 'Avanzar con nuevo plan...'):-
    retractall(dest(_)),
    busqueda_plan(Plan, Destino, _Costo),
	Plan \= [],
        assert(dest(Destino)),
	!,
	assert(plandesplazamiento(Plan)),
    mirarAdestino(Destino,Action),
	writeln(accion('AVANZAR CON PLAN', Action, Plan)).

% Me muevo a una posición vecina seleccionada de manera aleatoria, repetir hasta 3 veces antes de girar.
decide_action(Action, 'Me muevo a la posicion de al lado...'):-
	avanzo_random(Cant),
	Cant < 3,
	NewCant is Cant + 1,
    retractall(avanzo_random(_)),
    assert(avanzo_random(NewCant)),
    at(MyNode, agente, me),
	node(MyNode, _, _, _, AdyList),
	length(AdyList, LenAdyList),
	LenAdyList > 0,
	random_member([IdAdyNode, _CostAdyNode], AdyList),
	!,
	Action = avanzar(IdAdyNode),
	writeln(accion('AVANZAR ALEATORIO', Action)).

% Giro 180°, para conocer mas terreno.
decide_action(Action, 'Girar para conocer el territorio...'):-
	(
		direction(w)
		-> Action = girar(s)
		; ( direction(d)
			-> Action = girar(a)
			; ( direction(s)
				-> Action = girar(w)
				; Action = girar(d)
				)
			)
	),
    retractall(avanzo_random(_)),
    assert(avanzo_random(0)),
	writeln(accion('GIRAR')).

checkDestino():-
    dest(Id),
    at(Id,_,_).
checkDestino():-
    dest(Id),
    \+at(Id,_,_),
    retractall(plandesplazamiento(_)),
    false.



% Obtiene la acción de girar necesaria para que el agente mire hacia el nodo destino
mirarAdestino(Destino,Action):-
    node(Destino,DestX,DestY, _, _),
    at(MyNode, agente, me),
    node(MyNode, MeX, MeY, _, _),
    obtenerDireccion(DestX, DestY, MeX, MeY, Action).

obtenerDireccion(DestX, DestY, MeX, MeY, Action):-
    abs(DestX - MeX) < abs(DestY - MeY),
    MeY < DestY, !,
    Action = girar(d).

obtenerDireccion(DestX, DestY, MeX, MeY, Action):-
    abs(DestX - MeX) < abs(DestY - MeY),
    MeY >= DestY, !,
    Action = girar(a).

obtenerDireccion(DestX, DestY, MeX, MeY, Action):-
    abs(DestX - MeX) >= abs(DestY - MeY),
    MeX < DestX, !,
    Action = girar(s).

obtenerDireccion(_DestX, _DestY, _MeX, _MeY, Action):-
    Action = girar(w).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% obtenerMovimiento(?Lista, ?Movimiento, ?Resto)
%
% Obtiene el primer movimiento de una lista de movimientos.
%
obtenerMovimiento([], [], []).
obtenerMovimiento([X|Xs], X, Xs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% busqueda_plan(-Plan, -Destino, -Costo)
%
% Busca un plan de desplazamiento hacia el tesoro que se encuentre mas cerca.
%
busqueda_plan(Plan, Destino, Costo):-
	retractall(plandesplazamiento(_)),
	retractall(esMeta(_)),
	findall(Nodo, (at(Nodo, Tipo, _), Tipo \== agente), Metas), % nuevas metas

	buscar_plan_desplazamiento(Metas, Plan, Destino, Costo). % implementado en module_path_finding
