:- module(beliefs_update,
	  [
	    update_beliefs/1,
	    time/1,
	    node/5,
	    at/3,
	    padre/2,
	    direction/1
	  ]).

:- dynamic time/1, node/5, at/3, direction/1, padre/2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TO-DO
%
% update_beliefs(+Perc)
%
% IMPORTANTE: Debe exportarse todo predicado dinámico (creencia)
% manipulado por la actualización de creencias, para que puedan ser
% consultado por el resto del código del agente.
%
% El parámetro Perc recibe una lista con el siguiente formato: [N1,...,Nk,A1,...Ap,Time,Dir]
% donde:
% N1 a Nk son k elementos (k>0) de la forma node(Id, PosX, PosY, Costo, Conexiones),
% A1 a Ap son p elementos (p>0) de la forma at(IdNodo, TipoEntidad, IdEntidad),
% Time es el functor time(T), donde T es el tiempo actual (descendente) de la partida.
% Dir es el functor direction(D), donde D ∈ {w, s, a, d}.
%
% Este agente básico, al recibir una nueva percepcion olvida todo lo que tenía guardado en su estado interno
% Y almance la información de la percepción nueva.
%
% Pueden realizar todos los cambios de implementación que consideren necesarios.
% Esta implementación busca ser un marco para facilitar la resolución del proyecto.

update_beliefs(Perc):-
	retractall(time(_)),
	retractall(direction(_)),
	retractall(at(_, agente, me)),
	forall(at(Id, TipoEntidad, IdEntidad), check_entity(at(Id, TipoEntidad, IdEntidad), node(Id,_,_,_,_), Perc)),
	forall(member(Rel, Perc), add(Rel)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% controla que las entidades ubicadas en el nodo sigan estando en la nueva percepcion
check_entity(_At, Node, Perc):- \+member(Node, Perc), !.
check_entity(At, Node, Perc):- member(Node, Perc), member(At, Perc), !.
check_entity(at(IdNodo, TipoEntidad, IdEntidad), _Node, _Perc):- retract(at(IdNodo, TipoEntidad, IdEntidad)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% agrega X si no existe actualmente
add(X):- X, !.
add(X):- assert(X).
