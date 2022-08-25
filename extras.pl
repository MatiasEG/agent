:- module(extras,
	  [
	    append3/4
	  ]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% append3(?Lista1, ?List2, ?Lista3, ?Res)
%
% Concatena tres listas Xs, Ys y Zs.
% 
append3(Xs, Ys, Zs, XsYsZs) :-
	append(Xs, YsZs, XsYsZs),
	append(Ys, Zs, YsZs).