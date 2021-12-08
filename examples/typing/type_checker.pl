% SWIPL-OCaml

% Copyright (C) 2021  Kiran Gopinathan

% This program is free software: you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation, either version 3 of the License, or
% (at your option) any later version.

% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.

% You should have received a copy of the GNU General Public License
% along with this program.  If not, see <http://www.gnu.org/licenses/>.


typeof(Gamma, Term, Type) :-
        atom(Term),
        member(type(Term, Tvar), Gamma),
        !,
        unify_with_occurs_check(Type, Tvar).

typeof(Gamma, lam(A,B), Type) :-
        atom(A),
        typeof([type(A, TvarA)|Gamma], B, TvarB),
        unify_with_occurs_check(Type, fun(TvarA, TvarB)).

typeof(Gamma, app(A,B), Type) :-
        typeof(Gamma, A, TvarA),
        unify_with_occurs_check(TvarA, fun(TvarIn, TvarOut)),
        typeof(Gamma, B, TvarIn),
        unify_with_occurs_check(Type, TvarOut).
