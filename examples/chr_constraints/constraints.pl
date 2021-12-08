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

:- use_module(library(chr)).

:- chr_constraint salt/0, water/0, salt_water/0.

salt, water <=> salt_water.

reducesTo_(Goal, C) :-
        call(Goal),
        call(user:'$enumerate_constraints'(C)).
reducesTo(Goal, Constraints) :-
        findall(Constraint, reducesTo_(Goal, Constraint), Constraints).