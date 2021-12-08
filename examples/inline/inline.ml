(*
SWIPL-OCaml

Copyright (C) 2021  Kiran Gopinathan

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)

let () =
  Swipl.initialise ();
  Swipl.load_source {|
likes(sam, apple).    
likes(sam, pear).

likes_of(Person, Result) :-
    findall(X, likes(Person, X), Result).
|};
  let likes_of person result  = Swipl.Syntax.(app ("likes_of" /@ 2) [!person; result]) in
  let decode ctx t =
    Swipl.extract_list ctx t
    |> List.map (Swipl.extract_atom ctx)
    |> List.map Swipl.show_atom in
  let likes_of person =
    Swipl.with_ctx @@ fun ctx -> 
    let result = Swipl.fresh ctx in
    Swipl.call ctx (likes_of person result);
    decode ctx result in
  print_endline @@ "sam likes "  ^ (String.concat ", " (likes_of "sam"))
