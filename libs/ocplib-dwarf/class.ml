(* Attribute value form classes express the permissible values for a given attribute *)
(* See Section 2.2 pg 21 with Figure 3 pg 29 *)

type form_class = [ `address | `flag | `constant | `block
                  | `string | `reference | `exprloc | `indirect
                  | `lineptr | `macptr | `rangelistptr | `loclistptr | `ptr ]
