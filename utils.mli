val remdups : 'a list -> 'a list
val union : 'a list -> 'a list -> 'a list
val distunion : 'a list list -> 'a list
val ( -- ) : 'a list -> 'a list -> 'a list
val filter : ('a -> bool) -> 'a list -> 'a list
val intersection : 'a list -> 'a list -> 'a list
val echo : bool -> unit
val fileuse : (string -> unit) ref
val usefile : string -> unit
val stripslosh : string -> string
val declareNotations: string -> ((string * string * string list) list -> unit)
val setDeclareNotations: ((string * string * string list) list -> unit) -> unit





