open Hibi

let score : int option = Some 4
let update (entry : Entry.t) = { entry with score }
