open Hibi

let recorded_at : string = "2026-10-22T00:00:00Z"
let score : int option = Some 3
let accomplishments : string option = None
let moments : string option = None
let next_step : string option = None

let entry : Entry.t =
  { recorded_at; score; accomplishments; moments; next_step }
