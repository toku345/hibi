open Hibi

let create ~recorded_at =
  Entry.create ~recorded_at ~tz_offset_s:0 ~score:(Some 3) ~accomplishments:None
    ~moments:None ~next_step:None

let read (entry : Entry.t) :
    string * int option * string option * string option * string option =
  ( entry.recorded_at,
    entry.score,
    entry.accomplishments,
    entry.moments,
    entry.next_step )
