module G = Graphics

type note =
  | Do
  | Re
  | Mi
  | Fa
  | Sol
  | La
  | Si

type pitch =
  { note : note
  ; octave : int
  }

type duration =
  | Half
  | Quarter

type symbol =
  | Note of pitch * duration
  | Rest of duration

type score =
  { symbols : symbol list
  ; tempo : int
  }

let frequency { note; octave } =
  let f0 =
    match note with
    | Do -> 33
    | Re -> 37
    | Mi -> 41
    | Fa -> 44
    | Sol -> 49
    | La -> 55
    | Si -> 62
  in
  f0 * truncate (2. ** float octave)
;;

let milliseconds duration tempo =
  let quarter = 60000 / tempo in
  match duration with
  | Half -> quarter * 2
  | Quarter -> quarter
;;

let sound tempo = function
  | Note (pitch, duration) ->
    let f = frequency pitch in
    G.sound f (milliseconds duration tempo)
  | Rest duration -> G.sound 0 (milliseconds duration tempo)
;;

let play_score { symbols; tempo } = List.iter (sound tempo) symbols

let () =
  G.open_graph " 800x600";
  (* The sound doesn't actually play on my system.
   * Maybe there's a bug with new harder, since this is an old library
   *)
  play_score
    { symbols =
        [ Note ({ note = Mi; octave = 1 }, Quarter)
        ; Rest Quarter
        ; Note ({ note = Sol; octave = 1 }, Half)
        ; Note ({ note = La; octave = 1 }, Quarter)
        ; Rest Half
        ; Note ({ note = Mi; octave = 1 }, Quarter)
        ]
    ; tempo = 120
    };
  read_line () |> ignore
;;
