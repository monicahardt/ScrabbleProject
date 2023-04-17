// Insert your StateMonad.fs from Assignment 6 here. All modules must be internal.
module internal StateMonad

    type Error = 
        | VarExists of string
        | VarNotFound of string
        | IndexOutOfBounds of int
        | DivisionByZero 
        | ReservedName of string           

    type Result<'a, 'b>  =
        | Success of 'a
        | Failure of 'b

    type State = { vars     : Map<string, int> list
                   word     : (char * int) list 
                   reserved : Set<string> }

    type SM<'a> = S of (State -> Result<'a * State, Error>)

    let mkState lst word reserved = 
           { vars = [Map.ofList lst];
             word = word;
             reserved = Set.ofList reserved }

    let evalSM (s : State) (S a : SM<'a>) : Result<'a, Error> =
        match a s with
        | Success (result, _) -> Success result
        | Failure error -> Failure error

    let bind (f : 'a -> SM<'b>) (S a : SM<'a>) : SM<'b> =
        S (fun s ->
              match a s with
              | Success (b, s') -> 
                match f b with 
                | S g -> g s'
              | Failure err     -> Failure err)

    let ret (v : 'a) : SM<'a> = S (fun s -> Success (v, s))
    let fail err     : SM<'a> = S (fun s -> Failure err)

    let (>>=)  x f = bind f x
    let (>>>=) x f = x >>= (fun () -> f)

    let push : SM<unit> = 
        S (fun s -> Success ((), {s with vars = Map.empty :: s.vars}))

    let pop : SM<unit> = 
         S (fun s -> Success ((), {s with vars = s.vars.Tail}))

    let wordLength : SM<int> =
        S (fun s -> Success (s.word.Length, s))

    let characterValue (pos : int) : SM<char> =
        S (fun s ->
        match pos with
        |pos when pos < s.word.Length -> Success (fst s.word.[pos],s)
        |_ -> Failure (IndexOutOfBounds pos))

    let pointValue (pos : int) : SM<int> =
        S (fun s ->
        match pos with
        |pos when pos < s.word.Length -> Success (snd s.word.[pos],s)
        |_ -> Failure (IndexOutOfBounds pos))

    let lookup (x : string) : SM<int> = 
        let rec aux =
            function
            | []      -> None
            | m :: ms -> 
                match Map.tryFind x m with
                | Some v -> Some v
                | None   -> aux ms

        S (fun s -> 
              match aux (s.vars) with
              | Some v -> Success (v, s)
              | None   -> Failure (VarNotFound x))

    let declare (x : string) : SM<unit> =
        S (fun s ->
        match x with
        |_ when s.vars.IsEmpty -> Failure (VarNotFound "The variable stack is empty")
        |x when s.reserved.Contains x -> Failure (ReservedName x)
        |_ when s.vars.[0].ContainsKey x -> Failure (VarExists x)
        |_ -> Success((), {s with vars = Map [x,0] :: s.vars}))
        
        
    let update (x: string) (v:int) : SM<unit> =
        let rec aux (sVars: Map<string,int> list) (index: int) =
            match sVars with
            |[] -> None
            |m :: ms ->
                match Map.tryFind x m with //check if the string is there
                |Some _ -> Some index //the string was found. We pass the index at which the string was on to the next function. This is the ind in the below function
                |None -> aux ms (index+1) //the string was not found try the next index
        S (fun s ->
            match aux s.vars 0 with
            | Some ind -> //the ind here is the index found in the auxillary function
                let updated = List.mapi (fun i (m:Map<string,int>) -> if i = ind then m.Add(x,v) else m) s.vars
                Success ((),{s with vars = updated}) //updating the state
            | None   -> Failure (VarNotFound x)) //fail if the string was not at all in the state
 