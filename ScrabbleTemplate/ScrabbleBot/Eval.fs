// Insert your updated Eval.fs file here from Assignment 7. All modules must be internal.

module internal Eval

    open StateMonad

    
    let binop f a b =
        a >>= fun x ->
            b >>= fun y ->
                ret (f x y)
    let add (a: SM<int>) (b: SM<int>) : SM<int> = binop (+) a b
    let div (a: SM<int>) (b: SM<int>) : SM<int> = binop (/) a b      

    type aExp =
        | N of int
        | V of string
        | WL
        | PV of aExp
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp
        | Div of aExp * aExp
        | Mod of aExp * aExp
        | CharToInt of cExp

    and cExp =
       | C  of char  (* Character value *)
       | CV of aExp  (* Character lookup at word index *)
       | ToUpper of cExp
       | ToLower of cExp
       | IntToChar of aExp

    type bExp =             
       | TT                   (* true *)
       | FF                   (* false *)

       | AEq of aExp * aExp   (* numeric equality *)
       | ALt of aExp * aExp   (* numeric less than *)

       | Not of bExp          (* boolean not *)
       | Conj of bExp * bExp  (* boolean conjunction *)

       | IsVowel of cExp      (* check for vowel *)
       | IsConsonant of cExp  (* check for constant *)

    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    let (./.) a b = Div (a, b)
    let (.%.) a b = Mod (a, b)

    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
    let (.->.) b1 b2 = (~~b1) .||. b2           (* boolean implication *) 
       
    let (.=.) a b = AEq (a, b)   
    let (.<.) a b = ALt (a, b)   
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)    

    let isVowel (c:char) = "aeiouyæøåAEIOUYÆØÅ".Contains(c)
        
    let rec arithEval a : SM<int> =
        match a with
        |N n -> ret n
        |V x -> lookup x
        |WL -> wordLength
        |PV aEx ->
            arithEval aEx >>= fun x ->
                if x >= 0 then pointValue x else fail (IndexOutOfBounds x)
        |Add(a1,a2) -> binop (+) (arithEval a1) (arithEval a2)
        |Sub(a1,a2) -> binop (-) (arithEval a1) (arithEval a2)
        |Mul(a1,a2) -> binop (*) (arithEval a1) (arithEval a2)
        |Div(a1,a2) ->
            (arithEval a1) >>= fun x ->
                (arithEval a2) >>= fun y ->
                    if y<>0 then ret (x/y) else fail DivisionByZero
        | Mod(a1, a2) ->
             (arithEval a1) >>= fun x ->
                (arithEval a2) >>= fun y ->
                    if y<>0 then ret (x%y) else fail DivisionByZero
        | CharToInt(c) ->
            charEval c >>= fun x ->
                ret (int x ) //do we want to check charactervalue here hmmm what about this: - int '0'?
    and charEval c : SM<char> =
        match c with
        |C c -> ret c //what should this be
        |CV aEx ->
            arithEval aEx >>= fun x ->
                characterValue x 
        |ToUpper cEx ->
            charEval cEx >>= fun x ->
                ret (System.Char.ToUpper x)
        |ToLower cEx ->
            charEval cEx >>= fun x ->
                ret (System.Char.ToLower x)
        |IntToChar aEx ->
            arithEval aEx >>= fun x ->
                ret (char x + '0')
    and boolEval b : SM<bool> =
        match b with
        |TT -> ret true
        |FF -> ret false
        |AEq (a1,a2) ->
            arithEval a1 >>= fun x ->
                arithEval a2 >>= fun y ->
                    ret (x=y)
        |ALt (a1,a2) ->
            arithEval a1 >>= fun x ->
                arithEval a2 >>= fun y ->
                    ret (x<y)
        |Not bEx ->
            boolEval bEx >>= fun x ->
                ret (not x)
        |Conj (b1,b2) ->
            boolEval b1 >>= fun x ->
                boolEval b2 >>= fun y ->
                    ret (x && y)
        |IsVowel cEx ->
            charEval cEx >>= fun x ->
                ret (isVowel x)
        |IsConsonant cEx ->
            charEval cEx >>= fun x ->
                ret (not (isVowel x)) //added new here!
            
    type stm =                (* statements *)
    | Declare of string       (* variable declaration *)
    | Ass of string * aExp    (* variable assignment *)
    | Skip                    (* nop *)
    | Seq of stm * stm        (* sequential composition *)
    | ITE of bExp * stm * stm (* if-then-else statement *)
    | While of bExp * stm     (* while statement *)

    let rec stmntEval stmnt : SM<unit> =
        match stmnt with
        |Declare s -> declare s
        |Ass (s, aEx) ->
            arithEval aEx >>= fun x ->
                update s x
        |Seq (s1,s2) ->
            stmntEval s1 >>>=
                stmntEval s2 >>= (fun x -> ret x)
        |Skip -> ret ()  
        |ITE (bEx, s1, s2) ->
            push >>>=
            boolEval bEx >>= fun x ->
                (if x then
                    stmntEval s1
                else
                    stmntEval s2) >>>=
                pop
        |While (bEx, s1)  ->
            push >>>=
                boolEval bEx >>= fun x ->
                (if not x then
                    ret ()
                else
                    stmntEval s1 >>>=
                    stmntEval (While (bEx, s1))) >>>=
                pop

       
(* Part 3 (Optional) *)

    type StateBuilder() =

        member this.Bind(f, x)    = f >>= x
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Delay(f)      = f ()
        member this.Combine(a, b) = a >>= (fun _ -> b)
        
    let prog = StateBuilder()
        
    let rec arithEval2 a = prog {
        match a with
        |N n -> return n
        |V x -> return! lookup x
        |WL -> return! wordLength
        |PV aEx ->
            let! x = (arithEval2 aEx)
            if x >= 0 then return! pointValue x else return! fail (IndexOutOfBounds x)
        |Add(a1,a2) -> return! binop (+) (arithEval2 a1) (arithEval2 a2)
        |Sub(a1,a2) -> return! binop (-) (arithEval2 a1) (arithEval2 a2)
        |Mul(a1,a2) -> return! binop (*) (arithEval2 a1) (arithEval2 a2)
        |Div(a1,a2) ->
            let! x =  (arithEval2 a1)
            let! y =  (arithEval2 a2)
            if y<>0 then return (x/y) else return! fail DivisionByZero
        | Mod(a1, a2) ->
             let! x= (arithEval2 a1) 
             let! y = (arithEval2 a2)
             if y<>0 then return (x%y) else return! fail DivisionByZero
        | CharToInt(c) ->
            let! x = (charEval c)
            return (int x ) //do we want to check charactervalue here hmmm what about this: - int '0'?
            }
    and charEval2 c : SM<char> = prog {
        match c with
        |C c -> return c //what should this be
        |CV aEx ->
            let! x = (arithEval2 aEx)
            return! characterValue x     
        |ToUpper cEx ->
            let! x =  charEval2 cEx
            return (System.Char.ToUpper x)
        |ToLower cEx ->
            let! x =  charEval2 cEx
            return (System.Char.ToLower x)
        |IntToChar aEx ->
            let! x = (arithEval2 aEx)
            return (char x + '0')
          }
    and boolEval2 b = prog {
        match b with
        |TT -> return true
        |FF -> return false
        |AEq (a1,a2) ->
            let! x = (arithEval2 a1)
            let! y = (arithEval2 a2)
            return (x=y)
        |ALt (a1,a2) ->
            let! x = (arithEval2 a1)
            let! y = (arithEval2 a2)
            return (x<y)
        |Not bEx ->
            let! x = (boolEval2 bEx)
            return (not x)
        |Conj (b1,b2) ->
            let! x = (boolEval2 b1)
            let! y = (boolEval2 b2)
            return (x && y)         
        |IsVowel cEx ->
            let! x = charEval2 cEx
            return (isVowel x)
        |IsConsonant cEx ->
            let! x = charEval2 cEx
            return (not (isVowel x))
    }

    let rec stmntEval2 stmnt = prog{
        match stmnt with
        |Declare s -> return! declare s
        |Ass (s, aEx) ->
            let! x = arithEval2 aEx
            return! update s x
        |Seq (s1,s2) ->
            do! (stmntEval2 s1)
            let! x = (stmntEval2 s2)
            return x
        |Skip -> return ()  
        |ITE (bEx, s1, s2) ->
            do! push
            let! x = (boolEval2 bEx)
            if x then do! (stmntEval2 s1) else do! (stmntEval2 s2)
            return! pop
        |While (bEx, s1)  ->
            do! push
            let! x = (boolEval2 bEx)
            if not x then return () else
                    do! (stmntEval2 s1)
                    do! stmntEval2 (While (bEx, s1))
                    return! pop
    }

(* Part 4 *) 

    type word = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>
    type squareStmnt = Map<int, stm>
    
    type square = Map<int, squareFun>
    type coord = int * int
    
    type boardFun2 = coord -> Result<square option, Error>
        
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun2
    }

    let stmntToSquareFun (stmnt: stm) (w:word) (pos: int) (acc: int) =
        let state = mkState [("_pos_", pos); ("_acc_", acc); ("_result_", 0)] w ["_pos_"; "_acc_";"_result_"]
        stmntEval2 stmnt >>>=
        lookup "_result_" |>
        evalSM state

 
    let stmntsToSquare (m: squareStmnt): square =
        Map.map(fun _ y -> (stmntToSquareFun y) ) m

    type boardFun = coord -> Result<squareFun option, Error> 

    let stmntToBoardFun (stmnt: stm) (t: Map<int, 'a>) ((x,y):coord): Result<'a option, Error> =
        let state = mkState [("_x_", x); ("_y_", y); ("_result_", 0)] [] ["_x_"; "_y_";"_result_"]
        stmntEval2 stmnt >>>=
        lookup "_result_" >>= (fun x ->
            match t.TryFind x with
            |Some x -> ret (Some x)
            |None -> ret None) |>
        evalSM state 

    let mkBoard (c:coord) (defaultSq:int) (boardStmnt: stm) (m:Map<int,squareStmnt>) : board =
        {
         center = c;
         defaultSquare = (m[defaultSq] |> stmntsToSquare);
         squares = stmntToBoardFun boardStmnt (Map.map(fun _ y -> stmntsToSquare y ) m)
         }