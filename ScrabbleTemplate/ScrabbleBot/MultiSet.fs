// Insert your MultiSet.fs file here. All modules must be internal
//Implementation file

module internal MultiSet
        
    type MultiSet<'a when 'a : comparison> = Multi of Map<'a,uint32>
    let empty = Multi Map.empty
    let isEmpty (Multi s) = s.IsEmpty
    
    let size (Multi s) = Map.fold(fun acc x y -> acc + y) 0u s
        
    let contains a (Multi s) = s.ContainsKey a
    let numItems a (Multi s) =
        let num = s.TryFind a
        match num with
        |None -> 0u
        |_ -> num.Value

    let add a n (Multi s) = Multi (s.Add(a,(numItems a (Multi s) + n)))
                               
    let addSingle a (Multi s) = add a 1u (Multi s)
        
    let remove a n (Multi s) =
        let num = numItems a (Multi s)
        match num with
        |num when num <= n -> Multi(s.Remove a) //this case works
        |_-> Multi(s.Add(a,num-n))
            
    let removeSingle a (Multi s) =
        let found = s.TryFind a
        match found with
        |None -> (Multi s)
        |_ -> remove a 1u (Multi s)
            
    let fold f acc (Multi s) = Map.fold f acc s
        
    let foldBack f (Multi s) acc  = Map.foldBack f s acc
        
    let ofList lst = List.fold(fun acc x -> addSingle x acc) empty lst
      
    //creating a new list for every 'a in the map and then appending these lists together
    let toList (Multi s) = foldBack (fun x y acc -> List.init (int y) (fun t -> x) @ acc) (Multi s) []
          
    let map (f: 'a -> 'b) (Multi s) = fold(fun acc x y -> add (f x) y acc) empty (Multi s)
        
    //||> is to pipe more parameters
    let union (Multi s1) (Multi s2) = (empty, s1) ||> Map.fold(fun acc x y ->
            let contains = contains x (Multi s2)
            match contains with
            |true -> if numItems x (Multi s2) > y then add x (numItems x (Multi s2)) acc else add x y acc
            |_ -> add x y acc)
           
    let sum (Multi s1) (Multi s2) = (empty, s1) ||> Map.fold(fun acc x y ->
            let numOfItemIn2 = numItems x (Multi s2) //if the item is now in the second then that just returns 0
            add x (numOfItemIn2 + y) acc)
        
    let subtract (Multi s1) (Multi s2) =  fold (fun acc x y -> remove x y acc) (Multi s1) (Multi s2)
    
   
    let intersection (Multi s1) (Multi s2) = (empty, (Multi s1)) ||> fold(fun acc x y ->
            let numIn2 = numItems x (Multi s2) //if the item is now in the second then that just returns 0
            match numIn2 with
            |0u -> acc //if there are none in s2 we don't want it in the intersection
            |numIn2 when numIn2 < y -> add x numIn2 acc //if there are less of an item in s2 we want that number of it
            |_ -> add x y acc) //if there are more of an item in s2 we want to keep the number in s1
            
            