module internal Dictionary

open System.Collections.Generic
open System.Linq
open ScrabbleUtil.DebugPrint

type Dict =
    |Leaf of bool
    |Node of bool * System.Collections.Generic.Dictionary<char, Dict>
    
let empty () = Leaf false


//OLD BUT WORKS! NOT WITH GADDAG THOUGH
let rec lookup (s:string) (dict: Dict) =
    match dict with
    |Leaf b when s.Length = 0 ->
        //printfn "got to a leaf the word is done searching the result was: "
        b
    |Leaf _ ->
        //printfn "Reached a leaf before the string was done, the word was not found"
        false //if we get here the string is not done but we have reached a leaf, therefor the word was not found 
    //if we come to a node that has the boolean true, and the lenght of the string is 1 then we have found the word
    |Node (b,_) when s.Length = 0 ->
        //printfn "Node and the string length is 0"
        b
        //is this correct tho?
    //if the string is done and no word ends here we return false
    |Node (_,d) ->
        //check if the char we are at is in the dictionary
        let (bool, foundDict) = d.TryGetValue s.[0]
        match (bool, foundDict) with
        |(true, dict) ->
            //printfn "continue we found a char that macthed"
            lookup s.[1..] dict
            
        |(false, _) ->
            //printf("no char macthed. Do not continue")
            false

let rec insert (s: string) (dict: Dict) =
    match dict with
    //when the dict is a leaf and the string is done inserting
    |Leaf _ when s.Length = 0 ->
        //printfn "Leaf string is done"
        Leaf true
        
    //if the dict is a node and the length of the string is 0 we are done inserting
    |Node (_,d) when s.Length = 0 ->
         //printfn "Node string is done"
         Node(true, d)
    
    //when the dict is a leaf but the string is more than 0
    //we want to continue inserting
    |Leaf b -> //do we need the boolean?
        //printfn "Leaf string is not done"
        let newDict = Dictionary<char,Dict>()
        newDict.Add(s.[0],insert s.[1..] (empty()))
        Node(b, newDict) 
        
    //if the dict is a node and the length of the string is more than 0
    |Node (b, d) ->
        //printfn "Node string is not done"
        //check if the dictionary contains the char
        let (bool, foundDict)  = d.TryGetValue s.[0]
        match (bool, foundDict) with
        |(true, dict) ->
            //printfn "Found a key already in the trie keep going"
            d.[s.[0]] <- (insert s.[1..] dict) //the dictionary did have the key char so we want to continue down the trie in the dictioanary belonging to the char
            Node(b,d)
            
        //the dictionary did not have the key so we add it to make a new "way" down the trie
        |(false,_) ->
            //printfn "Inserting the new key"
            d.Add(s.[0],insert s.[1..] (empty()))
            Node(b, d)

   
let step (c: char) (dict: Dict) =
    match dict with
     //if we reach a node and the nodes dictionary contains the char then there is a valid path. The boolean lets us know if a word ends
    |Node (_,d) ->
        //try to get the value in the dictionary in that node
        let contains = d.ContainsKey c
        match contains with
        |true ->
            let foundDict = d.[c]
            match foundDict with
            |Node (b,dictionary) ->
                //printf("There was a path (node) to the char in the dictionary and a word has ended:  %b\n") b
                Some (b,foundDict)
            |Leaf b ->
                //printf("There was a path (leaf) to the char in the dictionary and a word has ended %b\n") b
                Some (b, foundDict)
        |(false) ->
            //printf("There char was not in the dictionary\n")
            //if we are here the char was not found in the nodes dictionary and therefor there exists no path
            None
    //if we are here we have reached a leaf and the node does not contain the char so we return none
    |Leaf _ ->
        //printf("There was not a path to the char in the dictionary we are at a leaf\n")
        None
   

