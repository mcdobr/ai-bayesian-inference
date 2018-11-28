
type Event = {
    name: string
    causes: Event list
    distribution: float list
}

// P(event = value | observations) formând o configuratie folosind puteri ale lui 2 
let getProbability (event : Event) (value : bool) (observations : Map<Event, bool>) : float =
    let reverseCausesTuples =  event.causes |> List.rev |> List.mapi (fun i cause -> (i, cause));
    let partialBooleanConfiguration = List.fold (fun acc (i, cause) -> if (Map.find cause observations) then acc + (1 <<< i) else acc) 0 reverseCausesTuples
    let booleanConfiguration = if (value) then partialBooleanConfiguration + (1 <<< (List.length event.causes))  else partialBooleanConfiguration
    event.distribution.[booleanConfiguration]

// Normalizeaza o distributie (suma lor = 1)
let normalize (distribution : float list) =
    let alpha = 1.0 / (List.sum distribution)
    let normalizedDistribution = List.map (fun p -> alpha * p) distribution
    normalizedDistribution

// Suma de probabilitati
let rec enumerateAll (observations : Map<Event, bool>) (vars : Event list) : float =
    if List.isEmpty vars then 1.0
    else
        let Y = List.head vars
        if Map.containsKey Y observations then 
            let factor = enumerateAll observations (List.tail vars)
            let probability = getProbability Y (Map.find Y observations) observations
            probability * factor
        else
            let probFalse = getProbability Y false observations
            let probTrue = getProbability Y true observations

            let observationsFalse = Map.add Y false observations
            let observationsTrue = Map.add Y true observations

            let valueFalse = probFalse * (enumerateAll observationsFalse (List.tail vars))
            let valueTrue = probTrue * (enumerateAll observationsTrue (List.tail vars))

            valueFalse + valueTrue
            
// Distributia de prob a lui query date fiind observations
let enumerationAsk (query : Event) (observations : Map<Event, bool>) (bayesNet : Event list) : float list =
    let fls = enumerateAll (Map.add query false observations) bayesNet
    let tru = enumerateAll (Map.add query true observations) bayesNet

    let distribution = [fls; tru];
    normalize distribution

[<EntryPoint>]
let main argv = 

    let gripa = {name = "gripa"; causes = List.Empty; distribution = [0.9; 0.1]};
    let abces = {name = "abces"; causes = List.Empty; distribution = [0.95; 0.05]};
    let febra = {name = "febra"; causes = [gripa; abces]; distribution = [0.95; 0.75; 0.3; 0.2; 0.05; 0.25; 0.7; 0.8]};
    let oboseala = {name = "oboseala"; causes = [febra]; distribution = [0.8; 0.4; 0.2; 0.6]};
    let anorexie = {name = "anorexie"; causes = [febra]; distribution = [0.9; 0.5; 0.1; 0.5]};

    let bayesNet = [gripa; abces; febra; oboseala; anorexie];

    let observations = [(oboseala, true); (anorexie, true)] |> Map.ofList

    let result = enumerationAsk gripa observations bayesNet
    printfn "%A" result

    0
