//open System.Linq

#light

//
// <<Marek Zietek>>
// U. of Illinois, Chicago
// CS 341, Fall 2018
// Project #05: Language prediction based on letter frequencies
//
// This program analyzes text from various languages, and computes
// letter frequencies.  These letter frequencies serve as a "barcode"
// that potentially identify the language when written.  This approach,
// and assignment, is inspired by the students and professor of CS 141,
// Fall 2018, at the U. of Illinois, Chicago.  Kudos to Prof Reed.
//


//
// explode:
//
// Given a string s, explodes the string into a list of characters.
// Example: explode "apple" => ['a';'p';'p';'l';'e']
//
let explode s =
  let finalText = String.map System.Char.ToLower s
  //printfn "%A" finalText
  [for c in finalText -> c ]



//
// implode
//
// The opposite of explode --- given a list of characters, returns
// the list as a string.
//
let implode L =
  let sb = System.Text.StringBuilder()
  List.iter (fun c -> ignore (sb.Append (c:char))) L
  sb.ToString()


//
// FileInput:
//
// Inputs text from a file line by line, returning this as a list
// of strings.  Each line is converted to lower-case.
//
let FileInput filename = 
  [ for line in System.IO.File.ReadLines(filename) -> line.ToLower() ]


//
// UserInput:
//
// This function reads from the keyboard, line by line, until 
// # appears on a line by itself.  The lines are returned as
// a list of strings; each line is converted to lower-case.
//
// NOTE: if the first line of input is blank (i.e. the user 
// presses ENTER), then input is read from the file 'input.txt'.
// Helpful when testing.
//
let rec _UserInput input =
  let line = System.Console.ReadLine()
  match line with
  | "#" -> List.rev input
  |  _  -> _UserInput (line.ToLower()::input)

let UserInput() =
  let firstLine = System.Console.ReadLine()
  match firstLine with
  | "" -> FileInput @"..\..\input.txt"
  | _  -> _UserInput [firstLine.ToLower()]



//Counts occurences and create a tuple  
let rec functionThing alphabetList L1 =
    match alphabetList with 
    | [] -> []
    | hd::tl -> let occurences = List.map ( fun x -> if x = hd then 1 else 0 ) L1
                [(hd, List.sum occurences)] @ functionThing tl L1
    

//Counts letters
let rec countFunction alphabetList newExplodedList =
    match alphabetList with 
    | [] -> [] 
    | hd::tl -> let occurences = List.countBy( fun x -> if x = hd then 1 else 0) newExplodedList 
                countFunction tl newExplodedList


//Function that will recurse till you find the position 
let rec findPosition letter engList = 
    match engList with 
    | [] -> 0
    | e::rest -> if letter = e then 0 else 1 + findPosition letter rest

    // List.findIndex (fun x -> x=letter) engList

//Function to count the diffrence 
let diffrence letter inputList engList threshold = 
    let x = findPosition letter inputList
    let y = findPosition letter engList
    let diff = abs(x-y) 
    if diff > threshold then diff else 0 


// *********************************************************************** //
//
// Main:
//
[<EntryPoint>]
let main argv =
  printfn "** Training... **"
  printfn ""
 
  //Opens up training file
  let files = [ for filename in System.IO.Directory.GetFiles(@"..\..\training") -> filename]
  
  //Passes oganizeLetters into a function called getSum using map 
  let alphabetList = [ 'a' .. 'z' ] 

  //printfn "Training files: %A" files
  printfn "** Letter Frequency Counts (A->Z) **"
  
  //Skips the first line
  let languages = List.sort(files)
  let L1 = List.map( fun x -> FileInput x ) languages
  
  //English total letters
  let english = List.item(0)L1
  let english1 = List.tail(english)
  let organizeEnglish = List.map ( fun x -> explode x ) english1
  let newExplodedLanguage = List.concat organizeEnglish
  let LineEngLanguage = functionThing alphabetList newExplodedLanguage
  
  let printAllEng = List.map ( fun (a,b) -> b ) LineEngLanguage
  printf "\"english\": "
  List.iter (printf "%A ") printAllEng
  printfn " "

  //finnish tat0l letters 
  let finnish = List.item(1)L1
  let finnish1 = List.tail(finnish)
  let organizeFinnish = List.map ( fun x -> explode x ) finnish1
  let newExplodedLanguage1 = List.concat organizeFinnish
  let LineEngLanguage1 = functionThing alphabetList newExplodedLanguage1
  
  let printAllFin = List.map ( fun (a,b) -> b ) LineEngLanguage1
  printf "\"finnish\": "
  List.iter (printf "%A ") printAllFin
  printfn " "

  //french tatol letters 
  let french = List.item(2)L1
  let french1 = List.tail(french)
  let organizeFrench = List.map ( fun x -> explode x ) french1
  let newExplodedLanguage2 = List.concat organizeFrench
  let LineEngLanguage2 = functionThing alphabetList newExplodedLanguage2
  
  let printAllFre = List.map ( fun (a,b) -> b ) LineEngLanguage2
  printf "\"french\": "
  List.iter (printf "%A ") printAllFre
  printfn " "

  //german tatol letters 
  let german = List.item(3)L1
  let german1 = List.tail(german)
  let organizeGerman = List.map ( fun x -> explode x ) german1
  let newExplodedLanguage3 = List.concat organizeGerman
  let LineEngLanguage3 = functionThing alphabetList newExplodedLanguage3
  
  let printAllGer = List.map ( fun (a,b) -> b ) LineEngLanguage3
  printf "\"german\": "
  List.iter (printf "%A ") printAllGer
  printfn " "

  // Hungarian tatol letters 
  let hungarian = List.item(4)L1
  let hung1 = List.tail(hungarian)
  let organizeHung = List.map ( fun x -> explode x ) hung1
  let newExplodedLanguage4 = List.concat organizeHung
  let LineEngLanguage4 = functionThing alphabetList newExplodedLanguage4
  
  let printAllHung = List.map ( fun (a,b) -> b ) LineEngLanguage4
  printf "\"hungarian\": "
  List.iter (printf "%A ") printAllHung
  printfn " "

  // Italian tatol letters 
  let italian = List.item(5)L1
  let ital1 = List.tail(italian)
  let organizeItal = List.map ( fun x -> explode x ) ital1
  let newExplodedLanguage5 = List.concat organizeItal
  let LineEngLanguage5 = functionThing alphabetList newExplodedLanguage5
  
  let printAllItal = List.map ( fun (a,b) -> b ) LineEngLanguage5
  printf "\"italian\": "
  List.iter (printf "%A ") printAllItal
  printfn " "

  // Portuguese tatol letters 
  let port = List.item(6)L1
  let port1 = List.tail(port)
  let organizePort = List.map ( fun x -> explode x ) port1
  let newExplodedLanguage6 = List.concat organizePort
  let LineEngLanguage6 = functionThing alphabetList newExplodedLanguage6
  
  let printAllPort = List.map ( fun (a,b) -> b ) LineEngLanguage6
  printf "\"portuguese\": "
  List.iter (printf "%A ") printAllPort
  printfn " "

  // Spanish tatol letters 
  let spanish = List.item(7)L1
  let sp1 = List.tail(spanish)
  let organizeSp = List.map ( fun x -> explode x ) sp1
  let newExplodedLanguage7 = List.concat organizeSp
  let LineEngLanguage7 = functionThing alphabetList newExplodedLanguage7
  
  let printAllSp = List.map ( fun (a,b) -> b ) LineEngLanguage7
  printf "\"spanish\": "
  List.iter (printf "%A ") printAllSp
  printfn " "

  //------------------------------------------------------------------------------------
  //Frequency printing

  printfn "\n** Letter Frequency Order (High->Low) **"
  
  //English  descending order 
  let sortedEng = List.sortByDescending( fun (a,b) -> b ) LineEngLanguage
  let sortedE = List.map ( fun (a,b) -> a ) sortedEng
  printf "\"english\": "
  printf "%s" (implode sortedE)
  printfn ""

  //finnish  descending order 
  let sortedFin = List.sortByDescending( fun (a,b) -> b ) LineEngLanguage1
  let sortedF = List.map ( fun (a,b) -> a ) sortedFin
  printf "\"finnish\": "
  printf "%s" (implode sortedF)
  printfn ""

  //french  descending order 
  let sortedFre = List.sortByDescending( fun (a,b) -> b ) LineEngLanguage2
  let sortedFr = List.map ( fun (a,b) -> a ) sortedFre
  printf "\"french\": "
  printf "%s" (implode sortedFr)
  printfn ""

  //german  descending order 
  let sortedG = List.sortByDescending( fun (a,b) -> b ) LineEngLanguage3
  let sortedG = List.map ( fun (a,b) -> a ) sortedG
  printf "\"german\": "
  printf "%s" (implode sortedG)
  printfn ""

  //hungarian  descending order 
  let sortedHu = List.sortByDescending( fun (a,b) -> b ) LineEngLanguage4
  let sortedH = List.map ( fun (a,b) -> a ) sortedHu
  printf "\"hungarian\": "
  printf "%s" (implode sortedH)
  printfn ""

  //italian  descending order 
  let sortedIt = List.sortByDescending( fun (a,b) -> b ) LineEngLanguage5
  let sortedI = List.map ( fun (a,b) -> a ) sortedIt
  printf "\"italian\": "
  printf "%s" (implode sortedI)
  printfn ""

  //portuguese  descending order 
  let sortedPo = List.sortByDescending( fun (a,b) -> b ) LineEngLanguage6
  let sortedP = List.map ( fun (a,b) -> a ) sortedPo
  printf "\"portugese\": "
  printf "%s" (implode sortedP)
  printfn ""

  //spanish  descending order 
  let sortedSp = List.sortByDescending( fun (a,b) -> b ) LineEngLanguage7
  let sortedS = List.map ( fun (a,b) -> a ) sortedSp
  printf "\"spanish\": "
  printf "%s" (implode sortedS)
  printfn ""
  printfn ""

  // Here we get text from the user, analyze, and guess the language:
  printfn "Please enter text, followed by # (default = 'input.txt')> "
  let text = UserInput()
  
  // [ [H; e; y; " "; M; a; n] [W; h; a; t; s; ; " "; u; p; ] ]
  let organizeLetters = List.map ( fun x -> explode x ) text
  
  //Concate so it's [ h;e;y; ; m;a;n; ; w;h;a;t;s; ; ]
  let newExplodedList = List.concat organizeLetters

  //Counts the total letters. This is the MAIN / most IMPORTANT one
  let LineMap = functionThing alphabetList newExplodedList

  //Prints the total of letters from A to Z in order
  printf "\n"
  let printAllLetters = List.map ( fun (a,b) -> b ) LineMap
  printf "\"input\": "
  List.iter (printf "%A ") printAllLetters
  
  //Now sort them in descending order 
  let sortedTuples = List.sortByDescending( fun (a,b) -> b ) LineMap
  let sorted = List.map ( fun (a,b) -> a ) sortedTuples
  printf "\n\"input\": "
  printf "%s" (implode sorted)
  
  printf "\n"
  printfn ""
  
  //------------------------------------------------------------------------------------------------
  //Thresold step

  printf "Enter difference threshold (default = 4)> "
  let s = System.Console.ReadLine()
  let threshold = if s = "" then 4 else int(s)
  

  //Total for english
  let diffsE = List.map( fun x -> diffrence x sorted sortedE threshold ) alphabetList
  let totalE = List.sum diffsE

  //Total for finnish
  let diffsF = List.map( fun x -> diffrence x sorted sortedF threshold ) alphabetList
  let totalF = List.sum diffsF

  //Total for frensh
  let diffsFr = List.map( fun x -> diffrence x sorted sortedFr threshold ) alphabetList
  let totalFr = List.sum diffsFr

  //Total for german
  let diffsG = List.map( fun x -> diffrence x sorted sortedG threshold ) alphabetList
  let totalG = List.sum diffsG

  //Total for hugarian
  let diffsH = List.map( fun x -> diffrence x sorted sortedH threshold ) alphabetList
  let totalH = List.sum diffsH

  //Total for Italian
  let diffsI = List.map( fun x -> diffrence x sorted sortedI threshold ) alphabetList
  let totalI = List.sum diffsI

  //Total for Potugese
  let diffsP = List.map( fun x -> diffrence x sorted sortedP threshold ) alphabetList
  let totalP = List.sum diffsP

  //Total for Spanish
  let diffsSp = List.map( fun x -> diffrence x sorted sortedS threshold ) alphabetList
  let totalSP = List.sum diffsSp

  //------------------------------------------------------------------------------------------------

  //Set up the tuple for the last step
  let diffLang = [("english", totalE); ("finnish", totalF);("frensh",totalFr);("german",totalG); ("hungarian",totalH);("italian", totalI); ("portuguese",totalP); ("spanish",totalSP) ]
  let sortedPredic = List.sortBy( fun (a,b) -> b ) diffLang
  let lang, count = sortedPredic.[0]

  printfn ""
  printfn "diffs: %A" sortedPredic

  printfn ""
  
  let prediction = "?"
  printfn "** Input language: %A" lang
  printfn ""
  //
  0