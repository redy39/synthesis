module Synthesis

open System.ComponentModel.Design

let abelar a =
     a>12 && a<3097 && (a%12)=0

let area b h =
    match (b<0.0),(h<0.0) with
    |true,_ |_,true -> failwith "theQueen"
    |_,_ -> (b+h)/2.0
    

let zollo n =
    match n>0 with
    |true -> n*2
    |false -> -1*n

let min p q =
    match p>q with 
    |true -> q
    |false -> p

let max p q =
    match p>q with 
    |true -> p
    |false -> q

let ofTime h m s =
    (h*60*60) + (m*60) + s

let toTime s =
    match s>0 with
    |false -> 0,0,0
    |true -> let h = s/60/60
             let m = ((s-(h*60*60))/60)
             let sec = s-(h*60*60)-(m*60)
             h,m,sec
             

let digits n =
    let rec cnt v acc = 
        match v>=(-9) && v<=9 with
        |true -> acc+1
        |false -> cnt (v/10) (acc+1)
    cnt n 0

let minmax _ =
    failwith "Not implemented"

let isLeap _ =
    failwith "Not implemented"

let month _ =
    failwith "Not implemented"

let toBinary _ =
    failwith "Not implemented"

let bizFuzz _ =
    failwith "Not implemented"

let monthDay _ _ =
    failwith "Not implemented"

let coord _ =
    failwith "Not implemented"