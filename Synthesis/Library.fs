module Synthesis

open System.ComponentModel.Design

let abelar a =
     a>12 && a<3097 && (a%12)=0

let area b h =
    match (b<0.0),(h<0.0) with
    |true,_ |_,true -> failwith "theQueen"
    |_,_ -> (b*h)/2.0
    

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

let minmax (a,s,d,f) =
    let ma = max (max a s) (max d f)
    let mi = min (min a s) (min d f)
    mi,ma

let isLeap yr =
    match (yr>=1582), (yr%4=0), (yr%100=0), (yr%400=0) with
    |false,_,_,_ -> failwith "theQueen"
    |_,true,true,true |_,true,false,false -> true
    |_,true,true,false -> false
    |_ -> false

    

let month i =
    match (i<=0),(i>12),i with
    |true,_,_ |_,true,_ -> failwith "theQueen"
    |_,_,1 -> "January", 31
    |_,_,2 -> "February", 28
    |_,_,3 -> "March", 31
    |_,_,4 -> "April", 30
    |_,_,5 -> "May", 31
    |_,_,6 -> "June", 30
    |_,_,7 -> "July", 31
    |_,_,8 -> "August", 31
    |_,_,9 -> "September", 30
    |_,_,10 -> "October", 31
    |_,_,11 -> "November", 30
    |_,_,12 -> "December", 31

let toBinary d =
    match d<0 with
    |true -> failwith "theQueen"
    |false -> let rec toBinary d rem =
               match d/2,d%2 with
               |0,0 -> "0" + rem
               |0,1 -> "1" + rem
               |nd,0 -> toBinary nd ("0" + rem)
               |nd,1 -> toBinary nd ("1" + rem)
              toBinary d ""

let bizFuzz _ =
    failwith "Not implemented"

let monthDay _ _ =
    failwith "Not implemented"

let coord _ =
    failwith "Not implemented"