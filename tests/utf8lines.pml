/*      This is a very cursory test of the hgetLine action for properly-utf8-coded files
        Here are ßøµ 
        α→β
*/

let h         = findInput $ ⟨  p::ps -> p | [] ->  "tests/utf8lines.pml" ⟩ argv;;
let read ls h = (hgetLine h >>> ⟨ l -> read ((len l, l)::ls) h ⟩) 
                >?> 
                ⟨ eof -> return (rev ls) ⟩;;
                
let main = (h>>>read []) >?> ⟨ error -> return [((-1), "No such file")] ⟩;;



