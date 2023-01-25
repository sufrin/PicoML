let
 ⌈⌉: Num->Num;
 ⌈n⌉ = if is_int n then n else 1 +  ⌊n⌋
 ;;
