structure Eval =
struct
  open TM
  (*リストの先頭を取得して返す*)
  fun Hd nil = B
    | Hd (h :: _) = h

  (*リストの先頭を除いた要素を返す*)
  fun Tl nil = nil 
    | Tl (_ :: tl) = tl

  (*二つのリストを結合*)
  fun Cons (B, nil) = nil
    | Cons (h,t) = h::t

  fun moveL (LList, h, RList) = 
      (Tl LList, Hd LList, Cons (h, RList))

  fun moveR (LList, h, RList) = 
      (Cons (h, LList), Hd RList, Tl RList)

  (*テープを動かす関数。引数に応じてmoveL,moveRを呼ぶ*)
  fun move L tape = moveL tape
    | move R tape = moveR tape

  (*現在のTMの状態を返す。*)
  fun print (q, (LList, h, RList)) =
      Dynamic.pp
      {state=q,
       tape = (List.rev LList, h, RList)}

  (*遷移先がある間は次の状態を計算*)
  fun exec delta (q, tape as (LList, h, RList)) =
       case List.find (fn (x,y) => x = (q, h)) delta of
         NONE => (LList, h, RList)
       | SOME (x, (q', s, d)) => 
         exec delta (q', move d (LList, s, RList))

  fun eval (state, delta) tape = exec delta (state,tape)
end
