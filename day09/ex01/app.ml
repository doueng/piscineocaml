module type APP =
sig
  type project = string * string * int
  val zero : project
  val combine : project -> project -> project
  val fail : project -> project
  val success : project -> project
end

module App : APP =
struct
  type project = string * string * int

  let (zero : project) = ("", "", 0)

  let fail (p : project) : project =
    let (old, _, _) = p in
    (old, "failed", 0)

  let success (p : project) : project =
    let (old, _, _) = p in
    (old, "succeed", 80)

  let combine (p1 : project) (p2 : project) : project =
    let (s1, status1, grade1) = p1 in
    let (s2, status2, grade2) = p2 in
    if (grade1 + grade2) / 2 > 80 then
      success ((s1 ^ s2), "", 0)
    else
      fail ((s1 ^ s2), "", 0)
end

let () =
  let print_proj p : unit =
    let (s, status, grade) = p in
    print_endline (s ^ " " ^ status ^ " " ^ (string_of_int grade))
  in
  print_endline "App.zero";
  print_proj (App.zero);
  print_proj (App.fail ("App.fail", "fail", 100));
  print_proj (App.fail ("App.fail", "succeed", 0));
  print_proj (App.success ("App.success", "fail", 100));
  print_proj (App.success ("App.success", "succeed", 0));
  print_proj (App.combine ("first", "fail", 40) ("second", "succeed", 100));
  print_proj (App.combine ("first", "fail", 65) ("second", "succeed", 100));
