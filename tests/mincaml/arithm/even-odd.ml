let rec even x =
  let rec odd x =
    if x > 0 then even (x - 1) else
    if x < 0 then even (x + 1) else
    456 in
  if x > 0 then odd (x - 1) else
  if x < 0 then odd (x + 1) else
  123 in
print_int (even 789)
