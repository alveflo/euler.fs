// The prime factors of 13195 are 5, 7, 13 and 29.
// What is the largest prime factor of the number 600851475143 ?
let isPrime (n: int64) =
  let sqrt' = (float >> sqrt >> int64) n
  [ 2L .. sqrt']
  |> List.forall (fun x -> n % x <> 0L)

let rec nextPrime (n: int64) : int64 =
  let next = int64(n+1L)
  if isPrime(next) then next
  else nextPrime(next)

let rec primeFactor (num: int64, divider: int64): int64 =
  if num % divider = 0L then
    let res = int64(num/divider)
    primeFactor(res, divider)
  elif isPrime(num) then num
  else primeFactor(num, (nextPrime divider))

printfn "%d" primeFactor(600851475143L, 2L)
