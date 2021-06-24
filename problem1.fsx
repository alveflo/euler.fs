// If we list all the natural numbers below 10
// that are multiples of 3 or 5, we get 3, 5, 6 and 9.
// The sum of these multiples is 23.

// Find the sum of all the multiples of 3 or 5 below 1000.

let rec test num sum =
    if num > 999 then sum
    else
        if num % 3 = 0 || num % 5 = 0 then
            test (num + 1) (sum + num)
        else
            test (num + 1) sum

let res = test 1 0
printfn "%d" res
