  function fib(n) {
  const a = 0
  const __temp0 = n-1
  const next1 = __temp0
  const __temp1 = n-2
  const next2 = __temp1
  if (n < 2) {
   const a = n
 } else {
   const __temp2 = fib(next1)
  const __temp3 = fib(next2)
  const __temp4 = __temp2+__temp3
  const a = __temp4
 }

  
 return a

}

  const __temp5 = fib(35)
  const a = __temp5
  console.log(a)
