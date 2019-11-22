int: k = 16;

function var int: bitshift_left(var int: a, var int: i) =
  let {
    array[0..k-1] of var bool: a_bits;
    var 0..pow(2,k)-1: a1 = sum(j in 0..k-1)(bool2int(a_bits[j])*pow(2,j));
    constraint a1 = a;
    } in
  sum(j in 0..k-1-i)(bool2int(a_bits[j])*pow(2,j+i));

var int: x;
constraint x = bitshift_left(1, 3) ;

solve satisfy;
