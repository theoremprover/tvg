int: k = 16;

var 0..pow(2,k)-1: x;
var 0..pow(2,k)-1: y;

function var int: bitwise_or(var int: a, var int: b) =
  let {
    array[0..k-1] of var bool: a_bits;
    var 0..pow(2,k)-1: a1 = sum(j in 0..k-1)(bool2int(a_bits[j])*pow(2,j));
    array[0..k-1] of var bool: b_bits;
    var 0..pow(2,k)-1: b1 = sum(j in 0..k-1)(bool2int(b_bits[j])*pow(2,j));
    constraint a1 = a;
    constraint b1 = b;
    } in
  sum(j in 0..k-1)(bool2int(a_bits[j] \/ b_bits[j])*pow(2,j));

function var int: bitwise_and(var int: a, var int: b) =
  let {
    array[0..k-1] of var bool: a_bits;
    var 0..pow(2,k)-1: a1 = sum(j in 0..k-1)(bool2int(a_bits[j])*pow(2,j));
    array[0..k-1] of var bool: b_bits;
    var 0..pow(2,k)-1: b1 = sum(j in 0..k-1)(bool2int(b_bits[j])*pow(2,j));
    constraint a1 = a;
    constraint b1 = b;
    } in
  sum(j in 0..k-1)(bool2int(a_bits[j] /\ b_bits[j])*pow(2,j));

function var int: bitwise_not(var int: a) =
  let {
    array[0..k-1] of var bool: a_bits;
    var 0..pow(2,k)-1: a1 = sum(j in 0..k-1)(bool2int(a_bits[j])*pow(2,j));
    constraint a1 = a;
    } in
  sum(j in 0..k-1)(bool2int(not a_bits[j])*pow(2,j));

constraint bitwise_and(x,y) = 27;
constraint bitwise_not(x) = 128;

solve satisfy;
