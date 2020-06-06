par int: k = 32;
par int: max_uint = 2147483646;

function var 0..max_uint: mypow2(var 0..2147483646: x) = if x>0 then 2*mypow2(x-1) else 1 endif;

%function var 0..max_uint: bitshift_right(var 0..max_uint: a, var 0..max_uint: i) =
%  let {
%    array[0..k-1] of var bool: a_bits;
%    var 0..2147483646: a1 = sum(j in 0..k-1)(bool2int(a_bits[j])*mypow2(j));
%    constraint a1 = a;
%    } in
%  sum(j in i..k-1)(bool2int(a_bits[j])*mypow2(j-i));

function var 0..max_uint: bitshift_right(var 0..max_uint: a, var 0..max_uint: i) =
    if i=0 then a else bitshift_right(a div 2,i-1) endif;

var 0..20 : n;
var 0..max_uint : bit0 = 30;

constraint bitshift_right(bit0,n-1) != 0;
constraint bitshift_right(bit0,n) = 0;

%constraint bitshift_right(bit0,1)=n;

%constraint mypow2(n) = 16;

solve :: int_search([n]
                   , input_order, indomain_median, complete) satisfy;