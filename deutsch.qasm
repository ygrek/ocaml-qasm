OPENQASM 2.0;
include "qelib1.inc";

gate f0 a, b  {
  id a;
  id b;
}

gate f1 a, b  {
  id a;
  x b;
}

gate fx a, b  {
  cx a, b;
}

gate fxc a, b  {
  x a;
  cx a, b;
  x a;
}

qreg q[2];
creg c[1];

id q[0];
x q[1];
h q[0];
h q[1];
fxc q[0], q[1];
//  x q[0];
//  cx q[0], q[1];
//  x q[0];
id q[1];
h q[0];
measure q[0] -> c[0];
