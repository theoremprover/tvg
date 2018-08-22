/*
gcc -w -DFORBUILD -shared -fPIC tvg.c -o libtvg.so

*/

void opentrace()
{
#ifndef FORBUILD
#endif
}

void closetrace()
{
#ifndef FORBUILD
#endif
}
