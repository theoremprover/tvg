
typedef struct {
	int line, column;
	int cnt = 0; }
	COUNTER;

typedef struct {
	char sourcefilename[100];
	COUNTER counters[1234];
} SRCFILE;

SRCFILE abc_def_xyz_c = { "abc/def/xyz.c" };
