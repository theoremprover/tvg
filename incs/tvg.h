void mytrace(char *);
void opentrace(char*);
void closetrace();

typedef struct {
	int line, column;
	int cnt; }
	COUNTER;

typedef struct {
	char sourcefilename[100];
	COUNTER counters[];
} SRCFILE;
