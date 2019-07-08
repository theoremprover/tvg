int g(int x)
{
    if(x>5)
        return(2*x);

    else
    {
        x = x-- + 2;
    }

    return(x+10);
}

int f(int x)
{
    int z = 2*g(x);

    int y = x>100 ? 2*z : 3*z;

    if(y>10)
    {
        return(g(y));
    }
    else
    {
		y++;
        if(z>5)
        {
            y = z++ > 10000 ? g(z) : g(z+2);
        }
    }

    return(y+1);
}

int main()
{
    short q[4][3][2] = {  // Initializers not nested deeply enough
	{ 1 },
	{ 2, 3 },
    { 4, 5, 6 },
	{ { 4,5}, {5,6}, {6,7} } };

    short q1[2] = // Too many initializers
	{ 1,2,3 };

long q2[4][5] = {  
	{ 21, 2, 45545, 3, 64563 },
	{ 2, 3, 2313, 33, 8734 },
	{ 304, 13222, 3, 454333 },   // Too few initializers here
	{ 4, 5, 16, 63400, 4594 } };

	int q4[][2] = {  // unknown size
	{ 1,2 }, { 3,4 }, { 5,6 } };

/*  THIS IS DETECTED BY THE PREPROCESSOR
    short q3[2] = {  // Initializers nested deeper than array dimensions
	{ 1, 2, 3 },
	{ 2, 3, 3 } };
*/

	int q5[3][2] = { { 1,2 }, { 3,4 }, { 5,6 } }; // Correct
	
    int x;
    x= 6;
    x=-2;
    x=-9;
}
