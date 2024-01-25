class Hello
{
	int a;
	int b;
	b = 11;
    int c;
	c = 12;

    void main()
	{
		int h;
		h = true;

		int i;
		i = true;

		print(test(1));
		print3();
		print3(true);
		printAdd(test(1), test(1));
	}

	void test(int a)
	{
		print(a);
		print(b * c);
	}

	void print3(int x, int y, int z)
	{
		print(x, y, z);

		// test correct next value with same arg name
		test(z + 1);
	}

	int printSquare(int x)
	{
		print(x * x);

		return x * x;
	}

	int printAdd(int x, int y)
	{
		print(x + y);

		return x + y;
	}

	int fac(int x)
	{
		int r; int t;
		t = 1; r = 1;
		while (t <= x)
		{
			r = r * t;
			t = t + 1;
		}
		return r;
	}

	int facRec(int x)
	{
		if (x == 1)
		{
			return 1;
		}
		else
		{
			return x * facRec(x - 1);
		}
	}

	void printFor(int x)
	{
		int i;
		for (i = 0; i < x; i = i + 1)
		{
			print(i);
		}
	}

	void printWhile(int x)
	{
		int i;
		i = 0;
		while (i < x)
		{
			print(i);
			i = i + 1;
		}
	}

	int rInt(int x)
	{
		return x;
	}
}