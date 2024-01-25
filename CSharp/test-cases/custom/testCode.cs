class Hello
{
	// Expected output:
	// 100
	// 9
	// 9
	// 1
	// 2
	// 3
	// 4
	// 132
	// 3628800
	// 3628800
	// 0
	// 1
	// 2
	// 3
	// 4
	// 5
	// 6
	// 7
	// 8
	// 9
	// 0
	// 1
	// 2
	// 3
	// 4
	// 5
	// 6
	// 7
	// 8
	// 9
	// 9
	// 9
	// 45
	// 45
	// 90
	// machine halted

	int a;
	int b;
	b = 11;
    int c;
	c = 12;

    void main()
	{
		a = 10;

		// test with global var
		print(a * 10);

		// test with calculations and function calls
		printSquare(rInt(3));
		printAdd(4, 5);
		print3(1, 2, 3);

		// test for and while
		print(fac(10));
		print(facRec(10));
		printFor(10);
		printWhile(10);

		// test with return value and reassign to same var
		int x;
		x = printSquare(rInt(3));
		print(x);
		x = printAdd(rInt(40), rInt(5));
		print(x);

		// test with global var
		a = 9;
		print(a * 10);
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