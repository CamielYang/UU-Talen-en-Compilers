class Hello
{
    int a;
    a = 10;

    // Test main function with scope and type errors
    // void main()
    // {
    //     int h;
    //     h = true;

    //     int i;
    //     i = true;

    //     print(test(1));
    //     print3();
    //     print3(true);
    //     printAdd(test(1), test(1));
    // }

    // Test main function with correct scope and type
    void main()
    {
        // Expected output:
        // 1x - 100
        // 2x - 9
        // 1x - 1 to 4
        // 2x - 3628800
        // 2x - 0 to 9
        // 2x - 9
        // 2x - 45
        // 1x - 100
        // machine halted

        // test with global var
        print(a * 10);

        // test with calculations and function calls
        printSquare(rInt(3));
        printAdd(4, 5);
        print3(1, 2, 3);

        // test for and while
        print(fac(10));
        print(facRec(3));
        printFor(10);
        printWhile(10);

        // test with return value and reassign to same var
        int x;
        x = printSquare(rInt(3));
        print(x);
        x = printAdd(rInt(40), rInt(5));
        print(x);

        // test with global var
        print(a * 10);
    }

    void test(int a)
    {
        print(a);
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