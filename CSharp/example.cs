class Hello
{
    int a;
    a = 10;

    void test(int a)
    {
        print(a);
    }

    void print3(int a, int b, int c)
    {
        print(a, b, c);

        // test correct next value with same arg name
        test(c + 1);
    }

    void printSquare(int x)
    {
        print(x * x);
    }

    void printAdd(int x, int y)
    {
        print(x + y);
    }

    void printFor(int n)
    {
        int i;
        for (i = 0; i < n; i = i + 1)
        {
            print(i);
        }
    }

    void printWhile(int n)
    {
        int i;
        i = 0;
        while (i < n)
        {
            print(i);
            i = i + 1;
        }
    }

    void main()
    {
        print(a * 10);

        printSquare(3);
        printAdd(4, 5);
        print3(1, 2, 3);

        printFor(10);
        printWhile(10);

        print(a * 10);
    }
}