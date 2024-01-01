class Hello
{
    void main()
    {
        printSquare(3);
        printAdd(4, 5);
        print3(1, 2, 3);
    }

    void printSquare(int x)
    {
        print(x * x);
    }

    void printAdd(int x, int y)
    {
        print(x + y);
    }

    void print3(int a, int b, int c)
    {
        print(a);
        print(b);
        print(c);
    }
}