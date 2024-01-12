x = 100000;
d = 2423423;

def add(a=100, b=132423, d=2222):    
    print(a, b, d);
    a = 119;
    print(a);
    x = d;
    return x;;

print(add(20, 30));

print(x, d);

def global_variable():
    global x;
    global d;
    x = 10;
    d = 150;
    return 0;
    print(43);;

temp = global_variable();
print(temp);
print(x, d);

def fibo(n=1):
    if n == 1:
        return 1;
    else:
        pass;;
    if n == 0:
        return 0;
    else:
        pass;;
    
    return fibo(n-1) + fibo(n-2);;

print(fibo(8));

def lazy():
    print(503);
    return 10;;

def test_lazy(a=1, b=2):
    print(a);
    print(b);
    return;;

temp = test_lazy(1099999, lazy());
print(temp);

