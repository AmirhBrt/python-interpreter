x = 100000;
d = 2423423;
def add(a=100, b=132423, d=2222):    
    print(a, b, d);
    x = d;
    return x;;

print(add(20, 10));
print(x, d);

def global_variable():
    global x;
    global d;
    x = 10;
    d = 150;
    return;
    print(43);;

temp = global_variable();
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

print(fibo(9));