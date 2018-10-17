```c
typedef void (*sighander_t)(int);

sighander_t singal(int sig, sighander_t handler)
{
    handler(sig);
    return handler;
}

void foo1(int a)
{
    printf("%d\n", a); 
}

int main(int argc, char const *argv[])
{
    singal(5, &foo1);

    return 0;
}
```

