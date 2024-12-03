```
arch -x86_64 gcc -o parser -I/usr/local/Cellar/libantlr3c/3.5.3/include -L/usr/local/Cellar/libantlr3c/3.5.3/lib -lantlr3c main.c CLanguageLexer.c CLanguageParser.c
```

```
./parser input.txt  
```

```
dot -Tpng output.dot -o output.png
```