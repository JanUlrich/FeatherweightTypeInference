## Typeinference for Featherweight Java

[Try it here](https://janulrich.github.io/FeatherweightTypeInference/)

### Input Examples

```
class Identity extends Object{
  id(a){
    return a;
  }
}

```


```
class Overloading extends Object{
  m(a, b){return a;}
  m(a,b){return b;}
}

class TestOverloading extends Object{
  test(a){
    return new Test().m(this,a);
  }
}
```


```
class List<A extends Object> extends Object{
  A head;
  List<A> tail;
  add( a){
    return new List(a, this);
  }
  get(){
    return this.head;
  }
}

class PrincipleType extends Object {
  function(a){
    return a.add(this).get();
  }
}

```

