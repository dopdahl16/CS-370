public class Scheme
{

    //...

    public static Value cons(
        Value car,
        Value cdr
        )
    {

        return new Pair(car, cdr);

        }

    //...

    }
