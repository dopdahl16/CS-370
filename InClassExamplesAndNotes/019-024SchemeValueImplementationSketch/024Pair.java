public class Pair extends Value
{

    private Value myCar;
    private Value myCdr;

    public Pair(Value car, Value cdr)
    {

        myCar = car;
        myCdr = cdr;

        }

    public Value car()
    {

        return myCar;

        }

    public Value cdr()
    {

        return myCdr;

        }

    }
