import std.variant;

import simpleext01;

struct LispObject {

    Variant value;
    LVAL type;
    LispObject[] values;

    string toString() {
        string ret;
        switch (type) {
            case LVAL.QUOTE:
            case LVAL.QUASIQUOTE:
            case LVAL.COMMA:
                if (value != null)
                    ret = value.toString;
                else
                    ret = printList(values);
                break;

            case LVAL.LAMBDA:
            case LVAL.PAIR:
            case LVAL.LIST:
                ret = printList(values);
                break;

            case LVAL.NULL:
                ret = "()";
                break;

            case LVAL.ERR:
                ret = "ERROR";
                break;

            case LVAL.CHAR:
            default:
                ret = value.toString;
        }
        return ret;
    }

    private string printList(LispObject[] objs) {
        string ret = "(";
        foreach (idx, val; objs) {
            ret ~= val.toString;
            if (idx != objs.length - 1)
                ret ~= " ";
        }
        ret ~= ")";
        return ret;
    }
}

LispObject lispObject(T)(T value, LVAL type, LispObject[] values = [])
{
    Variant val = value;
    return LispObject(value, type, values);
}
