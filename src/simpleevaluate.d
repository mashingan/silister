import simpleread;
import simpleext01;
import simpledatatypes;

import std.stdio;
import std.string:  format, chomp;
import std.variant;
import std.range:   retro;

alias Environment = LispObject[LispObject];

Environment[] GLOBAL_ENV;

void inspectObj (LispObject obj) {
    writefln("---%s info---", obj);
    writeln("Value  : ", obj.value);
    writeln("Type   : ", obj.type);
    writeln("Values : ", obj.values);
}

string report(string file = __FILE__, size_t line = __LINE__) {
    return format("file: %s, line: %s ", file, line);
}

void inspectObj (string funinfo, LispObject obj) {
    write(funinfo, ": ");writefln("---%s info---", obj);
    write(funinfo, ": ");writeln("Value  : ", obj.value);
    write(funinfo, ": ");writeln("Type   : ", obj.type);
    write(funinfo, ": ");writeln("Values : ", obj.values);
}

Variant readyValue (LispObject obj, Environment[] env) {
    return obj.isSelfEval ?
        obj.value : (eval(obj, env)).value;
}

bool isFalse (LispObject obj) {
    return (obj.type == LVAL.NULL ||
            (obj.type == LVAL.LIST && obj.values.length == 0) ||
            (obj.isQuotedList && obj.values.length == 0) ||
            (obj.isQuasiList && obj.values.length == 0)) ?
        true : false;
}

bool isTrue (LispObject obj) {
    return !obj.isFalse;
}

bool isSelfEval (LispObject obj) {
    return (obj.isAtom && !obj.isSymbol) ? true : false;
}

bool isAtom(LispObject obj) {
    return (obj.type != LVAL.LIST && obj.type != LVAL.PAIR &&
            obj.type != LVAL.QUOTE && obj.type != LVAL.LAMBDA &&
            obj.type != LVAL.QUASIQUOTE && obj.type != LVAL.COMMA &&
            obj.type != LVAL.NULL) ?
        true : false;
}

bool isSymbol(LispObject obj) {
    return obj.type == LVAL.SYMBOL ? true : false;
}

bool isErr (LispObject obj) {
    return obj.type == LVAL.ERR ? true : false;
}

string isTypeList (string name, string type) {
    return format("
            bool is%sList (LispObject obj) {
                return (obj.type == LVAL.%s && obj.value == null) ?
                    true : false;
            }", name, type);
}

mixin(isTypeList("Quoted", "QUOTE"));
mixin(isTypeList("Quasi", "QUASIQUOTE"));
mixin(isTypeList("Comma", "COMMA"));

string makeNumerical (string funName, string op) {
    return format(`
            LispObject %s(Environment[] env, LispObject[] numbers...) {
                auto firstobj = numbers[0];
                auto res = readyValue(firstobj, env);
                foreach (number; numbers[1..$]) {
                    auto num = readyValue(number, env);
                    res %s= num;
                }
                return LispObject(res, identify(res.toString));
            }`, funName, op);
}

string makeComparison (string funName, string op) {
    return format(`
            LispObject %s(Environment[] env, LispObject[] numbers...) {
                if (numbers.length < 2)
                    throw new Exception("%s need at least 2 arguments, " ~
                        "supplied " ~ numbers.length.stringof);

                for (int i = 1; i < numbers.length; ++i) {
                    auto num1 = readyValue(numbers[i-1], env);
                    auto num2 = readyValue(numbers[i], env);
                    if (! (num1 %s num2))
                        return LispObject(Variant(), LVAL.NULL);
                }
                return LispObject(Variant("t"), LVAL.SYMBOL);
            }`, funName, funName, op);
}

mixin(makeNumerical("add", "+"));
mixin(makeNumerical("mul", "*"));
mixin(makeNumerical("sub", "-"));
mixin(makeNumerical("div", "/"));

mixin(makeComparison("eqCmp",   "=="));
mixin(makeComparison("gtCmp",   ">" ));
mixin(makeComparison("ltCmp",   "<" ));
mixin(makeComparison("gtEqCmp", ">="));
mixin(makeComparison("ltEqCmp", "<="));
mixin(makeComparison("equal",   "=="));

enum funPrimitive = [&add, &sub, &mul, &div,
                  &setSym, &cons, &car, &cdr,
                  &lambda, &ifForm, &applyForm, &evalForm,
                  &eprogn, &quit, &defun, &let,
                  &list, &defmacro,
                  &print, &println, &load,
                  &equal,
                  &eqCmp, &gtCmp, &ltCmp, &gtEqCmp, &ltEqCmp];
enum symPrimitive = ["+", "-", "*", "/",
                  "setf", "cons", "car", "cdr",
                  "lambda", "if", "apply", "eval",
                  "progn", "quit", "defun", "let",
                  "list", "defmacro",
                  "print", "println", "load",
                  "equal",
                  "=", ">", "<", ">=", "<="];

static assert(funPrimitive.length == symPrimitive.length);

LispObject[] allEval(Environment[] env, LispObject[] theargs...) {
    LispObject[] res;
    foreach (obj; theargs)
        res ~= eval(obj, env);
    return res;
}

LispObject getObj (Environment[] envs, LispObject obj) {
    foreach (env; retro(envs))
        if (obj in env)
            return env[obj];
    return LispObject(Variant(), LVAL.ERR);
}

alias Funlisp = LispObject function(LispObject[]...);

Environment makePrimitive(T)(string[] syms, T[] funs) {
    Environment res;
    if (syms.length != funs.length)
        throw new Exception("makePrimitive: syms and funs not equal");

    for(int i=0; i < syms.length; i++) {
        auto sym = syms[i];
        auto fun = funs[i];
        Variant value = fun;
        auto key = read(sym);
        auto val = LispObject(value, LVAL.FUNCTION);
        res[key] = val;
    }
    return res;
}

void setupEnvironment() {
    Environment env;
    env = makePrimitive(symPrimitive, funPrimitive);
    auto truekey = LispObject(Variant("t"), LVAL.SYMBOL);
    auto trueval = LispObject(Variant(true), LVAL.SYMBOL);
    auto falskey = LispObject(Variant("nil"), LVAL.SYMBOL);
    auto falsval = LispObject(Variant(false), LVAL.NULL);
    env[truekey] = trueval;
    env[falskey] = falsval;
    GLOBAL_ENV ~= env;
}

LispObject print (Environment[] env, LispObject[] objs...) {
    foreach (obj; objs)
        write(obj);
    return LispObject(Variant(), LVAL.NULL);
}

LispObject println(Environment[] env, LispObject[] objs...) {
    print(env, objs);
    writeln();
    return LispObject(Variant(), LVAL.NULL);
}

LispObject setSym(Environment[] env, LispObject[] vars...) {
    auto len = vars.length;
    if (len < 1)
        throw new Exception("SETF: Need minimum 1 argument, supplied 0");

    LispObject newvalue;
    auto sym = vars[0];

    Environment theenv;
    foreach (envi; retro(env))
        if (sym in envi) {
            theenv = envi;
            break;
        }

    if (!theenv)
        theenv = env[0];

    debug writef("SETF: setting %s with type %s ", sym, sym.type);
    if (len == 1) {
        newvalue = LispObject(Variant(), LVAL.NULL);
        theenv[sym] = newvalue;
        debug writeln(" with ", newvalue);
    } else if (len == 2) {
        newvalue = eval(vars[1], env);
        theenv[sym] = newvalue;
        debug writeln(" with ", newvalue);
    } else {
        newvalue = eval(vars[1], env);
        theenv[sym] = newvalue;
        debug writeln(" with ", newvalue);
        return setSym(env, vars[2..$]);
    }

    return newvalue;
}

void quote(ref LispObject obj) {
    debug writefln("%s is invoking quote", obj);
    auto len = obj.values.length;
    debug {
        writefln("%s has %s values members", obj, len);
        if (!obj.isQuotedList)
            writefln("%s is symbolic quote", obj);
        else
            writefln("%s is list quote", obj);
        auto theobj = len > 0 ? obj.values[0] : obj;
        writefln("The extracted obj is %s with %s",
                theobj, theobj.type);
    }
    if (obj.value != null) {
        obj = read(obj.value.toString);
        if (!obj.isAtom && obj.values.length > 0)
            obj.type = LVAL.QUOTE;
    } else
        obj.type = LVAL.LIST;
}

LispObject list (Environment[] env, LispObject[] objs...) {
    auto elements = allEval(env, objs);
    return LispObject(Variant(null), LVAL.LIST, elements);
}

LispObject cons (Environment[] env, LispObject[] objs...) {
    if (objs.length != 2)
        throw new Exception("Cons: must supply two arguments.");

    auto thecar = objs[0].isSelfEval? objs[0] : objs[0].eval(env);
    auto thecdr = objs[1].isSelfEval? objs[1] : objs[1].eval(env);
    Variant val = null;
    LVAL type;
    LispObject[] values;

    debug writeln("car: ", thecar.type);
    debug writeln("cdr: ", thecdr.type);
    if (thecdr.type == LVAL.NULL) {
        debug writeln("cdr NULL");
        values = [thecar];
        type = LVAL.LIST;
    } else if (thecdr.isAtom) {
        debug writeln("cdr ATOM");
        values = [thecar, thecdr];
        type = LVAL.PAIR;
    } else {
        debug writeln("cdr LIST");
        values = thecar ~ thecdr.values;
        type = LVAL.LIST;
    }

    return LispObject(val, LVAL.QUOTE, values);
}

auto notValidCxrArg(LispObject[] objs...) {
    string message;
    if (objs.length != 1)
        message = format ("Need one arguments, supplied: %s", objs.length);

    auto obj = objs[0];
    if (obj.isSelfEval && obj.type != LVAL.QUOTE)
        message = "Need argument a list, supplied atom";
    else if (obj.type == LVAL.QUOTE && obj.values.length < 1)
        message = "Need a list quoted argument.";

    return message;
}

LispObject car (Environment[] env, LispObject[] objs...) {
    if(auto msg = notValidCxrArg(objs))
        throw new Exception(msg);

    auto obj = eval(objs[0], env);
    LispObject res;
    version (Debug) {
        writefln("%s has type %s", objs[0], objs[0].type);
        writefln("After eval, got %s with type %s", obj, obj.type);
        writefln("%s has values: %s with total members %s",
                obj, obj.values, obj.values.length);
    }
    if (obj.type == LVAL.NULL) {
        debug writeln("CAR: return null");
        res = obj;
    } else if (obj.isSelfEval) {
        debug writeln("CAR: return self-eval");
        res = obj;
    } else if (obj.isSymbol) {
        debug writeln("(CAR: return symbol");
        res = obj;
    } else if (obj.values.length > 0) {
        debug writeln("CAR: return listed obj");
        if (obj.values[0].isAtom)
            res = read(obj.values[0].value.toString);
        else
            res = obj.values[0];
    } else {
        throw new Exception("CAR: need " ~ obj.toString ~
                " of type LIST, supplied type " ~
                obj.type.stringof);
    }

    return res;
}

LispObject cdr (Environment[] env, LispObject[] objs...) {
    if (auto msg = notValidCxrArg(objs))
        throw new Exception(msg);

    auto obj = eval(objs[0], env);
    debug {
        writefln("CDR: %s type is %s", obj, obj.type);
        writeln("CDR: with values: ", obj.values);
    }
    LispObject res;
    if (obj.type == LVAL.NULL)
        res = obj;
    else if (obj.isSelfEval)
        res = obj;
    else if (obj.values.length < 1)
        res = LispObject(Variant(), LVAL.NULL);
    else
        res = lispObject(obj.value, LVAL.QUOTE, obj.values[1..$]);

    return res;
}

void extendEnv(ref Environment resenv, Environment[] from = GLOBAL_ENV) {
    foreach (env; from)
        foreach (key, val; env)
            resenv[key] = val;
}

Environment makeEnvironment(LispObject[] keys, LispObject[] vals) {
    Environment env;
    foreach (idx, key; keys)
        env[key] = vals[idx];
    return env;
}

alias TheLambda = LispObject delegate(Environment[], LispObject[]...);

int findKey (string keyval, LispObject[] allformals) {
    if (allformals.length > 1 &&
            allformals[$-2].toString == keyval)
        return allformals.length - 2;
    else
        return -1;
}

string makeFunBody (bool isLambda = true) {
    auto thekey = isLambda? "&rest" : "&body";
    auto withRestargs = isLambda?
        "for (int i = 0; i < pos; ++i)
            allargs ~= eval(args[i], lenv);
        restargs = allEval(lenv, restargs);
        allargs ~= LispObject(Variant(null), LVAL.QUOTE,
                restargs);":
        "allargs ~= args[0..pos];";
    auto noRestargs = isLambda?
        "allargs = allEval(lenv, args);" :
        "allargs = args;";

    auto funname = isLambda? "LAMBDA" : `DEFMACRO`;

    return format(`
    auto fun = delegate LispObject(Environment[] lenv,
            LispObject[] args...)
    {
        LispObject[] formals, restargs;
        auto pos = findKey("%s", formal.values);
        if (pos != -1) {
            if (args.length < pos)
                throw new Exception(report() ~ "Mismatched formals supplied");
            foreach (idx, form; formal.values)
                if (idx != pos)
                    formals ~= form;
            restargs = args[pos .. $];
        }
        else if (pos == -1) {
            if (args.length != formal.values.length)
                throw new Exception(report() ~ "Mismatched formals supplied");
            formals = formal.values;
        }

        debug {
            auto funname = "%s";
            writeln(funname, ": got &rest key pos at ", pos);
            writeln(funname, ": formals  = ", formals);
            writeln(funname, ": restargs = ", restargs);
        }

        LispObject[] allargs;
        if (restargs) {
            %s
        } else
            %s

        auto locenv = makeEnvironment(formals, allargs);
        debug {
            writeln("%s: allargs = ", allargs);
            foreach (formval; formals)
                writeln(formval, " going to be key with type ",
                        formval.type);
            writeln("the local env: ", locenv);
            foreach (key, lnv; locenv)
                writeln(key, " has type ", key.type);
        }

        if (bodies.length == 1) {
            auto obj = bodies[0];
            return eval(obj, lenv ~ locenv);
        } else {
            return eprogn(lenv ~ locenv, bodies);
        }
    };`, thekey, funname, withRestargs, noRestargs, funname);
}

LispObject lambda(Environment[] env, LispObject[] objs...) {
    debug writeln("invoking lambda");
    if (objs.length < 2)
        throw new Exception("Lambda: At least provide 2 arguments,"
                ~ " supplied: " ~ objs.length.stringof);

    auto formal = objs[0];
    auto bodies = objs[1..$];

    if (formal.isAtom)
        throw new Exception("Lambda: not list of formal");

    mixin(makeFunBody(true));

    auto obj = lispObject(Variant(fun), LVAL.FUNCTION);
    debug writefln("From lambda: return %s -> %s", obj, obj.type);
    return obj;
}

LispObject let (Environment[] env, LispObject[] objs...) {
    if (objs.length < 1)
        throw new Exception("LET: need minimum one argument list, "
                ~ " supplied 0");

    auto vars = objs[0];
    if (vars.isAtom)
        throw new Exception("LET: " ~ vars.toString ~ " is not list");

    auto bodies = objs[1..$];

    LispObject[] formals;
    LispObject[] args;
    foreach (var; vars.values) {
        debug writefln("%s for formal with type %s", var, var.type);
        if (var.isAtom) {
            formals ~= var;
            args ~= LispObject(Variant(), LVAL.NULL);
        } else {
            var.type = LVAL.QUOTE;
            var.value = null;
            formals ~= car(env, var);
            args ~= car(env, cdr(env, var));
        }
    }
    debug {
        writeln("LET: got formals ", formals);
        writeln("LET: got args ", args);
    }

    bodies =  LispObject(Variant(null), LVAL.LIST, formals) ~ bodies;
    debug writeln("LET: got bodies ", bodies);

    auto funobj = lambda(env, bodies);
    return apply(env, funobj, args);
}

LispObject ifForm (Environment[] env, LispObject[] objs...) {
    if (objs.length < 2)
        throw new Exception("If form need at least 2 expression, "
                ~ " supplied " ~ objs.length.stringof);
    else if (objs.length > 3)
        throw new Exception("If form need maximum 3 expression, "
                ~ " supplied " ~ objs.length.stringof);

    auto testform = eval(objs[0], env);
    return testform.isTrue?
        eval(objs[1], env) : objs.length == 3 ?
        eval(objs[2], env) : LispObject(Variant(), LVAL.NULL);
}

LispObject eprogn (Environment[] env, LispObject[] objs...) {
    LispObject ret;
    foreach (idx, obj; objs) {
        if (idx == objs.length - 1)
            ret = eval(obj, env);
        else
            eval(obj, env);
    }
    return ret;
}

LispObject quit (Environment[] env, LispObject[] objs...) {
    return LispObject(Variant("QUIT"), LVAL.SYMBOL);
}

LispObject defun (Environment[] env, LispObject[] objs...) {
    if (objs.length < 2)
        throw new Exception("Defun need at least 2 arguments, supplied "
                ~ objs.length.stringof);

    auto name = objs[0];
    if (!name.isSymbol)
        throw new Exception("Defun: " ~ name.toString ~
                " is not a symbol, it's " ~ name.type.stringof);

    auto formals = objs[1];
    if (formals.isAtom)
        throw new Exception("Defun: " ~ formals.toString ~
                " is not a list, it's " ~ formals.type.stringof);

    auto bodies = lambda(env, objs[1..$]);
    debug writefln("The %s has bodies: %s with type %s", name,
            bodies, bodies.type);
    auto obj = setSym(env, [name, bodies]);
    debug writefln("The obj is: %s with type %s", obj, obj.type);
    return obj;
}

LispObject quasiquote (LispObject obj, Environment[] env) {
    debug inspectObj("QUASIQUOTE", obj);
    LispObject res;

    LispObject[] objs;
    if (obj.isQuasiList) {
        debug  writeln("QUASIQUOTE: going in quasi list direction");
        foreach (idx, theobj; obj.values) {
            debug writefln("QUASIQUOTE: obj.values[%s] is %s" ~
                    " with type %s", idx, theobj, theobj.type);
            if (theobj.type == LVAL.COMMA) {
                auto tobeExpanded = (!theobj.isCommaList &&
                        theobj.value.toString == expandToken) ?
                    true : false;
                auto commares = theobj.comma(env);

                debug {
                    writeln("QUASIQUOTE: theobj is expandToken? ",
                            tobeExpanded);
                    inspectObj("QUASIQUOTE", commares);
                }

                if (commares.isSelfEval)
                    objs ~= commares;
                else if (tobeExpanded)
                    objs ~= commares.values;
                else
                    objs ~= commares;
            } else if (!theobj.isAtom) {
                objs ~= theobj.quasiquote(env);

            } else {
                objs ~= theobj;
            }
        }
        debug writeln("QUASIQUOTE: now objs is ", objs);
        res.value  = Variant(null);
        res.type   = LVAL.LIST;
        res.values = objs;
    } else if (obj.value != null && obj.value.toString.length > 1 &&
            obj.value.toString[0..2] == ",(")
    {
        debug writeln("QUASIQUOTE: going into comma-eval direction");
        auto val = obj.value.toString[1..$];
        res = eval(read(val), env);
    } else {
        debug {
            writeln("QUASIQUOTE: going into other direction");
            inspectObj("QUASIQUOTE", obj);
        }
        if (obj.isAtom) {
            debug writeln("QUASIQUOTE: the obj is atom");
            res = obj;
        } else if (obj.type == LVAL.COMMA) {
            debug writeln("QUASIQUOTE: evaluating the comma");
            res = obj.comma(env);
        } else if (obj.toString == commaToken) {
            debug writeln("QUASIQUOTE: evaluating comma-symbol");
            obj = read(obj.toString);
            res = obj.comma(env);
        } else if (obj.values.length > 0) {
            LispObject[] accres;
            foreach (theobj; obj.values) {
                debug inspectObj("QUASIQUOTE: theobj", theobj);
                if (theobj.type == LVAL.LIST || theobj.type == LVAL.QUOTE)
                    theobj.type = LVAL.QUASIQUOTE;
                accres ~= theobj.quasiquote(env);
            }
            res = LispObject(Variant(null), obj.type, accres);
        }
        else {
            debug writeln("QUASIQUOTE: couldn't be identified");
            res = LispObject(Variant(), LVAL.NULL);
        }
        debug inspectObj("QUASIQUOTE", res);
    }

    debug writefln("QUASIQUOTE: now res %s with type %s",
            res, res.type);

    return res;
}

LispObject comma (LispObject obj, Environment[] env) {
    debug {
        writeln("COMMA: obj is ", obj);
        writeln("COMMA: is obj comma listed? ",
                obj.isCommaList ? "yes" : "no");
    }
    LispObject res;

    LispObject[] objs;
    if (obj.isCommaList) {
        debug writeln("COMMA: going to comma list direction");
        foreach (theobj; obj.values) {
            debug writefln("COMMA: theobj is %s with type %s",
                    theobj, theobj.type);
            objs ~= theobj;
        }
        res = eval(LispObject(Variant(null), LVAL.LIST, objs), env);
    } else if (obj.value.toString == expandToken) {
        debug writeln("COMMA: going to expand list direction");
        auto bodiesname = (obj.value.toString)[1..$];
        auto bodiesobj  = read(bodiesname);
        debug writefln("COMMA: bodiesobj is %s with type %s",
                bodiesobj, bodiesobj.type);
        res = eval(bodiesobj, env);
    } else
        res = eval(read(obj.value.toString), env);
    
    debug writefln("COMMA: res is %s with type %s", res, res.type);
    return res;
}

LispObject defmacro (Environment[] env, LispObject[] objs...) {
    if (objs.length < 2)
        throw new Exception(report() ~ "DEFMACRO: need at least 2 arguments,"
                ~ " supplied " ~ objs.length.stringof);

    auto name = objs[0];
    if (!name.isSymbol)
        throw new Exception(report() ~ "DEFMACRO: " ~ name.toString ~
                " need to be a symbol, supplied " ~
                name.type.stringof);

    auto formal = objs[1];
    if (formal.isAtom)
        throw new Exception(report() ~ "DEFMACRO: " ~ formal.toString ~
                " need to a list, supplied " ~
                formal.type.stringof);

    auto bodies = objs[2..$];

    debug {
        writeln("DEFMACRO: formal are ", formal);
        writeln("DEFMACRO: bodies are ", bodies);
    }

    mixin(makeFunBody(false));

    auto macrobodies = LispObject(Variant(fun), LVAL.FUNCTION);
    debug writefln("The %s has bodies: %s with type %s", name,
            macrobodies, macrobodies.type);
    auto obj = setSym(env, [name, macrobodies]);
    debug writefln("The obj is: %s with type %s", obj, obj.type);
    return obj;
}

LispObject load (Environment[] env, LispObject[] objs...) {
    foreach (obj; objs) {
        auto filename = obj.readyValue(env).toString[1..$-1];
        scope (failure) {
            debug writeln(report() ~ " LOAD: cannot open " ~ filename);
            return LispObject(Variant(), LVAL.ERR);
        }

        File newin = File(filename, "rb");
        debug writeln("LOAD: open " ~ filename ~ " is success");

        scope (exit) {
            newin.close();
            debug writeln(report() ~ " LOAD: exit successfully");
        }

        string line;
        while (!newin.eof) {
            auto theline = newin.readln.chomp;
            line ~= theline;
            debug write("LOAD: read newin ", line);
        }
        string[] expr;
        while (line.length > 0) {
            expr ~= getExpr(line);
            debug write("LOAD: now line ", line);
        }
        env.allEval(readall(expr));
    }
    return LispObject(Variant("t"), LVAL.SYMBOL);
}

LispObject evalForm (Environment[] env, LispObject[] objs...) {
    if (objs.length > 1)
        throw new Exception("Eval need maximum 1 argument, supplied "
                ~ objs.length.stringof);
    auto lobs = objs[0].eval(env);
    return lobs.eval(env);
}

LispObject applyForm (Environment[] env, LispObject[] objs...) {
    if (objs.length < 1)
        throw new Exception("Apply need minimum of 1 argument, supplied 0");
    return apply(env, objs[0], objs[1..$]);
}

LispObject eval (LispObject obj, Environment[] env = GLOBAL_ENV) {
    debug {
        writefln("Evaluating: %s", obj);
        writefln("--> with type: %s", obj.type);
    }
    if (obj.type == LVAL.QUASIQUOTE) {
        debug writeln("EVAL:----------evaluating QUASIQUOTE");
        return obj.quasiquote(env);
    } else if (obj.type == LVAL.COMMA) {
        throw new
            Exception("EVAL: comma only works in quasiquote expression");
    } else if (obj.type == LVAL.QUOTE) {
        debug writeln("EVAL: ----------evaluating QUOTE");
        quote(obj);
        debug writefln("After quote now %s.type is %s", obj, obj.type);
        return obj;
    } else if (obj.isAtom) {
        auto newobj = getObj(env, obj);
        if (obj.isSelfEval) {
            debug writeln("EVAL: ----------evaluating SELF-EVAL");
            return obj;
        } else if (newobj.isErr) {
            throw new
                Exception(report() ~ "No binding for " ~ obj.toString);
        }
        debug writeln("EVAL: ----------evaluating ATOM");
        return newobj;
    } else {
        LispObject op;
        if (obj.values.length <= 0)
            throw new Exception(report() ~ " Invalid list expression: " ~
                    obj.toString);
        else
            op = obj.values[0];
        auto args = obj.values[1..$];

        debug writeln("EVAL: op type is ", op.type);
        if (op.type == LVAL.LAMBDA) {
            debug writeln("EVAL: type lambda ok");
            auto thefun = lambda(env, op.values[1..$]);
            return apply(env, thefun, args);
        } else if (op.type == LVAL.FUNCTION) {
            debug writeln("EVAL: type function ok");
            return apply(env, op, args);
        } else {
            auto theop = getObj(env, op);
            debug inspectObj("EVAL", theop);
            if (!theop.isErr) {
                debug writeln("EVAL: ----------evaluating OP in ENV");
                return apply(env, op, args);
            } else {
                throw new Exception(report() ~
                        "EVAL: cannot apply " ~ op.toString());
            }
        }
    }
}

LispObject apply (Environment[] env, LispObject symfun,
        LispObject[] listing...)
{
    debug writefln("APPLY %s with args: %s", symfun, listing);
    if (symfun.type == LVAL.FUNCTION) {
        debug writeln("APPLY: FUNCTION");
        auto fun = *(symfun.value.peek!TheLambda);
        auto res = fun(env, listing);
        debug writefln("==>APPLY %s with result: %s -> %s",
                symfun, res, typeof(res).stringof);
        return res;
    } else {
        auto fun = getObj(env, symfun);
        if (!fun.isErr) {
            auto val = fun.value(env, listing);
            debug writefln("==>APPLY %s with result: %s -> %s",
                    symfun, val, typeof(val).stringof);
            auto lob = *(val.peek!LispObject);
            return lob;
        } else {
            throw new Exception("No function " ~
                    symfun.toString ~ ".");
        }
    }
}
