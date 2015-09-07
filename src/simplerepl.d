import std.stdio;

import simpleext01;
import simpledatatypes;
import simpleevaluate;

void println(T...)(T args) {
    writeln(args);
}

void main() {
    writeln("ok setting up ENV");
    setupEnvironment();
    eval(read(`(load "src/stdlib.lisp")`));
    const auto QUIT = read("QUIT");
    while(1) {
        write(">> ");
        try {
            auto line = getLine();
            auto obj = read(line);
            //inspectObj(obj);
            auto evalobj = obj.eval(GLOBAL_ENV);
            //writeln(GLOBAL_ENV);
            println(evalobj);
            if (evalobj == QUIT)
                break;
        } catch (Exception exc) {
            writeln(exc.msg);
        }
    }
}
