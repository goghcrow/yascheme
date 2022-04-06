package xiao.lang;

import org.openjdk.jmh.annotations.*;
import org.openjdk.jmh.runner.Runner;
import org.openjdk.jmh.runner.RunnerException;
import org.openjdk.jmh.runner.options.Options;
import org.openjdk.jmh.runner.options.OptionsBuilder;
import xiao.lang.expander.CompiledExpression;

import java.util.concurrent.TimeUnit;

import static xiao.lang.Test.resource;

//@BenchmarkMode(Mode.AverageTime)
@BenchmarkMode(Mode.SingleShotTime)
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@State(Scope.Thread)
public class Bench {

    @Benchmark
    public Object bench() {
        String s = resource("/tmp.ss");
        CompiledExpression compiled = Interp.compile(s);
        return compiled.eval();
    }

    public static void main(String[] args) throws RunnerException {
        Options opt = new OptionsBuilder()
                .include(Bench.class.getSimpleName())
                .forks(1)
                .warmupIterations(5)
                .measurementIterations(5)
                .build();
        new Runner(opt).run();
    }
}
