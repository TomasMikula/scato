package scato
package benchmarks

import scalaz._
import scalaz.Id._
import scalaz.Free._
import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Fork, Mode}

@Fork(1)
@BenchmarkMode(Array(Mode.Throughput))
class Interpreters {

  def leftAssocFlatMap[F[_]](n: Int)(implicit F: Monad[F]): StateT[F, Int, Int] =
    (1 to n).foldLeft(StateT[F, Int, Int](n => F.point((n, n))))((st, _) =>
      st.flatMap(i => StateT(_ => F.point((i-1, i-1))))
    )

  def rightAssocFlatMap[F[_]](implicit F: Monad[F]): StateT[F, Int, Int] =
    StateT[F, Int, Int](n => F.point((n-1, n-1))).flatMap(i =>
      if(i > 0) rightAssocFlatMap
      else StateT[F, Int, Int](n => F.point((n, n)))
    )

  // strict monad (Id), don't need stack-safety
  @Benchmark def strict_unsafe_left =
    leftAssocFlatMap[Id](100).run(-1)

  // strict monad (Id), don't need stack-safety
  @Benchmark def strict_unsafe_right =
    rightAssocFlatMap[Id].run(100)

  // strict monad (Id), don't need stack-safety, instance reused 100x
  @Benchmark def strict_unsafe_reused_left = {
    val st = leftAssocFlatMap[Id](100)
    (1 to 100).foreach { _ => st.run(-1) }
  }

  // strict monad (Id), don't need stack-safety, instance reused 100x
  @Benchmark def strict_unsafe_reused_right = {
    val st = rightAssocFlatMap[Id]
    (1 to 100).foreach { _ => st.run(100) }
  }

  // strict monad (Id), need stack-safety (thus runRec)
  @Benchmark def strict_safe_left =
    leftAssocFlatMap[Id](100000).runRec(-1)

  // strict monad (Id), need stack-safety (thus runRec)
  @Benchmark def strict_safe_right =
    rightAssocFlatMap[Id].runRec(100000)

  // strict monad (Id), need stack-safety (thus runRec), instance reused 100x
  @Benchmark def strict_safe_reused_left = {
    val st = leftAssocFlatMap[Id](100000)
    (1 to 100).foreach { _ => st.runRec(-1) }
  }

  // strict monad (Id), need stack-safety (thus runRec), instance reused 100x
  @Benchmark def strict_safe_reused_right = {
    val st = rightAssocFlatMap[Id]
    (1 to 100).foreach { _ => st.runRec(100000) }
  }

  // monad already trampolined (Trampoline)
  @Benchmark def trampolined_left =
    leftAssocFlatMap[Trampoline](100000).run(-1).run

  // monad already trampolined (Trampoline)
  @Benchmark def trampolined_right =
    rightAssocFlatMap[Trampoline].run(100000).run

  // monad already trampolined (Trampoline), instance reused 100x
  @Benchmark def trampolined_reused_left = {
    val st = leftAssocFlatMap[Trampoline](100000)
    (1 to 100).foreach { _ => st.run(-1).run }
  }

  // monad already trampolined (Trampoline), instance reused 100x
  @Benchmark def trampolined_reused_right = {
    val st = rightAssocFlatMap[Trampoline]
    (1 to 100).foreach { _ => st.run(100000).run }
  }

}
