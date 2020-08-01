package japgolly.scalagraal

import utest._
import js.GraalJs._
import GraalBoopickle._
import TestData._

object GraalBoopickleTest extends TestSuite {

  // https://github.com/scala-js/scala-js/issues/4057
  private lazy val shim = Expr("function nop(){}; scalajsCom={init:nop, send:nop}")
  private lazy val SJS = shim >> Expr.requireFileOnClasspath("ext-boopickle-test-fastopt.js")

  override def tests = Tests {
    val ctx = GraalContext()

//    'one {
//      val expr = for {
//        _ <- Expr.apply1(a => s"a=$a", Example(666, 777))
////        _ <- Expr("b=a.array()")
////        _ <- Expr("console.log(a)")
////        _ <- Expr("console.log(a.array())")
////        _ <- Expr("var i = new Int8Array(a.limit())")
//////        _ <- Expr("var i = new Int8Array(a, 0, a.limit())")
////        _ <- Expr("var j = i.length; while(j-->0) i[j]=b[j]")
//        //i <- Expr("(function(){const i=new Int8Array(a.limit());const b=a.array();let j=i.length;while(j-->0)i[j]=b[j];return i})()")
//        i <- Expr("a")
//      } yield i
//      ctx.eval(expr)
//    }
//
//    'two {
//      val expr = for {
//        _ <- Expr.apply2((a, b) => s"x=[$a, $b]", Example(666, 777), Example(3, 4))
//        _ <- Expr("console.log(x[0])")
//        _ <- Expr("console.log(x[1])")
//      } yield ()
//      ctx.eval(expr)
//    }

    "sjs" - {
      "null" - {
        val v = ctx.eval(SJS >> Expr("example1(null)").asString)
        assert(v == Right("<< null?! >>"))
      }
      "ser" - {
        val e = Expr.fn1("example1")(Example(9999, 3)).asString
        val v = ctx.eval(SJS >> e)
        assert(v == Right("<JS-9999:3>"))
      }
    }

  }
}
