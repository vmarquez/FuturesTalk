package futurestalk
import scala.concurrent.{Future, Await}
import scala.concurrent.ExecutionContext.Implicits.global
import scalaz._
import scala.collection._
import Scalaz._
import scala.concurrent.duration._ 

object Futures {
 
  def furueBasics(): Unit = {
    val f1 = Future { 1 + 2 } //happening asynchronously
    val f2 = Future { 3 + 5 } 
    
    val f3 = for {
      a <- f1
      b <- f2
    } yield 
      a + b
    Await.result(f3, Duration(5, SECONDS))
  }

  def processAll[A,B](li: List[A])(f: A => B): (List[A], List[B]) = ???

  def processAllFuture[A,B](li: List[A])(f: A => Future[B]): Future[(List[A],List[B])] = ???

  def doStuff[A](a1: A, a2: A)(f: A => Option[Int]): Future[Option[Int]] = {
    val fi1 = Future { f(a1) }
    val fi2 = Future { f(a2) }
    for {
      o1 <- fi1  //call to map
      o2 <- fi2
    } yield
      for {
        i1 <- o1
        i2 <- o2
      } yield
        i1 + i2
  }
  
  val o1 = Some(2) // or 1.some

  val l1 = List(1,2,3)
  
  val f1 = Future { 1 }

  val r1 = o1.map(i => i*i)

  val r2 = l1.map(i => i*i)

  val r3 = f1.map(i => i*i)

  def squareInContext(s: Seq[Int]): Seq[Int] = 
    s.map(i => i*i)
  
  val range1 = (0 to 3)
  val arr = Array(0,1,2,3)  
  
  //why do I need the =:=  WHO KNOWS???
  def squareInContextF[A <: Seq[Int]](s: A)(implicit ev: Seq[Int] =:= A): A = 
    s.map(i => i*i)
  
  //even if it did work...Option isn't sequence.  Nor is futuer.  :(

  trait Mappable[F[_]] {
    def map[A,B](fa: F[A])(mf: A => B): F[B]
  }

  implicit def listmappable: Mappable[List] = new Mappable[List] {
    def map[A,B](fa: List[A])(mf: A => B): List[B] = fa.map(mf)
  }

  implicit def futuremappable: Mappable[Future] = new Mappable[Future] {
    def map[A,B](fa: Future[A])(mf: A => B): Future[B] = fa.map(mf)
  }

  //what about things that don't even HAVE a map method?
  implicit def functionmappable: Mappable[Function0] = new Mappable[Function0] {
    def map[A,B](f: Function0[A])(mf: A => B): Function0[B] = () => mf(f())
  } 

  def squareInCtxtT[F[_]](s: F[Int])(implicit M: Mappable[F]): F[Int] = 
    M.map(s)(i => i*i)

  def someApiThatSometimesWorks(): Option[String] = {
    Some("123")
  }

  def square(i: Int) = 
    i*i
  
  //let's build upon our mappable trait
  trait FlatMappable[F[_]] extends Mappable[F] {
    def point[A](a: => A): F[A]
    def flatMap[A,B](fa: F[A])(fm: A => F[B]): F[B]
    override def map[A,B](fa: F[A])(m: A => B): F[B] = flatMap(fa)(a => point(m(a)))
  }

  class FlatMappableOps[F[_],A](fa: F[A])(implicit M: FlatMappable[F]) {
    def flatMap[B](f: A => F[B]): F[B] = M.flatMap(fa)(f)
    def map[B](f: A => B): F[B] = M.map(fa)(f)
  }

  implicit def toFlatMappableOps[F[_],A](fa: F[A])(implicit M: FlatMappable[F]) = new FlatMappableOps(fa)

  def addTwoCtxt[F[_]](fa: F[Int])(implicit M: FlatMappable[F]) = M.map(fa)(i => i+2)

  def minusCtxt[F[_]](fa: F[Int])(implicit M: FlatMappable[F]) = M.map(fa)(i => i-1) 

  //now what can we build?
  implicit def optionFlatMappable: FlatMappable[Option] = new FlatMappable[Option] {
    def point[A](a: => A): Option[A] = Option(a)
    def flatMap[A,B](fa: Option[A])(f: A => Option[B]) = fa.flatMap(a => f(a)) 
  }

  implicit def futureFlatMappable: FlatMappable[Future] = new FlatMappable[Future] {
    def point[A](a: => A): Future[A] = Future { a }
    def flatMap[A,B](fa: Future[A])(f: A => Future[B]): Future[B] = fa.flatMap(a => f(a))
  }
 
 //well, now we can start building interesting pipelines!
  import UnsafeJavaWebApi._
  def pipeline[F[_]](fa: F[Int])(implicit M: FlatMappable[F]): F[String] = {
    val newm = M.point(unsafeApi(2))
    M.flatMap(fa)(i => 
         M.flatMap(M.point(unsafeApi(i)))(s => 
            M.flatMap(M.point(unsafeApi2(s+"blah")))(s2 => 
              M.map(newm)(s3 => s3 + s2)
          )
        )
    )
  }
  
  def pipeline2[F[_]](fa: F[Int])(implicit M: FlatMappable[F]): F[String] = {
    val nm = M.point(unsafeApi(2))
    for {
      i   <- fa
      s   <- M.point(unsafeApi(i))
      s2  <- M.point(unsafeApi2(s+"blah"))
      s3  <- nm
    } yield
      s3 + s2 
  }

  def pipeline3[F[_]](fa: F[Int])(implicit M: Monad[F]): F[String] = {
    val nm = M.point(unsafeApi(2))
    for {
      i   <- fa
      s   <- M.point(unsafeApi(i))
      s2  <- M.point(unsafeApi2(s+"blah"))
      s3  <- nm
    } yield
      s3 + s2 
  }

  //we get short circuiting, becuase if null is passed to point, we return none
  def doPipeLine(i: Int): Option[String] =   
    pipeline(Option(1))
  
  //what does this have to do with futures?
  // we get parallelism (becuase of the call to point in the pipeline)
  def doPipelineF(i: Int): Future[String] = 
    pipeline(Future { 1 })
  
  //so what? 
  class OptionHelper[F[_],A](val foa: F[Option[A]]) {
      def flatMap[B](f: A => OptionHelper[F,B])(implicit M: FlatMappable[F]): OptionHelper[F,B] = new OptionHelper[F,B](M.flatMap(foa)(o => o match {
        case Some(a) => f(a).foa
        case None => M.point(None: Option[B])
      }))
  }

  type OptionFutureHelper[A] = OptionHelper[Future,A]
  
  def OptionHelperFutureFlatMappable(implicit M: FlatMappable[Future]): FlatMappable[OptionFutureHelper] = new FlatMappable[OptionFutureHelper] {
    def point[A](a: => A): OptionFutureHelper[A] = new OptionHelper(M.point(Option(a)))
    def flatMap[A,B](fa: OptionFutureHelper[A])(f: A => OptionFutureHelper[B]): OptionFutureHelper[B] = fa.flatMap(f)(M)
  }
  
  //what is happening here?  We are currying the slot here
  implicit def OptionHelperFlatMappable[F[_]](implicit M: FlatMappable[F]): FlatMappable[({ type l[a] = OptionHelper[F,a]})#l] = new FlatMappable[({ type l[a] = OptionHelper[F,a]})#l] {
    def point[A](a: => A): OptionHelper[F,A] = new OptionHelper(M.point(Option(a)))
    def flatMap[A,B](fa: OptionHelper[F,A])(f: A => OptionHelper[F,B]): OptionHelper[F,B] = fa.flatMap(f)
  }

  def runPipelineOptionHelper(): Unit = {
    val oh: OptionFutureHelper[Int] = new OptionHelper[Future,Int](Future { Some(1) })
    pipeline(oh) 
  }

}

object UnsafeJavaWebApi { 
  def unsafeApi(i: Int): String = {
    if (i % 2 == 0)
      null
    else {
      //long computation might occur here
      i+5.toString
    }
  }

  def unsafeApi2(s: String): String = 
    try {
      (s.toInt+1).toString
    } catch {
      case ex: Exception => null 
    }
  
  //\/.fromTryCatchNonFatal just wraps an expression in \/[Exception, A]
  def tryUnsafeApi(i: Int): \/[String, Int] = \/.fromTryCatchNonFatal( i + 2).leftMap(_.toString) 
  def tryUnsafeApi2(s: String): \/[String, String] = \/.fromTryCatchNonFatal(s + "asdf" ).leftMap(_.toString)
}

object FuturesScalaz {
  import UnsafeJavaWebApi._ 
  def pipeline3[F[_]](fa: F[Int])(implicit M: Monad[F]): F[String] = {
    val nm = M.point(unsafeApi(2))
    for {
      i   <- fa
      s   <- M.point(unsafeApi(i))
      s2  <- M.point(unsafeApi2(s+"blah"))
      s3  <- nm
    } yield
      s3 + s2 
  }
  
  def pipeline4[F[_]](fa: EitherT[F, String, Int])(implicit M: Monad[F]): EitherT[F, String, String] = {
     val nm = EitherT(M.point(tryUnsafeApi(2)))
     for {
        i <- fa
        s <- EitherT(M.point(tryUnsafeApi(i)))
        s2 <- EitherT(M.point(tryUnsafeApi2(s + "blah")))
        s3 <- nm
     } yield 
      s3 + s2
  }

  def oPipelineScalaz(i: Int): OptionT[Future,String] = {
    val ot= OptionT( Future { i.some } )
   pipeline3[({ type l[a] = OptionT[Future,a]})#l](ot)
  }

  def doPipelineId(i: Int): OptionT[Identity, String] = {
    val oi = OptionT( i.some.point[Identity] )
    pipeline3[({ type l[a] = OptionT[Identity, a]})#l](oi)
  }

  def dopiplineEither(i: Int): EitherT[Future, String, String] = {
    val et = EitherT( Future { i.right[String] } )
    pipeline4(et)
  }
  
  def separateSequence[F[_], A, B](list: List[EitherT[F,A,B]])(implicit M: Monad[F]): F[(List[A],List[B])] = {
    list.foldRight(M.point((List[A](), List[B]())))( (et, ftup) => {
      for {
        e <- et.run
        t <- ftup
      } yield e.fold(a => (a :: t._1,t._2), b => (t._1, b :: t._2)) 
    })
  }

  //how can we handle something like this? 
  def transformPipeline[F[_], A](l: List[Int], f: EitherT[F, String, Int] => EitherT[F, String, A])(implicit M: Monad[F]): F[(List[String], List[A])] = ???

  def pipelineList[F[_]](l: List[Int])(implicit M: Monad[F]): F[(List[String], List[String])] = {
    val eithers = l.map(i => Monad[({ type l[a] = EitherT[F, String, a]})#l].point(i)) //List[EitherT[Future,String,int]]
    val pipelineResults = eithers.map(et => pipeline4(et))
    separateSequence(pipelineResults)
  }

}




