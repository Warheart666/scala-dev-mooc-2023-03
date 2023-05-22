package module1

import module1.list.List

import java.util.UUID
import scala.annotation.tailrec
import java.time.Instant
import scala.language.postfixOps



/**
 * referential transparency
 */


 object referential_transparency{

  case class Abiturient(id: String, email: String, fio: String)

  type Html = String

  sealed trait Notification

  object Notification{
    case class Email(email: String, text: Html) extends Notification
    case class Sms(telephone: String, msg: String) extends Notification
  }


  case class AbiturientDTO(email: String, fio: String, password: String)

  trait NotificationService{
    def sendNotification(notification: Notification): Unit
    def createNotification(abiturient: Abiturient): Notification
  }


  trait AbiturientService{

    def registerAbiturient(abiturientDTO: AbiturientDTO): Abiturient
  }

  class AbiturientServiceImpl(val notificationService: NotificationService) extends AbiturientService{
    override def registerAbiturient(abiturientDTO: AbiturientDTO): Abiturient = {
      val notification = Notification.Email("", "")
      val abiturient = Abiturient(UUID.randomUUID().toString, abiturientDTO.email, abiturientDTO.fio)
      //notificationService.sendNotification(notification)
      // save(abiturient)
      abiturient
    }
  }


}


 // recursion

object recursion {

  /**
   * Реализовать метод вычисления n!
   * n! = 1 * 2 * ... n
   */

  def fact(n: Int): Int = {
    var _n = 1
    var i = 2
    while (i <= n){
      _n *= i
      i += 1
    }
    _n
  }

  def factRec(n: Int): Int =
    if(n <= 0) 1 else n * factRec(n - 1)

  def factRecTail(n: Int): Int = {
    @tailrec
    def loop(i: Int, acc: Int): Int = {
      if(i <= 0) acc
      else loop(i - 1, n * acc)
    }
    loop(n, 1)
  }



  /**
   * реализовать вычисление N числа Фибоначчи
   * F0 = 0, F1 = 1, Fn = Fn-1 + Fn - 2
   *
   */


}

object hof{


  // обертки

  def logRunningTime[A, B](f: A => B): A => B = a => {
    val start = System.currentTimeMillis()
    val r = f(a)
    val end = System.currentTimeMillis()
    println(end - start)
    r
  }

  def doomy(string: String): Unit = {
    Thread.sleep(1000)
    println(string)
  }

  val lDoomy: String => Unit = logRunningTime(doomy)



  // изменение поведения ф-ции

  val arr = Array(1, 2, 3, 4, 5)

  def isOdd(i: Int): Boolean = i % 2 > 0

  def not[A](f: A => Boolean): A => Boolean = x => !f(x)

  val isEven: Int => Boolean = not(isOdd)

  isOdd(2)
  isEven(3)

  // изменение самой функции

  def partial[A, B, C](a: A, f: (A, B) => C): B => C = b => f(a, b)

  def sum(x: Int, y: Int): Int  = x + y

  val p: Int => Int = sum(2, _)

  p(2) // 4
  p(3) // 5
















  trait Consumer{
       def subscribe(topic: String): LazyList[Record]
   }

   case class Record(value: String)

   case class Request()

   object Request {
       def parse(str: String): Request = ???
   }

  /**
   *
   * (Опционально) Реализовать ф-цию, которая будет читать записи Request из топика,
   * и сохранять их в базу
   */
   def createRequestSubscription() = ???



}






/**
 *  Реализуем тип Option
 */


 object opt {


  /**
   *
   * Реализовать структуру данных Option, который будет указывать на присутствие либо отсутсвие результата
   */

  // Animal -> Dog
  // Covariant + отношения переносятся на контейнер
  // Contravariant - отношения переносятся на контейнер наоборот
  // Invariant - нет отношений
  type Dog

  sealed trait Option[+T]{
    def isEmpty: Boolean = this match {
      case Option.Some(v) => false
      case Option.None => true
    }

    def map[B](f: T => B): Option[B] =
      flatMap(v => Option.Some(f(v)))

    def flatMap[B](f: T => Option[B]): Option[B] = this match {
      case Option.Some(v) => f(v)
      case Option.None => Option.None
    }


    /**
     *
     * Реализовать метод printIfAny, который будет печатать значение, если оно есть
     */
    def printIfAny(): Unit = this match {
      case Option.Some(v) => println(v)
      case _ =>
    }


    /**
     *
     * Реализовать метод zip, который будет создавать Option от пары значений из 2-х Option
     */

    def zip[B](v: Option[B]): Option[(T, B)] = {
      (this, v) match {
        case (Option.Some(_), Option.Some(value)) => this.map(th => (th, value))
        case _ => Option.None
      }
    }


    /**
     *
     * Реализовать метод filter, который будет возвращать не пустой Option
     * в случае если исходный не пуст и предикат от значения = true
     */


    def filter(p: T => Boolean): Option[T] = this match {
      case Option.Some(v) if p(v) => Option.Some(v)
      case _ => Option.None
    }
  }

  object Option{

    case class Some[T](v: T) extends Option[T]
    case object None extends Option[Nothing]
  }



 }

 object list {
   /**
    *
    * Реализовать односвязанный иммутабельный список List
    * Список имеет два случая:
    * Nil - пустой список
    * Cons - непустой, содердит первый элемент (голову) и хвост (оставшийся список)
    */

    trait List[+T] {


     /**
      * Метод cons, добавляет элемент в голову списка, для этого метода можно воспользоваться названием `::`
      *
      */

     def ::[B >: T](el: B): List[B] = this match {
       case list@List.::(_, _) => List.::(el, list)
       case List.Nil => List(el)
     }

     /**
      * Метод mkString возвращает строковое представление списка, с учетом переданного разделителя
      *
      */

     def mkString(delim: String): String = {
       val sb = new StringBuilder()

       @tailrec
       def mk(list: List[T]): Unit = {
         list match {
           case List.Nil =>
           case List.::(h, List.Nil) => sb.append(h)
           case List.::(head, tail) =>
             sb.append(head)
             sb.append(delim)
             mk(tail)
         }
       }

       mk(this)
       sb.toString()
     }


     /**
      *
      * Реализовать метод reverse который позволит заменить порядок элементов в списке на противоположный
      */
     def reverse: List[T] = {
       @tailrec
       def rev(list: List[T], acc: List[T]): List[T] = list match {
         case List.Nil => acc
         case List.::(head, tail) => rev(tail, acc.::(head))
       }

       this match {
         case List.::(head, tail) => rev(tail, List(head))
         case List.Nil => List.Nil
       }

     }

     /**
      *
      * Реализовать метод map для списка который будет применять некую ф-цию к элементам данного списка
      */

     def map[A](f: T => A): List[A] = {
       @tailrec
       def apply(t: List[T], a: List[A]): List[A] = {
         t match {
           case List.::(head, tail) => apply(tail, a.::(f(head)))
           case List.Nil => a
         }
       }

       this match {
         case List.::(head, tail) => apply(tail,List(f(head)))
         case List.Nil => List.Nil
       }
     }


     /**
      *
      * Реализовать метод filter для списка который будет фильтровать список по некому условию
      */

     def filter(f: T => Boolean): List[T] = {
       @tailrec
       def apply(a: List[T], b: List[T]): List[T] = a match {
         case List.::(head, tail) => apply(tail, if (f(head)) b.::(head) else b)
         case List.Nil => b
       }

       this match {
         case List.::(head, tail) => apply(tail, if (f(head)) List(head) else List.Nil)
         case List.Nil => List.Nil
       }

     }


   }




   /**
    * Конструктор, позволяющий создать список из N - го числа аргументов
    * Для этого можно воспользоваться *
    *
    * Например вот этот метод принимает некую последовательность аргументов с типом Int и выводит их на печать
    * def printArgs(args: Int*) = args.foreach(println(_))
    */


   object List {
       case class ::[A](head: A, tail: List[A]) extends List[A]

       case object Nil extends List[Nothing]

       def apply[A](v: A*): List[A] =
         if (v.isEmpty) List.Nil
         else ::(v.head, apply(v.tail: _*))


     /**
      *
      * Написать функцию incList котрая будет принимать список Int и возвращать список,
      * где каждый элемент будет увеличен на 1
      */

     def incList(list: List[Int]): List[Int] = list.map(_ + 1)




     /**
      *
      * Написать функцию shoutString котрая будет принимать список String и возвращать список,
      * где к каждому элементу будет добавлен префикс в виде '!'
      */


     def shoutString(list: List[String]): List[String] = list.map(s => s"!$s")

   }

     private val value: List[Int] = List(1, 2, 3, 4)
//     println(value.::(6))
     println(value.reverse)
     println("value.reverse")








 }