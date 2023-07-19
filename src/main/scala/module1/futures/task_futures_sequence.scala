package module1.futures

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

object task_futures_sequence {

  /**
   * В данном задании Вам предлагается реализовать функцию fullSequence,
   * похожую на Future.sequence, но в отличии от нее,
   * возвращающую все успешные и не успешные результаты.
   * Возвращаемое тип функции - кортеж из двух списков,
   * в левом хранятся результаты успешных выполнений,
   * в правово результаты неуспешных выполнений.
   * Не допускается использование методов объекта Await и мутабельных переменных var
   */
  /**
   * @param futures список асинхронных задач
   * @return асинхронную задачу с кортежом из двух списков
   */


  def fullSequence[A](futures: List[Future[A]])(implicit exc: ExecutionContext): Future[(List[A], List[Throwable])] = {
    futures.foldLeft(Future((List[A](), List[Throwable]())))((t, f) =>
      f.transformWith {
        case Success(v) => t.map(tt => (tt._1.appended(v), tt._2))
        case Failure(ex) => t.map(tt => (tt._1, tt._2.appended(ex)))
      }
    )
  }

}
