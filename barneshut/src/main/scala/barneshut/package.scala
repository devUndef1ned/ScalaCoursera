import java.util.concurrent._
import scala.{collection => coll}
import scala.util.DynamicVariable
import barneshut.conctrees._

package object barneshut {

  class Boundaries {
    var minX: Float = Float.MaxValue

    var minY: Float = Float.MaxValue

    var maxX: Float = Float.MinValue

    var maxY: Float = Float.MinValue

    def width = maxX - minX

    def height = maxY - minY

    def size = math.max(width, height)

    def centerX = minX + width / 2

    def centerY = minY + height / 2

    override def toString = s"Boundaries($minX, $minY, $maxX, $maxY)"
  }

  sealed abstract class Quad extends QuadInterface {
    def massX: Float

    def massY: Float

    def mass: Float

    def centerX: Float

    def centerY: Float

    def size: Float

    def total: Int

    def insert(b: Body): Quad
  }

  case class Empty(centerX: Float, centerY: Float, size: Float) extends Quad {
    def massX: Float = centerX

    def massY: Float = centerY

    def mass: Float = 0f

    def total: Int = 0

    def insert(b: Body): Quad = Leaf(centerX, centerY, size, List(b))
  }

  case class Fork(nw: Quad, ne: Quad, sw: Quad, se: Quad) extends Quad {
    val centerX: Float = (nw.centerX + ne.centerX) / 2
    val centerY: Float = (nw.centerY + sw.centerY) / 2
    val size: Float = nw.size + ne.size
    val mass: Float = nw.mass + ne.mass + sw.mass + se.mass
    val massX: Float = if (mass == 0) centerX else
      (nw.massX * nw.mass + ne.massX * ne.mass + sw.massX * sw.mass + se.massX * se.mass) / mass
    val massY: Float = if (mass == 0) centerY else
      (nw.massY * nw.mass + ne.massY * ne.mass + sw.massY * sw.mass + se.massY * se.mass) / mass
    val total: Int = nw.total + ne.total + sw.total + se.total

    def insert(b: Body): Fork = {
      val inTopPart = b.y <= centerY
      val inLeftPart = b.y <= centerX
      (inLeftPart, inTopPart) match {
        case (true, true) => Fork(nw.insert(b), ne, sw, se)
        case (false, true) => Fork(nw, ne.insert(b), sw, se)
        case (true, false) => Fork(nw, ne, sw.insert(b), se)
        case (false, false) => Fork(nw, ne, sw, se.insert(b))
      }
    }
  }

  case class Leaf(centerX: Float, centerY: Float, size: Float, bodies: coll.Seq[Body])
    extends Quad {
    override val (mass, massX, massY) = computeMass()
    val total: Int = bodies.size

    def computeMass(): (Float, Float, Float) = {
      val (mass, sumMassX, sumMassY) = (bodies.foldLeft((0.0f, 0.0f, 0.0f))((acc, p) =>
        (acc._1 + p.mass, acc._2 + p.mass * p.x, acc._3 + p.mass * p.y)))
      (mass, sumMassX / mass, sumMassY / mass)
    }

    def insert(b: Body): Quad = {
      val newBodies = bodies :+ b
      if (size <= minimumSize) {
        Leaf(centerX, centerY, size, newBodies)
      } else {
        val newSize = size / 2
        val delta = newSize / 2
        val newQuad = Fork(
          Empty(centerX - delta, centerY - delta, newSize),
          Empty(centerX + delta, centerY - delta, newSize),
          Empty(centerX - delta, centerY + delta, newSize),
          Empty(centerX + delta, centerY + delta, newSize)
        )
        newBodies.foldLeft(newQuad)((quad, body) => quad.insert(body))
      }
    }
  }

  def minimumSize = 0.00001f

  def gee: Float = 100.0f

  def delta: Float = 0.01f

  def theta = 0.5f

  def eliminationThreshold = 0.5f

  def force(m1: Float, m2: Float, dist: Float): Float = gee * m1 * m2 / (dist * dist)

  def distance(x0: Float, y0: Float, x1: Float, y1: Float): Float = {
    math.sqrt((x1 - x0) * (x1 - x0) + (y1 - y0) * (y1 - y0)).toFloat
  }

  class Body(val mass: Float, val x: Float, val y: Float, val xspeed: Float, val yspeed: Float) {

    def updated(quad: Quad): Body = {
      var netforcex = 0.0f
      var netforcey = 0.0f

      def addForce(thatMass: Float, thatMassX: Float, thatMassY: Float): Unit = {
        val dist = distance(thatMassX, thatMassY, x, y)
        /* If the distance is smaller than 1f, we enter the realm of close
         * body interactions. Since we do not model them in this simplistic
         * implementation, bodies at extreme proximities get a huge acceleration,
         * and are catapulted from each other's gravitational pull at extreme
         * velocities (something like this:
         * http://en.wikipedia.org/wiki/Interplanetary_spaceflight#Gravitational_slingshot).
         * To decrease the effect of this gravitational slingshot, as a very
         * simple approximation, we ignore gravity at extreme proximities.
         */
        if (dist > 1f) {
          val dforce = force(mass, thatMass, dist)
          val xn = (thatMassX - x) / dist
          val yn = (thatMassY - y) / dist
          val dforcex = dforce * xn
          val dforcey = dforce * yn
          netforcex += dforcex
          netforcey += dforcey
        }
      }

      def traverse(quad: Quad): Unit = (quad: Quad) match {
        case Empty(_, _, _) =>
        // no force
        case Leaf(_, _, _, bodies) => bodies.foreach(b => addForce(b.mass, b.x, b.y))
        case Fork(nw, ne, sw, se) =>
          // see if node is far enough from the body,
          // or recursion is needed
          val dist = distance(quad.massX, quad.massY, x, y)
          if (quad.size / dist < theta) {
            addForce(quad.mass, quad.massX, quad.massY)
          } else {
            traverse(nw)
            traverse(ne)
            traverse(sw)
            traverse(se)
          }
      }

      traverse(quad)

      val nx = x + xspeed * delta
      val ny = y + yspeed * delta
      val nxspeed = xspeed + netforcex / mass * delta
      val nyspeed = yspeed + netforcey / mass * delta

      new Body(mass, nx, ny, nxspeed, nyspeed)
    }

  }

  val SECTOR_PRECISION = 8

  class SectorMatrix(val boundaries: Boundaries, val sectorPrecision: Int) extends SectorMatrixInterface {
    val sectorSize: Float = boundaries.size / sectorPrecision
    val matrix = new Array[ConcBuffer[Body]](sectorPrecision * sectorPrecision)
    for (i <- matrix.indices) matrix(i) = new ConcBuffer

    def +=(b: Body): SectorMatrix = {
      apply(getXIndex(b), getYIndex(b)) += b
      this
    }

    def apply(x: Int, y: Int) = matrix(y * sectorPrecision + x)

    def combine(that: SectorMatrix): SectorMatrix = {
      def combine(res: SectorMatrix, matrix: Array[ConcBuffer[Body]]) = {
        matrix.foldLeft(res)((resAcc, buffer) => buffer.foldLeft(resAcc)((resAccInner, b) => resAccInner += b))
      }

      val result = new SectorMatrix(boundaries, sectorPrecision)
      combine(combine(result, matrix), that.matrix)
    }

    def getXIndex(b: Body): Int = {
      if (fitX(b)) {
        math.min(((b.x - boundaries.minX) / sectorSize).toInt, sectorPrecision - 1)
      } else if (b.x < boundaries.minX) {
        0
      } else {
        sectorPrecision - 1
      }
    }

    def getYIndex(b: Body): Int = {
      if (fitY(b)) {
        math.min(((b.y - boundaries.minY) / sectorSize).toInt, sectorPrecision - 1)
      } else if (b.y < boundaries.minY) {
        0
      } else {
        sectorPrecision - 1
      }
    }

    def fitX(b: Body): Boolean = (b.x >= boundaries.minX && b.x <= boundaries.maxX)

    def fitY(b: Body): Boolean = (b.y >= boundaries.minY && b.y <= boundaries.maxY)

    def toQuad(parallelism: Int): Quad = {
      def BALANCING_FACTOR = 4

      def quad(x: Int, y: Int, span: Int, achievedParallelism: Int): Quad = {
        if (span == 1) {
          val sectorSize = boundaries.size / sectorPrecision
          val centerX = boundaries.minX + x * sectorSize + sectorSize / 2
          val centerY = boundaries.minY + y * sectorSize + sectorSize / 2
          var emptyQuad: Quad = Empty(centerX, centerY, sectorSize)
          val sectorBodies = this (x, y)
          sectorBodies.foldLeft(emptyQuad)(_ insert _)
        } else {
          val nspan = span / 2
          val nAchievedParallelism = achievedParallelism * 4
          val (nw, ne, sw, se) =
            if (parallelism > 1 && achievedParallelism < parallelism * BALANCING_FACTOR) parallel(
              quad(x, y, nspan, nAchievedParallelism),
              quad(x + nspan, y, nspan, nAchievedParallelism),
              quad(x, y + nspan, nspan, nAchievedParallelism),
              quad(x + nspan, y + nspan, nspan, nAchievedParallelism)
            ) else (
              quad(x, y, nspan, nAchievedParallelism),
              quad(x + nspan, y, nspan, nAchievedParallelism),
              quad(x, y + nspan, nspan, nAchievedParallelism),
              quad(x + nspan, y + nspan, nspan, nAchievedParallelism)
            )
          Fork(nw, ne, sw, se)
        }
      }

      quad(0, 0, sectorPrecision, 1)
    }

    override def toString = s"SectorMatrix(#bodies: ${matrix.map(_.size).sum})"
  }

  class TimeStatistics {
    private val timeMap = collection.mutable.Map[String, (Double, Int)]()

    def clear() = timeMap.clear()

    def timed[T](title: String)(body: => T): T = {
      var res: T = null.asInstanceOf[T]
      val totalTime = /*measure*/ {
        val startTime = System.currentTimeMillis()
        res = body
        (System.currentTimeMillis() - startTime)
      }

      timeMap.get(title) match {
        case Some((total, num)) => timeMap(title) = (total + totalTime, num + 1)
        case None => timeMap(title) = (0.0, 0)
      }

      println(s"$title: ${totalTime} ms; avg: ${timeMap(title)._1 / timeMap(title)._2}")
      res
    }

    override def toString = {
      timeMap map {
        case (k, (total, num)) => k + ": " + (total / num * 100).toInt / 100.0 + " ms"
      } mkString ("\n")
    }
  }

  val forkJoinPool = new ForkJoinPool

  abstract class TaskScheduler {
    def schedule[T](body: => T): ForkJoinTask[T]

    def parallel[A, B](taskA: => A, taskB: => B): (A, B) = {
      val right = task {
        taskB
      }
      val left = taskA
      (left, right.join())
    }
  }

  class DefaultTaskScheduler extends TaskScheduler {
    def schedule[T](body: => T): ForkJoinTask[T] = {
      val t = new RecursiveTask[T] {
        def compute = body
      }
      Thread.currentThread match {
        case wt: ForkJoinWorkerThread =>
          t.fork()
        case _ =>
          forkJoinPool.execute(t)
      }
      t
    }
  }

  val scheduler =
    new DynamicVariable[TaskScheduler](new DefaultTaskScheduler)

  def task[T](body: => T): ForkJoinTask[T] = {
    scheduler.value.schedule(body)
  }

  def parallel[A, B](taskA: => A, taskB: => B): (A, B) = {
    scheduler.value.parallel(taskA, taskB)
  }

  def parallel[A, B, C, D](taskA: => A, taskB: => B, taskC: => C, taskD: => D): (A, B, C, D) = {
    val ta = task {
      taskA
    }
    val tb = task {
      taskB
    }
    val tc = task {
      taskC
    }
    val td = taskD
    (ta.join(), tb.join(), tc.join(), td)
  }
}
