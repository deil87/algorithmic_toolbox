package week4

import java.io.{BufferedReader, IOException, InputStream, InputStreamReader}
import java.util.StringTokenizer

import scala.util.control.Breaks._
/**
  * The goal in this problem is to redesign a given implementation of the randomized
  * quick sort algorithm so that it works fast even on sequences containing
  * many equal elements.
  * /* This function partitions a[] in three parts
   a) a[l..i] contains all elements smaller than pivot
   b) a[i+1..j-1] contains all occurrences of pivot
   c) a[j..r] contains all elements greater than pivot */
void partition(int a[], int l, int r, int &i, int &j)
{
    i = l-1, j = r;
    int p = l-1, q = r;
    int v = a[r];

    while (true)
    {
        // From left, find the first element greater than
        // or equal to v. This loop will definitely terminate
        // as v is last element
        while (a[++i] < v);

        // From right, find the first element smaller than or
        // equal to v
        while (v < a[--j])
            if (j == l)
                break;

        // If i and j cross, then we are done
        if (i >= j) break;

        // Swap, so that smaller goes on left greater goes on right
        swap(a[i], a[j]);

        // Move all same left occurrence of pivot to beginning of
        // array and keep count using p
        if (a[i] == v)
        {
            p++;
            swap(a[p], a[i]);
        }

        // Move all same right occurrence of pivot to end of array
        // and keep count using q
        if (a[j] == v)
        {
            q--;
            swap(a[j], a[q]);
        }
    }

    // Move pivot element to its correct index
    swap(a[i], a[r]);

    // Move all left same occurrences from beginning
    // to adjacent to arr[i]
    j = i-1;
    for (int k = l; k < p; k++, j--)
        swap(a[k], a[j]);

    // Move all right same occurrences from end
    // to adjacent to arr[i]
    i = i+1;
    for (int k = r-1; k > q; k--, i++)
        swap(a[i], a[k]);
}

// 3-way partition based quick sort
void quicksort(int a[], int l, int r)
{
    if (r <= l) return;

    int i, j;

    // Note that i and j are passed as reference
    partition(a, l, r, i, j);

    // Recur
    quicksort(a, l, j);
    quicksort(a, i, r);
}
  */
object QuickSort3Fastest { // Bentley-McIlroy 3-way algorithm

  sealed trait DebugConfig {
    def enabled: Boolean = false
    def benchmark: Boolean = false
  }
  case class EnabledDebug() extends DebugConfig {
    override def enabled: Boolean = true
  }
  case class DisabledDebug() extends DebugConfig {
    override def enabled: Boolean = false
  }

  class Logger(implicit val config: DebugConfig) {
    def log(str: => String): Unit = {
      if(config.enabled) println(str) else {}
    }
  }

  implicit val debug = DisabledDebug()//DisabledDebug()
  val logger = new Logger()

  private def partition3(a: Array[Int], l: Int, r: Int): (Int, Int) = {
//    logger.log(s"low: $l, high: $r")
//    for(u <- l to r) print(s"-${a(u)}-")
//    logger.log(s"Pivot = $x")

    //Помним о том что наши индексы не начинаются с нуля в последующих итерациях! i = 0 p = 0 не сработает,
    // так как далее мы рассматривает сабэрэй, но при этом мы не урезаем массив 'a'
    var i = l -1
    var j = r
    var p = l -1
    var q = r
    val pivot = a(r) // Important! We don't choose random element

    breakable {
      while (true) {

        i += 1
        while (a(i) < pivot) i += 1
        j -= 1
        while (a(j) > pivot && j != l) {
          j -= 1
        }

        if (i >= j) break

        swap(a, i, j)
        if (a(i) == pivot) {
          p += 1
          swap(a, p, i)
        }
        if (a(j) == pivot) {
          q -= 1
          swap(a, q, j)
        }
      }
    }
//    println(s"Swapping $i <-> $j")

    swap(a, i, r)
    j = i -1
    for{ k <- l until p} {
      swap(a, k ,j)
      j -= 1
    }
    i += 1
    for{ k <- r -1 until q by -1} {
      swap(a, k ,i)
      i += 1
    }
    (j, i )
  }

  def swap(a: Array[Int], l: Int, j: Int): Unit = {
    if(l != j) {
      val t: Int = a(l)
      a(l) = a(j)
      a(j) = t
    }
//    logger.log(a.mkString(","))
  }

  private def randomizedQuickSort(a: Array[Int], l: Int, r: Int)
  {
    if (l >= r) { return}

     val (middle1, middle2) =  partition3(a, l, r)
    randomizedQuickSort(a, l, middle1)
    randomizedQuickSort(a, middle2, r)
  }


  def main(args: Array[String]): Unit = {

    val s = new FastScanner(System.in)

    val amountOfNumbers = s.nextInt
    val numbers: Array[Int] = new Array[Int](amountOfNumbers)
    var i: Int = 0

    while (i < amountOfNumbers) {
      numbers(i) = s.nextInt
      i += 1
    }


    randomizedQuickSort(numbers, 0, amountOfNumbers - 1)

    // Print result
    var a: Int = 0
    while (a < amountOfNumbers) {
      System.out.print(numbers(a) + " ")
      a += 1
    }
  }

  class FastScanner(stream: InputStream) {
    private var br: BufferedReader = new BufferedReader(new InputStreamReader(stream))
    private var st: StringTokenizer = null

    def next: String = {
      while (st == null || !st.hasMoreTokens) {
        {
          try {
            st = new StringTokenizer(br.readLine)
          }
          catch {
            case e: IOException => {
              e.printStackTrace
            }
          }
        }
      }
      return st.nextToken
    }

    def nextInt: Int = {
      return next.toInt
    }
  }
}
