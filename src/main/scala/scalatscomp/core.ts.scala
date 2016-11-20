package scalatscomp

import scala.collection.mutable
import Types._

object Core {
  sealed abstract class Ternary
  object Ternary {
    case object False extends Ternary
    case object Maybe extends Ternary
    case object True extends Ternary
  }

  // val createObject = Object.create

  // More efficient to create a collator once and use its `compare` than to call `a.localeCompare(b)` many times.
  //val collator: { def compare(a: String, b: String): Int } =
  //  (if (((typeof(Intl) === "object") && (typeof(Intl.Collator) === "function")))
  //     new Intl.Collator()
  //   else undefined)

  def createMap[T](template: mutable.Map[String, T] = mutable.Map.empty): mutable.Map[String, T] =
    mutable.Map.empty[String, T] ++= template

  def createFileMap[T](keyMapper: Option[(String) => String] = None): FileMap[T] =
    new FileMap(keyMapper)

  def toPath(fileName: String,
             basePath: String,
             getCanonicalFileName: ((String) => String)): Path = {
    val nonCanonicalizedPath =
      (if (isRootedDiskPath(fileName)) normalizePath(fileName)
       else getNormalizedAbsolutePath(fileName, basePath))
    return getCanonicalFileName(nonCanonicalizedPath).asInstanceOf[Path]
  }

  sealed abstract class Comparison
  object Comparison {
    case object LessThan extends Comparison
    case object EqualTo extends Comparison
    case object GreaterThan extends Comparison
  }

  def forEach[T, U](array: Array[T],
                    callback: (T, Int) => Option[U]): U = {
    if (array != null) {
      var i = 0
      val len = array.length
      while (i < len) {
        val result = callback(array(i), i)
        if (result.isNonEmpty) {
          return result.get
        }
        i += 1
      }
    }
    return null.asInstanceOf[U]
  }

  def every[T](array: Array[T], callback: (T, Int) => Boolean): Boolean = {
    if (array != null) {
      var i = 0
      val len = array.length
      while (i < len) {
        if (!callback(array(i), i)) {
          return false
        }
        i += 1
      }
    }
    return true
  }

  def find[T](array: Array[T],
              predicate: (T, Int) => Boolean): T = {
    var i = 0
    val len = array.length
    while (i < len) {
      val value = array(i)
      if (predicate(value, i)) {
        return value
      }
      i += 1
    }
    return null.asInstanceOf[T]
  }

  def findMap[T, U](array: Array[T],
                    callback: (T, Int) => Option[U]): U = {
    var i = 0
    var len = array.length
    while (i < len) {
      val result = callback(array(i), i)
      if (result.nonEmpty) {
        return result.get
      }
      i += 1
    }
    throw new AssertionError("debug.fail")
  }

  def contains[T](array: Array[T], value: T): Boolean = {
    if (array) {
      (array).foreach { fresh4 =>
        val v = zeroOfMyType = fresh4 {
          if ((v === value)) {
            return true

          }

        }
      }

    }
    return false

  }
  def indexOf[T](array: Array[T], value: T): Int = {
    if (array) {
      {
        var i = 0
        var len = array.length
        while ((i < len)) {
          {
            if ((array(i) === value)) {
              return i

            }

          }
          (i += 1)
        }
      }

    }
    return (-1)

  }
  def indexOfAnyCharCode(text: String,
                         charCodes: Array[Int],
                         start: Int): Int = {
    {
      var i = (start || 0)
      var len = text.length
      while ((i < len)) {
        {
          if (contains(charCodes, text.charCodeAt(i))) {
            return i

          }

        }
        (i += 1)
      }
    }
    return (-1)

  }
  def countWhere[T](array: Array[T], predicate: ((T, Int) => Boolean)): Int = {
    var count = 0
    if (array) {
      {
        var i = 0
        while ((i < array.length)) {
          {
            val v = array(i)
            if (predicate(v, i)) {
              (count += 1)

            }

          }
          (i += 1)
        }
      }

    }
    return count

  }
  def filter[T, U <: T](array: Array[T], f: ((T) => Boolean)): Array[U]
  def filter[T](array: Array[T], f: ((T) => Boolean)): Array[T]
  def filter[T](array: Array[T], f: ((T) => Boolean)): Array[T] = {
    if (array) {
      val len = array.length
      var i = 0
      while (((i < len) && f(array(i)))) {
        (i += 1)
      }
      if ((i < len)) {
        val result = array.slice(0, i)
        (i += 1)
        while ((i < len)) {
          {
            val item = array(i)
            if (f(item)) {
              result.push(item)

            }
            (i += 1)

          }
        }
        return result

      }

    }
    return array

  }
  def removeWhere[T](array: Array[T], f: ((T) => Boolean)): Boolean = {
    var outIndex = 0
    (array).foreach { fresh5 =>
      val item = zeroOfMyType = fresh5 {
        if ((!f(item))) {
          (array(outIndex) = item)
          (outIndex += 1)

        }

      }
    }
    if ((outIndex !== array.length)) {
      (array.length = outIndex)
      return true

    }
    return false

  }
  def filterMutate[T](array: Array[T], f: ((T) => Boolean)): Unit = {
    var outIndex = 0
    (array).foreach { fresh6 =>
      val item = zeroOfMyType = fresh6 {
        if (f(item)) {
          (array(outIndex) = item)
          (outIndex += 1)

        }

      }
    }
    (array.length = outIndex)

  }
  def map[T, U](array: Array[T], f: ((T, Int) => U)): Array[U] = {
    var result: Array[U] = zeroOfMyType
    if (array) {
      (result = Array()) {
        var i = 0
        while ((i < array.length)) {
          {
            result.push(f(array(i), i))

          }
          (i += 1)
        }
      }

    }
    return result

  }
  def sameMap[T](array: Array[T], f: ((T, Int) => T)): Array[T] = {
    var result: Array[T] = zeroOfMyType
    if (array) {
      {
        var i = 0
        while ((i < array.length)) {
          {
            if (result) {
              result.push(f(array(i), i))

            } else {
              val item = array(i)
              val mapped = f(item, i)
              if ((item !== mapped)) {
                (result = array.slice(0, i))
                result.push(mapped)

              }

            }

          }
          (i += 1)
        }
      }

    }
    return (result || array)

  }
  def flatten[T](array: Array[(T | Array[T])]): Array[T] = {
    var result: Array[T] = zeroOfMyType
    if (array) {
      (result = Array())
      (array).foreach { fresh7 =>
        val v = zeroOfMyType = fresh7 {
          if (v) {
            if (isArray(v)) {
              addRange(result, v)

            } else {
              result.push(v)

            }

          }

        }
      }

    }
    return result

  }
  def flatMap[T, U](array: Array[T],
                    mapfn: ((T, Int) => (U | Array[U]))): Array[U] = {
    var result: Array[U] = zeroOfMyType
    if (array) {
      (result = Array()) {
        var i = 0
        while ((i < array.length)) {
          {
            val v = mapfn(array(i), i)
            if (v) {
              if (isArray(v)) {
                addRange(result, v)

              } else {
                result.push(v)

              }

            }

          }
          (i += 1)
        }
      }

    }
    return result

  }
  def span[T](array: Array[T],
              f: ((T, Int) => Boolean)): (Array[T], Array[T]) = {
    if (array) {
      {
        var i = 0
        while ((i < array.length)) {
          {
            if ((!f(array(i), i))) {
              return Array(array.slice(0, i), array.slice(i))

            }

          }
          (i += 1)
        }
      }
      return Array(array.slice(0), Array())

    }
    return undefined

  }
  def spanMap[T, K, U](array: Array[T],
                       keyfn: ((T, Int) => K),
                       mapfn: ((Array[T], K, Int, Int) => U)): Array[U] = {
    var result: Array[U] = zeroOfMyType
    if (array) {
      (result = Array())
      val len = array.length
      var previousKey: K = zeroOfMyType
      var key: K = zeroOfMyType
      var start = 0
      var pos = 0
      while ((start < len)) {
        {
          while ((pos < len)) {
            {
              val value = array(pos)
              (key = keyfn(value, pos))
              if ((pos === 0)) {
                (previousKey = key)

              } else if ((key !== previousKey)) {
                break()

              }
              (pos += 1)

            }
          }
          if ((start < pos)) {
            val v = mapfn(array.slice(start, pos), previousKey, start, pos)
            if (v) {
              result.push(v)

            }
            (start = pos)

          }
          (previousKey = key)
          (pos += 1)

        }
      }

    }
    return result

  }
  def mapObject[T, U](`object`: MapLike[T],
                      f: ((String, T) => (String, U))): MapLike[U] = {
    var result: MapLike[U] = zeroOfMyType
    if (`object`) {
      (result = Map(
        ))
      (getOwnKeys(`object`)).foreach { fresh8 =>
        val v = zeroOfMyType = fresh8 {
          val keyvalue: (String, U) = (f(v, `object`(v)) || Array(
              undefined,
              undefined))
          val key = keyvalue(0)
          val value = keyvalue(1)
          if ((key !== undefined)) {
            (result(key) = value)

          }

        }
      }

    }
    return result

  }
  def some[T](array: Array[T], predicate: ((T) => Boolean)): Boolean = {
    if (array) {
      (array).foreach { fresh9 =>
        val v = zeroOfMyType = fresh9 {
          if (((!predicate) || predicate(v))) {
            return true

          }

        }
      }

    }
    return false

  }
  def concatenate[T](array1: Array[T], array2: Array[T]): Array[T] = {
    if (((!array2) || (!array2.length)))
      return array1
    if (((!array1) || (!array1.length)))
      return array2
    return Array(array1: _*, array2: _*)

  }
  def deduplicate[T](array: Array[T],
                     areEqual: ((T, T) => Boolean)): Array[T] = {
    var result: Array[T] = zeroOfMyType
    if (array) {
      (result = Array())
      val loop = new scala.util.control.Breaks
      loop.breakable {
        (array).foreach { fresh10 =>
          val item = zeroOfMyType = fresh10 {
            (result).foreach { fresh11 =>
              val res = zeroOfMyType = fresh11 {
                if ((if (areEqual) areEqual(res, item) else (res === item))) {
                  continue loop

                }

              }
            }
            result.push(item)

          }
        }
      }
    }
    return result

  }
  def arrayIsEqualTo[T](array1: ReadonlyArray[T],
                        array2: ReadonlyArray[T],
                        equaler: ((T, T) => Boolean)): Boolean = {
    if (((!array1) || (!array2))) {
      return (array1 === array2)

    }
    if ((array1.length !== array2.length)) {
      return false

    }
    {
      var i = 0
      while ((i < array1.length)) {
        {
          val equals =
            (if (equaler) equaler(array1(i), array2(i))
             else (array1(i) === array2(i)))
          if ((!equals)) {
            return false

          }

        }
        (i += 1)
      }
    }
    return true

  }
  def changesAffectModuleResolution(oldOptions: CompilerOptions,
                                    newOptions: CompilerOptions): Boolean = {
    return ((((((((((((((((!oldOptions) || ((oldOptions.module !== newOptions.module))) || ((oldOptions.moduleResolution !== newOptions.moduleResolution))) || ((oldOptions.noResolve !== newOptions.noResolve))) || ((oldOptions.target !== newOptions.target))) || ((oldOptions.noLib !== newOptions.noLib))) || ((oldOptions.jsx !== newOptions.jsx))) || ((oldOptions.allowJs !== newOptions.allowJs))) || ((oldOptions.rootDir !== newOptions.rootDir))) || ((oldOptions.configFilePath !== newOptions.configFilePath))) || ((oldOptions.baseUrl !== newOptions.baseUrl))) || ((oldOptions.maxNodeModuleJsDepth !== newOptions.maxNodeModuleJsDepth))) || (!arrayIsEqualTo(
      oldOptions.lib,
      newOptions.lib))) || (!arrayIsEqualTo(
      oldOptions.typeRoots,
      newOptions.typeRoots))) || (!arrayIsEqualTo(
      oldOptions.rootDirs,
      newOptions.rootDirs))) || (!equalOwnProperties(
      oldOptions.paths,
      newOptions.paths)))

  }
  def compact[T](array: Array[T]): Array[T] = {
    var result: Array[T] = zeroOfMyType
    if (array) {
      {
        var i = 0
        while ((i < array.length)) {
          {
            val v = array(i)
            if ((result || (!v))) {
              if ((!result)) {
                (result = array.slice(0, i))

              }
              if (v) {
                result.push(v)

              }

            }

          }
          (i += 1)
        }
      }

    }
    return (result || array)

  }
  def sum(array: Array[Any], prop: String): Int = {
    var result = 0
    (array).foreach { fresh12 =>
      val v = zeroOfMyType = fresh12 {
        (result += v(prop))

      }
    }
    return result

  }
  def addRange[T](to: Array[T], from: Array[T]): Unit = {
    if ((to && from)) {
      (from).foreach { fresh13 =>
        val v = zeroOfMyType = fresh13 {
          if ((v !== undefined)) {
            to.push(v)

          }

        }
      }

    }

  }
  def rangeEquals[T](array1: Array[T], array2: Array[T], pos: Int, end: Int) = {
    while ((pos < end)) {
      {
        if ((array1(pos) !== array2(pos))) {
          return false

        }
        (pos += 1)

      }
    }
    return true

  }
  def firstOrUndefined[T](array: Array[T]): T = {
    return (if ((array && (array.length > 0))) array(0) else undefined)

  }
  def singleOrUndefined[T](array: Array[T]): T = {
    return (if ((array && (array.length === 1))) array(0) else undefined)

  }
  def singleOrMany[T](array: Array[T]): (T | Array[T]) = {
    return (if ((array && (array.length === 1))) array(0) else array)

  }
  def lastOrUndefined[T](array: Array[T]): T = {
    return (if ((array && (array.length > 0))) array((array.length - 1))
            else undefined)

  }
  def replaceElement[T](array: Array[T], index: Int, value: T): Array[T] = {
    val result = array.slice(0)
    (result(index) = value)
    return result

  }
  def binarySearch[T](array: Array[T],
                      value: T,
                      comparer: ((T, T) => Int)): Int = {
    if (((!array) || (array.length === 0))) {
      return (-1)

    }
    var low = 0
    var high = (array.length - 1)
    (comparer =
      (if ((comparer !== undefined)) comparer
       else
         ((v1, v2) =>
            ((if ((v1 < v2)) (-1) else ((if ((v1 > v2)) 1 else 0) )) ))))
    while ((low <= high)) {
      {
        val middle = (low + ((((high - low)) >> 1)))
        val midValue = array(middle)
        if ((comparer(midValue, value) === 0)) {
          return middle

        } else if ((comparer(midValue, value) > 0)) {
          (high = (middle - 1))

        } else {
          (low = (middle + 1))

        }

      }
    }
    return (~low)

  }
  def reduceLeft[T, U](array: Array[T],
                       f: ((U, T, Int) => U),
                       initial: U,
                       start: Int,
                       count: Int): U
  def reduceLeft[T](array: Array[T], f: ((T, T, Int) => T)): T
  def reduceLeft[T](array: Array[T],
                    f: ((T, T, Int) => T),
                    initial: T,
                    start: Int,
                    count: Int): T = {
    if ((array && (array.length > 0))) {
      val size = array.length
      if ((size > 0)) {
        var pos = (if (((start === undefined) || (start < 0))) 0 else start)
        val end =
          (if (((count === undefined) || ((pos + count) > (size - 1))))
             (size - 1)
           else (pos + count))
        var result: T = zeroOfMyType
        if ((arguments.length <= 2)) {
          (result = array(pos))
          (pos += 1)

        } else {
          (result = initial)

        }
        while ((pos <= end)) {
          {
            (result = f(result, array(pos), pos))
            (pos += 1)

          }
        }
        return result

      }

    }
    return initial

  }
  def reduceRight[T, U](array: Array[T],
                        f: ((U, T, Int) => U),
                        initial: U,
                        start: Int,
                        count: Int): U
  def reduceRight[T](array: Array[T], f: ((T, T, Int) => T)): T
  def reduceRight[T](array: Array[T],
                     f: ((T, T, Int) => T),
                     initial: T,
                     start: Int,
                     count: Int): T = {
    if (array) {
      val size = array.length
      if ((size > 0)) {
        var pos =
          (if (((start === undefined) || (start > (size - 1)))) (size - 1)
           else start)
        val end =
          (if (((count === undefined) || ((pos - count) < 0))) 0
           else (pos - count))
        var result: T = zeroOfMyType
        if ((arguments.length <= 2)) {
          (result = array(pos))
          (pos -= 1)

        } else {
          (result = initial)

        }
        while ((pos >= end)) {
          {
            (result = f(result, array(pos), pos))
            (pos -= 1)

          }
        }
        return result

      }

    }
    return initial

  }
  val `hasOwnProperty` = Object.prototype.`hasOwnProperty`
  def hasProperty[T](map: MapLike[T], key: String): Boolean = {
    return `hasOwnProperty`.call(map, key)

  }
  def getProperty[T](map: MapLike[T], key: String): (T | undefined) = {
    return (if (`hasOwnProperty`.call(map, key)) map(key) else undefined)

  }
  def getOwnKeys[T](map: MapLike[T]): Array[String] = {
    val keys: Array[String] = Array()
    (map).keys.foreach { fresh14 =>
      val key = zeroOfMyType = fresh14
      if (`hasOwnProperty`.call(map, key)) {
        keys.push(key)

      }
    }
    return keys

  }
  def forEachProperty[T, U](map: Map[T], callback: ((T, String) => U)): U = {
    var result: U = zeroOfMyType
    (map).keys.foreach { fresh15 =>
      val key = zeroOfMyType = fresh15 {
        if ((result = callback(map(key), key)))
          break()

      }
    }
    return result

  }
  def someProperties[T](map: Map[T], predicate: ((T, String) => Boolean)) = {
    (map).keys.foreach { fresh16 =>
      val key = zeroOfMyType = fresh16 {
        if (((!predicate) || predicate(map(key), key)))
          return true

      }
    }
    return false

  }
  def copyProperties[T](source: Map[T], target: MapLike[T]): Unit = {
    (source).keys.foreach { fresh17 =>
      val key = zeroOfMyType = fresh17 {
        (target(key) = source(key))

      }
    }

  }
  def assign[T1 <: MapLike[{}], T2, T3](t: T1,
                                        arg1: T2,
                                        arg2: T3): (T1 with T2 with T3)
  def assign[T1 <: MapLike[{}], T2](t: T1, arg1: T2): (T1 with T2)
  def assign[T1 <: MapLike[{}]](t: T1, args: Array[Any]): Any
  def assign[T1 <: MapLike[{}]](t: T1, args: Array[Any]) = {
    (args).foreach { fresh18 =>
      val arg = zeroOfMyType = fresh18 {
        (getOwnKeys(arg)).foreach { fresh19 =>
          val p = zeroOfMyType = fresh19 {
            (t(p) = arg(p))

          }
        }

      }
    }
    return t

  }
  def reduceProperties[T, U](map: Map[T],
                             callback: ((U, T, String) => U),
                             initial: U): U = {
    var result = initial
    (map).keys.foreach { fresh20 =>
      val key = zeroOfMyType = fresh20 {
        (result = callback(result, map(key), String(key)))

      }
    }
    return result

  }
  def reduceOwnProperties[T, U](map: MapLike[T],
                                callback: ((U, T, String) => U),
                                initial: U): U = {
    var result = initial
    (map).keys.foreach { fresh21 =>
      val key = zeroOfMyType = fresh21
      if (`hasOwnProperty`.call(map, key)) {
        (result = callback(result, map(key), String(key)))

      }
    }
    return result

  }
  def equalOwnProperties[T](left: MapLike[T],
                            right: MapLike[T],
                            equalityComparer: ((T, T) => Boolean)) = {
    if ((left === right))
      return true
    if (((!left) || (!right)))
      return false
    (left).keys.foreach { fresh22 =>
      val key = zeroOfMyType = fresh22
      if (`hasOwnProperty`.call(left, key)) {
        if (((!`hasOwnProperty`.call(right, key)) === undefined))
          return false
        if ((if (equalityComparer) (!equalityComparer(left(key), right(key)))
             else (left(key) !== right(key))))
          return false

      }
    }
    (right).keys.foreach { fresh23 =>
      val key = zeroOfMyType = fresh23
      if (`hasOwnProperty`.call(right, key)) {
        if ((!`hasOwnProperty`.call(left, key)))
          return false

      }
    }
    return true

  }
  def arrayToMap[T](array: Array[T], makeKey: ((T) => String)): Map[T]
  def arrayToMap[T, U](array: Array[T],
                       makeKey: ((T) => String),
                       makeValue: ((T) => U)): Map[U]
  def arrayToMap[T, U](array: Array[T],
                       makeKey: ((T) => String),
                       makeValue: ((T) => U)): Map[(T | U)] = {
    val result = createMap[(T | U)]()
    (array).foreach { fresh24 =>
      val value = zeroOfMyType = fresh24 {
        (result(makeKey(value)) = (if (makeValue) makeValue(value) else value))

      }
    }
    return result

  }
  def isEmpty[T](map: Map[T]) = {
    (map).keys.foreach { fresh25 =>
      val id = zeroOfMyType = fresh25 {
        if (hasProperty(map, id)) {
          return false

        }

      }
    }
    return true

  }
  def cloneMap[T](map: Map[T]) = {
    val clone = createMap[T]()
    copyProperties(map, clone)
    return clone

  }
  def clone[T](`object`: T): T = {
    val result: Any = Map(
      )
    (`object`).keys.foreach { fresh26 =>
      val id = zeroOfMyType = fresh26 {
        if (`hasOwnProperty`.call(`object`, id)) {
          (result(id) = (`object`.asInstanceOf[Any])(id))

        }

      }
    }
    return result

  }
  def extend[T1, T2](first: T1, second: T2): (T1 with T2) = {
    val result: (T1 with T2) = Map(
      ).asInstanceOf[Any]
    (second).keys.foreach { fresh27 =>
      val id = zeroOfMyType = fresh27
      if (`hasOwnProperty`.call(second, id)) {
        ((result.asInstanceOf[Any])(id) = (second.asInstanceOf[Any])(id))

      }
    }
    (first).keys.foreach { fresh28 =>
      val id = zeroOfMyType = fresh28
      if (`hasOwnProperty`.call(first, id)) {
        ((result.asInstanceOf[Any])(id) = (first.asInstanceOf[Any])(id))

      }
    }
    return result

  }
  def multiMapAdd[V](map: Map[Array[V]], key: String, value: V): Array[V] = {
    val values = map(key)
    if (values) {
      values.push(value)
      return values

    } else {
      return (map(key) = Array(value))

    }

  }
  def multiMapRemove[V](map: Map[Array[V]], key: String, value: V): Unit = {
    val values = map(key)
    if (values) {
      unorderedRemoveItem(values, value)
      if ((!values.length)) {
        map.remove(key)

      }

    }

  }
  def isArray(value: Any): Boolean = {
    return (if (Array.isArray) Array.isArray(value)
            else (valueinstanceofArray))

  }
  def noop(): Unit = {}
  def notImplemented(): Nothing = {
    throw new Error("Not implemented")
  }
  def memoize[T](callback: (() => T)): (() => T) = {
    var value: T = zeroOfMyType
    return (() => {
              if (callback) {
                (value = callback())
                (callback = undefined)

              }
              return value

            })

  }
  def chain[T, U](args: Array[((T) => ((U) => U))]): ((T) => ((U) => U))
  def chain[T, U](a: ((T) => ((U) => U)),
                  b: ((T) => ((U) => U)),
                  c: ((T) => ((U) => U)),
                  d: ((T) => ((U) => U)),
                  e: ((T) => ((U) => U))): ((T) => ((U) => U)) = {
    if (e) {
      val args: Array[((T) => ((U) => U))] = Array() {
        var i = 0
        while ((i < arguments.length)) {
          {
            (args(i) = arguments(i))

          }
          (i += 1)
        }
      }
      return (t => compose(map(args, (f => f(t))): _*))

    } else if (d) {
      return (t => compose(a(t), b(t), c(t), d(t)))

    } else if (c) {
      return (t => compose(a(t), b(t), c(t)))

    } else if (b) {
      return (t => compose(a(t), b(t)))

    } else if (a) {
      return (t => compose(a(t)))

    } else {
      return (_underscore_ => (u => u))

    }

  }
  def compose[T](args: Array[((T) => T)]): ((T) => T)
  def compose[T](a: ((T) => T),
                 b: ((T) => T),
                 c: ((T) => T),
                 d: ((T) => T),
                 e: ((T) => T)): ((T) => T) = {
    if (e) {
      val args: Array[((T) => T)] = Array() {
        var i = 0
        while ((i < arguments.length)) {
          {
            (args(i) = arguments(i))

          }
          (i += 1)
        }
      }
      return (t => reduceLeft[((T) => T), T](args, ((u, f) => f(u)), t))

    } else if (d) {
      return (t => d(c(b(a(t)))))

    } else if (c) {
      return (t => c(b(a(t))))

    } else if (b) {
      return (t => b(a(t)))

    } else if (a) {
      return (t => a(t))

    } else {
      return (t => t)

    }

  }
  def formatStringFromArgs(text: String, args: {
    def apply(index: Int): String
    /* def update() -- if you need it */
  }, baseIndex: Int): String = {
    (baseIndex = (baseIndex || 0))
    return text.replace(
      java.util.regex.Pattern.compile(raw"""{(\d+)}""", "g"),
      ((_match, index) => args(((+index) + baseIndex))))

  }
  var localizedDiagnosticMessages: Map[String] = undefined
  def getLocaleSpecificMessage(message: DiagnosticMessage) = {
    return ((localizedDiagnosticMessages && localizedDiagnosticMessages(
      message.key)) || message.message)

  }
  def createFileDiagnostic(file: SourceFile,
                           start: Int,
                           length: Int,
                           message: DiagnosticMessage,
                           args: Array[(String | Int)]): Diagnostic
  def createFileDiagnostic(file: SourceFile,
                           start: Int,
                           length: Int,
                           message: DiagnosticMessage): Diagnostic = {
    val end = (start + length)
    Debug.assert((start >= 0), ("start must be non-negative, is " + start))
    Debug.assert((length >= 0), ("length must be non-negative, is " + length))
    if (file) {
      Debug.assert(
        (start <= file.text.length),
        s"""start must be within the bounds of the file. ${start} > ${file.text.length}""")
      Debug.assert(
        (end <= file.text.length),
        s"""end must be the bounds of the file. ${end} > ${file.text.length}""")

    }
    var text = getLocaleSpecificMessage(message)
    if ((arguments.length > 4)) {
      (text = formatStringFromArgs(text, arguments, 4))

    }
    return Map(
      "file" -> file,
      "start" -> start,
      "length" -> length,
      "messageText" -> text,
      "category" -> message.category,
      "code" -> message.code)

  }
  def formatMessage(_dummy: Any, message: DiagnosticMessage): String = {
    var text = getLocaleSpecificMessage(message)
    if ((arguments.length > 2)) {
      (text = formatStringFromArgs(text, arguments, 2))

    }
    return text

  }
  def createCompilerDiagnostic(message: DiagnosticMessage,
                               args: Array[(String | Int)]): Diagnostic
  def createCompilerDiagnostic(message: DiagnosticMessage): Diagnostic = {
    var text = getLocaleSpecificMessage(message)
    if ((arguments.length > 1)) {
      (text = formatStringFromArgs(text, arguments, 1))

    }
    return Map(
      "file" -> undefined,
      "start" -> undefined,
      "length" -> undefined,
      "messageText" -> text,
      "category" -> message.category,
      "code" -> message.code)

  }
  def createCompilerDiagnosticFromMessageChain(
      chain: DiagnosticMessageChain): Diagnostic = {
    return Map(
      "file" -> undefined,
      "start" -> undefined,
      "length" -> undefined,
      "code" -> chain.code,
      "category" -> chain.category,
      "messageText" -> (if (chain.next) chain else chain.messageText))

  }
  def chainDiagnosticMessages(details: DiagnosticMessageChain,
                              message: DiagnosticMessage,
                              args: Array[Any]): DiagnosticMessageChain
  def chainDiagnosticMessages(
      details: DiagnosticMessageChain,
      message: DiagnosticMessage): DiagnosticMessageChain = {
    var text = getLocaleSpecificMessage(message)
    if ((arguments.length > 2)) {
      (text = formatStringFromArgs(text, arguments, 2))

    }
    return Map(
      "messageText" -> text,
      "category" -> message.category,
      "code" -> message.code,
      "next" -> details)

  }
  def concatenateDiagnosticMessageChains(
      headChain: DiagnosticMessageChain,
      tailChain: DiagnosticMessageChain): DiagnosticMessageChain = {
    var lastChain = headChain
    while (lastChain.next) {
      {
        (lastChain = lastChain.next)

      }
    }
    (lastChain.next = tailChain)
    return headChain

  }
  def compareValues[T](a: T, b: T): Comparison = {
    if ((a === b))
      return Comparison.EqualTo
    if ((a === undefined))
      return Comparison.LessThan
    if ((b === undefined))
      return Comparison.GreaterThan
    return (if ((a < b)) Comparison.LessThan else Comparison.GreaterThan)

  }
  def compareStrings(a: String, b: String, ignoreCase: Boolean): Comparison = {
    if ((a === b))
      return Comparison.EqualTo
    if ((a === undefined))
      return Comparison.LessThan
    if ((b === undefined))
      return Comparison.GreaterThan
    if (ignoreCase) {
      if ((collator && String.prototype.localeCompare)) {
        val result = a.localeCompare(
          b,
          undefined,
          Map("usage" -> "sort", "sensitivity" -> "accent"))
        return (if ((result < 0)) Comparison.LessThan
                else (if ((result > 0)) Comparison.GreaterThan
                      else Comparison.EqualTo))

      }
      (a = a.toUpperCase())
      (b = b.toUpperCase())
      if ((a === b))
        return Comparison.EqualTo

    }
    return (if ((a < b)) Comparison.LessThan else Comparison.GreaterThan)

  }
  def compareStringsCaseInsensitive(a: String, b: String) = {
    return compareStrings(a, b, true)

  }
  def getDiagnosticFileName(diagnostic: Diagnostic): String = {
    return (if (diagnostic.file) diagnostic.file.fileName else undefined)

  }
  def compareDiagnostics(d1: Diagnostic, d2: Diagnostic): Comparison = {
    return (((((compareValues(
      getDiagnosticFileName(d1),
      getDiagnosticFileName(d2)) || compareValues(d1.start, d2.start)) || compareValues(
      d1.length,
      d2.length)) || compareValues(d1.code, d2.code)) || compareMessageText(
      d1.messageText,
      d2.messageText)) || Comparison.EqualTo)

  }
  def compareMessageText(
      text1: (String | DiagnosticMessageChain),
      text2: (String | DiagnosticMessageChain)): Comparison = {
    while ((text1 && text2)) {
      {
        val string1 =
          (if ((typeof(text1) === "string")) text1 else text1.messageText)
        val string2 =
          (if ((typeof(text2) === "string")) text2 else text2.messageText)
        val res = compareValues(string1, string2)
        if (res) {
          return res

        }
        (text1 = (if ((typeof(text1) === "string")) undefined else text1.next))
        (text2 = (if ((typeof(text2) === "string")) undefined else text2.next))

      }
    }
    if (((!text1) && (!text2))) {
      return Comparison.EqualTo

    }
    return (if (text1) Comparison.GreaterThan else Comparison.LessThan)

  }
  def sortAndDeduplicateDiagnostics(
      diagnostics: Array[Diagnostic]): Array[Diagnostic] = {
    return deduplicateSortedDiagnostics(diagnostics.sort(compareDiagnostics))

  }
  def deduplicateSortedDiagnostics(
      diagnostics: Array[Diagnostic]): Array[Diagnostic] = {
    if ((diagnostics.length < 2)) {
      return diagnostics

    }
    val newDiagnostics = Array(diagnostics(0))
    var previousDiagnostic = diagnostics(0) {
      var i = 1
      while ((i < diagnostics.length)) {
        {
          val currentDiagnostic = diagnostics(i)
          val isDupe = (compareDiagnostics(
              currentDiagnostic,
              previousDiagnostic) === Comparison.EqualTo)
          if ((!isDupe)) {
            newDiagnostics.push(currentDiagnostic)
            (previousDiagnostic = currentDiagnostic)

          }

        }
        (i += 1)
      }
    }
    return newDiagnostics

  }
  def normalizeSlashes(path: String): String = {
    return path.replace(java.util.regex.Pattern.compile(raw"""\\""", "g"), "/")

  }
  def getRootLength(path: String): Int = {
    if ((path.charCodeAt(0) === CharacterCodes.slash)) {
      if ((path.charCodeAt(1) !== CharacterCodes.slash))
        return 1
      val p1 = path.indexOf("/", 2)
      if ((p1 < 0))
        return 2
      val p2 = path.indexOf("/", (p1 + 1))
      if ((p2 < 0))
        return (p1 + 1)
      return (p2 + 1)

    }
    if ((path.charCodeAt(1) === CharacterCodes.colon)) {
      if ((path.charCodeAt(2) === CharacterCodes.slash))
        return 3
      return 2

    }
    if ((path.lastIndexOf("file:///", 0) === 0)) {
      return "file:///".length

    }
    val idx = path.indexOf("://")
    if ((idx !== (-1))) {
      return (idx + "://".length)

    }
    return 0

  }
  val directorySeparator = "/"
  val directorySeparatorCharCode = CharacterCodes.slash
  def getNormalizedParts(normalizedSlashedPath: String,
                         rootLength: Int): Array[String] = {
    val parts =
      normalizedSlashedPath.substr(rootLength).split(directorySeparator)
    val normalized: Array[String] = Array()
    (parts).foreach { fresh29 =>
      val part = zeroOfMyType = fresh29 {
        if ((part !== ".")) {
          if ((((part === "..") && (normalized.length > 0)) && (lastOrUndefined(
                normalized) !== ".."))) {
            normalized.pop()

          } else {
            if (part) {
              normalized.push(part)

            }

          }

        }

      }
    }
    return normalized

  }
  def normalizePath(path: String): String = {
    (path = normalizeSlashes(path))
    val rootLength = getRootLength(path)
    val root = path.substr(0, rootLength)
    val normalized = getNormalizedParts(path, rootLength)
    if (normalized.length) {
      val joinedParts = (root + normalized.join(directorySeparator))
      return (if (pathEndsWithDirectorySeparator(path))
                (joinedParts + directorySeparator)
              else joinedParts)

    } else {
      return root

    }

  }
  def pathEndsWithDirectorySeparator(path: String): Boolean = {
    return (path.charCodeAt((path.length - 1)) === directorySeparatorCharCode)

  }
  def getDirectoryPath(path: Path): Path
  def getDirectoryPath(path: String): String
  def getDirectoryPath(path: String): Any = {
    return path.substr(
      0,
      Math.max(getRootLength(path), path.lastIndexOf(directorySeparator)))

  }
  def isUrl(path: String) = {
    return ((path && (!isRootedDiskPath(path))) && (path.indexOf("://") !== (-1)))

  }
  def isExternalModuleNameRelative(moduleName: String): Boolean = {
    return java.util.regex.Pattern
      .compile(raw"""^\.\.?($$|[\\/])""")
      .test(moduleName)

  }
  def getEmitScriptTarget(compilerOptions: CompilerOptions) = {
    return (compilerOptions.target || ScriptTarget.ES3)

  }
  def getEmitModuleKind(compilerOptions: CompilerOptions) = {
    return (if ((typeof(compilerOptions.module) === "number"))
              compilerOptions.module
            else
              (if ((getEmitScriptTarget(compilerOptions) >= ScriptTarget.ES2015))
                 ModuleKind.ES2015
               else ModuleKind.CommonJS))

  }
  def hasZeroOrOneAsteriskCharacter(str: String): Boolean = {
    var seenAsterisk = false {
      var i = 0
      while ((i < str.length)) {
        {
          if ((str.charCodeAt(i) === CharacterCodes.asterisk)) {
            if ((!seenAsterisk)) {
              (seenAsterisk = true)

            } else {
              return false

            }

          }

        }
        (i += 1)
      }
    }
    return true

  }
  def isRootedDiskPath(path: String) = {
    return (getRootLength(path) !== 0)

  }
  def convertToRelativePath(
      absoluteOrRelativePath: String,
      basePath: String,
      getCanonicalFileName: ((String) => String)): String = {
    return (if ((!isRootedDiskPath(absoluteOrRelativePath)))
              absoluteOrRelativePath
            else
              getRelativePathToDirectoryOrUrl(
                basePath,
                absoluteOrRelativePath,
                basePath,
                getCanonicalFileName,
                false))

  }
  def normalizedPathComponents(path: String, rootLength: Int) = {
    val normalizedParts = getNormalizedParts(path, rootLength)
    return Array(path.substr(0, rootLength)).concat(normalizedParts)

  }
  def getNormalizedPathComponents(path: String, currentDirectory: String) = {
    (path = normalizeSlashes(path))
    var rootLength = getRootLength(path)
    if ((rootLength === 0)) {
      (path = combinePaths(normalizeSlashes(currentDirectory), path))
      (rootLength = getRootLength(path))

    }
    return normalizedPathComponents(path, rootLength)

  }
  def getNormalizedAbsolutePath(fileName: String, currentDirectory: String) = {
    return getNormalizedPathFromPathComponents(
      getNormalizedPathComponents(fileName, currentDirectory))

  }
  def getNormalizedPathFromPathComponents(pathComponents: Array[String]) = {
    if ((pathComponents && pathComponents.length)) {
      return (pathComponents(0) + pathComponents
        .slice(1)
        .join(directorySeparator))

    }

  }
  def getNormalizedPathComponentsOfUrl(url: String) = {
    val urlLength = url.length
    var rootLength = (url.indexOf("://") + "://".length)
    while ((rootLength < urlLength)) {
      {
        if ((url.charCodeAt(rootLength) === CharacterCodes.slash)) {
          (rootLength += 1)

        } else {
          break()

        }

      }
    }
    if ((rootLength === urlLength)) {
      return Array(url)

    }
    val indexOfNextSlash = url.indexOf(directorySeparator, rootLength)
    if ((indexOfNextSlash !== (-1))) {
      (rootLength = (indexOfNextSlash + 1))
      return normalizedPathComponents(url, rootLength)

    } else {
      return Array((url + directorySeparator))

    }

  }
  def getNormalizedPathOrUrlComponents(pathOrUrl: String,
                                       currentDirectory: String) = {
    if (isUrl(pathOrUrl)) {
      return getNormalizedPathComponentsOfUrl(pathOrUrl)

    } else {
      return getNormalizedPathComponents(pathOrUrl, currentDirectory)

    }

  }
  def getRelativePathToDirectoryOrUrl(
      directoryPathOrUrl: String,
      relativeOrAbsolutePath: String,
      currentDirectory: String,
      getCanonicalFileName: ((String) => String),
      isAbsolutePathAnUrl: Boolean) = {
    val pathComponents = getNormalizedPathOrUrlComponents(
      relativeOrAbsolutePath,
      currentDirectory)
    val directoryComponents =
      getNormalizedPathOrUrlComponents(directoryPathOrUrl, currentDirectory)
    if (((directoryComponents.length > 1) && (lastOrUndefined(
          directoryComponents) === ""))) {
      (directoryComponents.length -= 1)

    }
    var joinStartIndex: Int = zeroOfMyType {
      (joinStartIndex = 0)
      while (((joinStartIndex < pathComponents.length) && (joinStartIndex < directoryComponents.length))) {
        {
          if ((getCanonicalFileName(directoryComponents(joinStartIndex)) !== getCanonicalFileName(
                pathComponents(joinStartIndex)))) {
            break()

          }

        }
        (joinStartIndex += 1)
      }
    }
    if (joinStartIndex) {
      var relativePath = ""
      val relativePathComponents =
        pathComponents.slice(joinStartIndex, pathComponents.length) {
          while ((joinStartIndex < directoryComponents.length)) {
            {
              if ((directoryComponents(joinStartIndex) !== "")) {
                (relativePath = ((relativePath + "..") + directorySeparator))

              }

            }
            (joinStartIndex += 1)
          }
        }
      return (relativePath + relativePathComponents.join(directorySeparator))

    }
    var absolutePath = getNormalizedPathFromPathComponents(pathComponents)
    if ((isAbsolutePathAnUrl && isRootedDiskPath(absolutePath))) {
      (absolutePath = ("file:///" + absolutePath))

    }
    return absolutePath

  }
  def getBaseFileName(path: String) = {
    if ((path === undefined)) {
      return undefined

    }
    val i = path.lastIndexOf(directorySeparator)
    return (if ((i < 0)) path else path.substring((i + 1)))

  }
  def combinePaths(path1: String, path2: String) = {
    if ((!((path1 && path1.length))))
      return path2
    if ((!((path2 && path2.length))))
      return path1
    if ((getRootLength(path2) !== 0))
      return path2
    if ((path1.charAt((path1.length - 1)) === directorySeparator))
      return (path1 + path2)
    return ((path1 + directorySeparator) + path2)

  }
  def removeTrailingDirectorySeparator(path: String) = {
    if ((path.charAt((path.length - 1)) === directorySeparator)) {
      return path.substr(0, (path.length - 1))

    }
    return path

  }
  def ensureTrailingDirectorySeparator(path: String) = {
    if ((path.charAt((path.length - 1)) !== directorySeparator)) {
      return (path + directorySeparator)

    }
    return path

  }
  def comparePaths(a: String,
                   b: String,
                   currentDirectory: String,
                   ignoreCase: Boolean) = {
    if ((a === b))
      return Comparison.EqualTo
    if ((a === undefined))
      return Comparison.LessThan
    if ((b === undefined))
      return Comparison.GreaterThan
    (a = removeTrailingDirectorySeparator(a))
    (b = removeTrailingDirectorySeparator(b))
    val aComponents = getNormalizedPathComponents(a, currentDirectory)
    val bComponents = getNormalizedPathComponents(b, currentDirectory)
    val sharedLength = Math.min(aComponents.length, bComponents.length) {
      var i = 0
      while ((i < sharedLength)) {
        {
          val result =
            compareStrings(aComponents(i), bComponents(i), ignoreCase)
          if ((result !== Comparison.EqualTo)) {
            return result

          }

        }
        (i += 1)
      }
    }
    return compareValues(aComponents.length, bComponents.length)

  }
  def containsPath(parent: String,
                   child: String,
                   currentDirectory: String,
                   ignoreCase: Boolean) = {
    if (((parent === undefined) || (child === undefined)))
      return false
    if ((parent === child))
      return true
    (parent = removeTrailingDirectorySeparator(parent))
    (child = removeTrailingDirectorySeparator(child))
    if ((parent === child))
      return true
    val parentComponents =
      getNormalizedPathComponents(parent, currentDirectory)
    val childComponents = getNormalizedPathComponents(child, currentDirectory)
    if ((childComponents.length < parentComponents.length)) {
      return false

    }
    {
      var i = 0
      while ((i < parentComponents.length)) {
        {
          val result =
            compareStrings(parentComponents(i), childComponents(i), ignoreCase)
          if ((result !== Comparison.EqualTo)) {
            return false

          }

        }
        (i += 1)
      }
    }
    return true

  }
  def startsWith(str: String, prefix: String): Boolean = {
    return (str.lastIndexOf(prefix, 0) === 0)

  }
  def endsWith(str: String, suffix: String): Boolean = {
    val expectedPos = (str.length - suffix.length)
    return ((expectedPos >= 0) && (str
      .indexOf(suffix, expectedPos) === expectedPos))

  }
  def fileExtensionIs(path: String, extension: String): Boolean = {
    return ((path.length > extension.length) && endsWith(path, extension))

  }
  def fileExtensionIsAny(path: String, extensions: Array[String]): Boolean = {
    (extensions).foreach { fresh30 =>
      val extension = zeroOfMyType = fresh30 {
        if (fileExtensionIs(path, extension)) {
          return true

        }

      }
    }
    return false

  }
  val reservedCharacterPattern =
    java.util.regex.Pattern.compile(raw"""[^\w\s\/]""", "g")
  val wildcardCharCodes =
    Array(CharacterCodes.asterisk, CharacterCodes.question)
  val singleAsteriskRegexFragmentFiles = "([^./]|(\\.(?!min\\.js$))?)*"
  val singleAsteriskRegexFragmentOther = "[^/]*"
  def getRegularExpressionForWildcard(
      specs: Array[String],
      basePath: String,
      usage: (`"files"` | `"directories"` | `"exclude"`)) = {
    if (((specs === undefined) || (specs.length === 0))) {
      return undefined

    }
    val replaceWildcardCharacter =
      (if ((usage === "files")) replaceWildCardCharacterFiles
       else replaceWildCardCharacterOther)
    val singleAsteriskRegexFragment =
      (if ((usage === "files")) singleAsteriskRegexFragmentFiles
       else singleAsteriskRegexFragmentOther)
    val doubleAsteriskRegexFragment =
      (if ((usage === "exclude")) "(/.+?)?" else "(/[^/.][^/]*)*?")
    var pattern = ""
    var hasWrittenSubpattern = false
    val spec = new scala.util.control.Breaks
    spec.breakable {
      (specs).foreach { fresh31 =>
        val spec = zeroOfMyType = fresh31 {
          if ((!spec)) {
            continue

          }
          var subpattern = ""
          var hasRecursiveDirectoryWildcard = false
          var hasWrittenComponent = false
          val components = getNormalizedPathComponents(spec, basePath)
          if (((usage !== "exclude") && (components((components.length - 1)) === "**"))) {
            continue spec

          }
          (components(0) = removeTrailingDirectorySeparator(components(0)))
          var optionalCount = 0
          (components).foreach { fresh32 =>
            var component = zeroOfMyType = fresh32 {
              if ((component === "**")) {
                if (hasRecursiveDirectoryWildcard) {
                  continue spec

                }
                (subpattern += doubleAsteriskRegexFragment)
                (hasRecursiveDirectoryWildcard = true)
                (hasWrittenComponent = true)

              } else {
                if ((usage === "directories")) {
                  (subpattern += "(")
                  (optionalCount += 1)

                }
                if (hasWrittenComponent) {
                  (subpattern += directorySeparator)

                }
                if ((usage !== "exclude")) {
                  if ((component.charCodeAt(0) === CharacterCodes.asterisk)) {
                    (subpattern += (("([^./]" + singleAsteriskRegexFragment) + ")?"))
                    (component = component.substr(1))

                  } else if ((component.charCodeAt(0) === CharacterCodes.question)) {
                    (subpattern += "[^./]")
                    (component = component.substr(1))

                  }

                }
                (subpattern += component
                  .replace(reservedCharacterPattern, replaceWildcardCharacter))
                (hasWrittenComponent = true)

              }

            }
          }
          while ((optionalCount > 0)) {
            {
              (subpattern += ")?")
              (optionalCount -= 1)

            }
          }
          if (hasWrittenSubpattern) {
            (pattern += "|")

          }
          (pattern += (("(" + subpattern) + ")"))
          (hasWrittenSubpattern = true)

        }
      }
    }
    if ((!pattern)) {
      return undefined

    }
    return (("^(" + pattern) + ((if ((usage === "exclude")) ")($|/)"
                                 else ")$") ))

  }
  def replaceWildCardCharacterFiles(`match`: String) = {
    return replaceWildcardCharacter(`match`, singleAsteriskRegexFragmentFiles)

  }
  def replaceWildCardCharacterOther(`match`: String) = {
    return replaceWildcardCharacter(`match`, singleAsteriskRegexFragmentOther)

  }
  def replaceWildcardCharacter(`match`: String,
                               singleAsteriskRegexFragment: String) = {
    return (if ((`match` === "*")) singleAsteriskRegexFragment
            else (if ((`match` === "?")) "[^/]"
                  else ("\\" + `match`)))

  }
  trait FileSystemEntries {
    var files: Array[String]
    var directories: Array[String]
  }
  trait FileMatcherPatterns {
    var includeFilePattern: String
    var includeDirectoryPattern: String
    var excludePattern: String
    var basePaths: Array[String]
  }
  def getFileMatcherPatterns(path: String,
                             excludes: Array[String],
                             includes: Array[String],
                             useCaseSensitiveFileNames: Boolean,
                             currentDirectory: String): FileMatcherPatterns = {
    (path = normalizePath(path))
    (currentDirectory = normalizePath(currentDirectory))
    val absolutePath = combinePaths(currentDirectory, path)
    return Map(
      "includeFilePattern" -> getRegularExpressionForWildcard(
        includes,
        absolutePath,
        "files"),
      "includeDirectoryPattern" -> getRegularExpressionForWildcard(
        includes,
        absolutePath,
        "directories"),
      "excludePattern" -> getRegularExpressionForWildcard(
        excludes,
        absolutePath,
        "exclude"),
      "basePaths" -> getBasePaths(path, includes, useCaseSensitiveFileNames))

  }
  def matchFiles(
      path: String,
      extensions: Array[String],
      excludes: Array[String],
      includes: Array[String],
      useCaseSensitiveFileNames: Boolean,
      currentDirectory: String,
      getFileSystemEntries: ((String) => FileSystemEntries)): Array[String] = {
    (path = normalizePath(path))
    (currentDirectory = normalizePath(currentDirectory))
    val patterns = getFileMatcherPatterns(
      path,
      excludes,
      includes,
      useCaseSensitiveFileNames,
      currentDirectory)
    val regexFlag = (if (useCaseSensitiveFileNames) "" else "i")
    val includeFileRegex = (patterns.includeFilePattern && new RegExp(
        patterns.includeFilePattern,
        regexFlag))
    val includeDirectoryRegex = (patterns.includeDirectoryPattern && new RegExp(
        patterns.includeDirectoryPattern,
        regexFlag))
    val excludeRegex = (patterns.excludePattern && new RegExp(
        patterns.excludePattern,
        regexFlag))
    val result: Array[String] = Array()
    (patterns.basePaths).foreach { fresh33 =>
      val basePath = zeroOfMyType = fresh33 {
        visitDirectory(basePath, combinePaths(currentDirectory, basePath))

      }
    }
    return result
    def visitDirectory(path: String, absolutePath: String) = {
      const fresh34 = getFileSystemEntries(path)
      val files = fresh34.files
      val directories = fresh34.directories
      (files).foreach { fresh35 =>
        val current = zeroOfMyType = fresh35 {
          val name = combinePaths(path, current)
          val absoluteName = combinePaths(absolutePath, current)
          if ((((((!extensions) || fileExtensionIsAny(name, extensions))) && (((!includeFileRegex) || includeFileRegex
                .test(absoluteName)))) && (((!excludeRegex) || (!excludeRegex
                .test(absoluteName)))))) {
            result.push(name)

          }

        }
      }
      (directories).foreach { fresh36 =>
        val current = zeroOfMyType = fresh36 {
          val name = combinePaths(path, current)
          val absoluteName = combinePaths(absolutePath, current)
          if (((((!includeDirectoryRegex) || includeDirectoryRegex.test(
                absoluteName))) && (((!excludeRegex) || (!excludeRegex.test(
                absoluteName)))))) {
            visitDirectory(name, absoluteName)

          }

        }
      }

    }

  }
  def getBasePaths(path: String,
                   includes: Array[String],
                   useCaseSensitiveFileNames: Boolean) = {
    val basePaths: Array[String] = Array(path)
    if (includes) {
      val includeBasePaths: Array[String] = Array()
      (includes).foreach { fresh37 =>
        val include = zeroOfMyType = fresh37 {
          val absolute: String =
            (if (isRootedDiskPath(include)) include
             else normalizePath(combinePaths(path, include)))
          val wildcardOffset = indexOfAnyCharCode(absolute, wildcardCharCodes)
          val includeBasePath =
            (if ((wildcardOffset < 0))
               removeTrailingDirectorySeparator(getDirectoryPath(absolute))
             else
               absolute.substring(
                 0,
                 absolute.lastIndexOf(directorySeparator, wildcardOffset)))
          includeBasePaths.push(includeBasePath)

        }
      }
      includeBasePaths.sort(
        (if (useCaseSensitiveFileNames) compareStrings
         else compareStringsCaseInsensitive))
      val include = new scala.util.control.Breaks
      include.breakable {
        {
          var i = 0
          while ((i < includeBasePaths.length)) {
            {
              val includeBasePath = includeBasePaths(i) {
                var j = 0
                while ((j < basePaths.length)) {
                  {
                    if (containsPath(
                          basePaths(j),
                          includeBasePath,
                          path,
                          (!useCaseSensitiveFileNames))) {
                      continue include

                    }

                  }
                  (j += 1)
                }
              }
              basePaths.push(includeBasePath)

            }
            (i += 1)
          }
        }
      }
    }
    return basePaths

  }
  def ensureScriptKind(fileName: String, scriptKind: ScriptKind): ScriptKind = {
    return (((scriptKind || getScriptKindFromFileName(fileName))) || ScriptKind.TS)

  }
  def getScriptKindFromFileName(fileName: String): ScriptKind = {
    val ext = fileName.substr(fileName.lastIndexOf("."))
    ext.toLowerCase() match {
      case ".js" =>
        return ScriptKind.JS
      case ".jsx" =>
        return ScriptKind.JSX
      case ".ts" =>
        return ScriptKind.TS
      case ".tsx" =>
        return ScriptKind.TSX
      case _ =>
        return ScriptKind.Unknown
    }

  }
  val supportedTypeScriptExtensions = Array(".ts", ".tsx", ".d.ts")
  val supportedTypescriptExtensionsForExtractExtension =
    Array(".d.ts", ".ts", ".tsx")
  val supportedJavascriptExtensions = Array(".js", ".jsx")
  val allSupportedExtensions =
    supportedTypeScriptExtensions.concat(supportedJavascriptExtensions)
  def getSupportedExtensions(options: CompilerOptions): Array[String] = {
    return (if ((options && options.allowJs)) allSupportedExtensions
            else supportedTypeScriptExtensions)

  }
  def hasJavaScriptFileExtension(fileName: String) = {
    return forEach(
      supportedJavascriptExtensions,
      (extension => fileExtensionIs(fileName, extension)))

  }
  def hasTypeScriptFileExtension(fileName: String) = {
    return forEach(
      supportedTypeScriptExtensions,
      (extension => fileExtensionIs(fileName, extension)))

  }
  def isSupportedSourceFileName(fileName: String,
                                compilerOptions: CompilerOptions) = {
    if ((!fileName)) {
      return false

    }
    (getSupportedExtensions(compilerOptions)).foreach { fresh38 =>
      val extension = zeroOfMyType = fresh38 {
        if (fileExtensionIs(fileName, extension)) {
          return true

        }

      }
    }
    return false

  }
  sealed abstract class ExtensionPriority
  object ExtensionPriority {
    case object TypeScriptFiles extends ExtensionPriority
    case object DeclarationAndJavaScriptFiles extends ExtensionPriority
    case object Limit extends ExtensionPriority
    case object Highest extends ExtensionPriority
    case object Lowest extends ExtensionPriority
  }
  def getExtensionPriority(
      path: String,
      supportedExtensions: Array[String]): ExtensionPriority = {
    {
      var i = (supportedExtensions.length - 1)
      while ((i >= 0)) {
        {
          if (fileExtensionIs(path, supportedExtensions(i))) {
            return adjustExtensionPriority(i.asInstanceOf[ExtensionPriority])

          }

        }
        (i -= 1)
      }
    }
    return ExtensionPriority.Highest

  }
  def adjustExtensionPriority(
      extensionPriority: ExtensionPriority): ExtensionPriority = {
    if ((extensionPriority < ExtensionPriority.DeclarationAndJavaScriptFiles)) {
      return ExtensionPriority.TypeScriptFiles

    } else if ((extensionPriority < ExtensionPriority.Limit)) {
      return ExtensionPriority.DeclarationAndJavaScriptFiles

    } else {
      return ExtensionPriority.Limit

    }

  }
  def getNextLowestExtensionPriority(
      extensionPriority: ExtensionPriority): ExtensionPriority = {
    if ((extensionPriority < ExtensionPriority.DeclarationAndJavaScriptFiles)) {
      return ExtensionPriority.DeclarationAndJavaScriptFiles

    } else {
      return ExtensionPriority.Limit

    }

  }
  val extensionsToRemove = Array(".d.ts", ".ts", ".js", ".tsx", ".jsx")
  def removeFileExtension(path: String): String = {
    (extensionsToRemove).foreach { fresh39 =>
      val ext = zeroOfMyType = fresh39 {
        val extensionless = tryRemoveExtension(path, ext)
        if ((extensionless !== undefined)) {
          return extensionless

        }

      }
    }
    return path

  }
  def tryRemoveExtension(path: String,
                         extension: String): (String | undefined) = {
    return (if (fileExtensionIs(path, extension))
              removeExtension(path, extension)
            else undefined)

  }
  def removeExtension(path: String, extension: String): String = {
    return path.substring(0, (path.length - extension.length))

  }
  def isJsxOrTsxExtension(ext: String): Boolean = {
    return ((ext === ".jsx") || (ext === ".tsx"))

  }
  def changeExtension[T <: (String | Path)](path: T, newExtension: String): T = {
    return ((removeFileExtension(path) + newExtension)).asInstanceOf[T]

  }
  trait ObjectAllocator {
    def getNodeConstructor(): ((SyntaxKind, Int, Int) => Node)
    def getTokenConstructor(): ((TKind, Int, Int) => Token[TKind])
    def getIdentifierConstructor()
      : ((SyntaxKind.Identifier, Int, Int) => Identifier)
    def getSourceFileConstructor()
      : ((SyntaxKind.SourceFile, Int, Int) => SourceFile)
    def getSymbolConstructor(): ((SymbolFlags, String) => Symbol)
    def getTypeConstructor(): ((TypeChecker, TypeFlags) => Type)
    def getSignatureConstructor(): ((TypeChecker) => Signature)
  }
  def Symbol(`this`: Symbol, flags: SymbolFlags, name: String) = {
    (this.flags = flags)
    (this.name = name)
    (this.declarations = undefined)

  }
  def Type(`this`: Type, _checker: TypeChecker, flags: TypeFlags) = {
    (this.flags = flags)

  }
  def Signature() = {}
  def Node(`this`: Node, kind: SyntaxKind, pos: Int, end: Int) = {
    (this.id = 0)
    (this.kind = kind)
    (this.pos = pos)
    (this.end = end)
    (this.flags = NodeFlags.None)
    (this.modifierFlagsCache = ModifierFlags.None)
    (this.transformFlags = TransformFlags.None)
    (this.parent = undefined)
    (this.original = undefined)

  }
  var objectAllocator: ObjectAllocator = Map(
    "getNodeConstructor" -> (() => Node.asInstanceOf[Any]),
    "getTokenConstructor" -> (() => Node.asInstanceOf[Any]),
    "getIdentifierConstructor" -> (() => Node.asInstanceOf[Any]),
    "getSourceFileConstructor" -> (() => Node.asInstanceOf[Any]),
    "getSymbolConstructor" -> (() => Symbol.asInstanceOf[Any]),
    "getTypeConstructor" -> (() => Type.asInstanceOf[Any]),
    "getSignatureConstructor" -> (() => Signature.asInstanceOf[Any]))
  sealed abstract class AssertionLevel
  object AssertionLevel {
    case object None extends AssertionLevel
    case object Normal extends AssertionLevel
    case object Aggressive extends AssertionLevel
    case object VeryAggressive extends AssertionLevel
  }
  object Debug {
    var currentAssertionLevel = AssertionLevel.None
    def shouldAssert(level: AssertionLevel): Boolean = {
      return (currentAssertionLevel >= level)

    }
    def assert(expression: Boolean,
               message: String,
               verboseDebugInfo: (() => String)): Unit = {
      if ((!expression)) {
        var verboseDebugString = ""
        if (verboseDebugInfo) {
          (verboseDebugString = ("\r\nVerbose Debug Information: " + verboseDebugInfo()))

        };
        throw new Error(
          (("Debug Failure. False expression: " + ((message || ""))) + verboseDebugString))
      }

    }
    def fail(message: String): Unit = {
      Debug.assert(false, message)

    }
  }
  def orderedRemoveItemAt[T](array: Array[T], index: Int): Unit = {
    {
      var i = index
      while ((i < (array.length - 1))) {
        {
          (array(i) = array((i + 1)))

        }
        (i += 1)
      }
    }
    array.pop()

  }
  def unorderedRemoveItemAt[T](array: Array[T], index: Int): Unit = {
    (array(index) = array((array.length - 1)))
    array.pop()

  }
  def unorderedRemoveItem[T](array: Array[T], item: T): Unit = {
    unorderedRemoveFirstItemWhere(array, (element => (element === item)))

  }
  def unorderedRemoveFirstItemWhere[T](array: Array[T],
                                       predicate: ((T) => Boolean)): Unit = {
    {
      var i = 0
      while ((i < array.length)) {
        {
          if (predicate(array(i))) {
            unorderedRemoveItemAt(array, i)
            break()

          }

        }
        (i += 1)
      }
    }

  }
  def createGetCanonicalFileName(
      useCaseSensitiveFileNames: Boolean): ((String) => String) = {
    return (if (useCaseSensitiveFileNames) ((fileName => fileName))
            else ((fileName => fileName.toLowerCase())))

  }
  def matchPatternOrExact(
      patternStrings: Array[String],
      candidate: String): (String | Pattern | undefined) = {
    val patterns: Array[Pattern] = Array()
    (patternStrings).foreach { fresh40 =>
      val patternString = zeroOfMyType = fresh40 {
        val pattern = tryParsePattern(patternString)
        if (pattern) {
          patterns.push(pattern)

        } else if ((patternString === candidate)) {
          return patternString

        }

      }
    }
    return findBestPatternMatch(
      patterns,
      (_underscore_ => _underscore_),
      candidate)

  }
  def patternText(tuple: Pattern): String = {
    const fresh41 = tuple
    val prefix = fresh41.prefix
    val suffix = fresh41.suffix
    return s"""${prefix}*${suffix}"""

  }
  def matchedText(pattern: Pattern, candidate: String): String = {
    Debug.assert(isPatternMatch(pattern, candidate))
    return candidate.substr(
      pattern.prefix.length,
      (candidate.length - pattern.suffix.length))

  }
  def findBestPatternMatch[T](values: Array[T],
                              getPattern: ((T) => Pattern),
                              candidate: String): (T | undefined) = {
    var matchedValue: (T | undefined) = undefined
    var longestMatchPrefixLength = (-1)
    (values).foreach { fresh42 =>
      val v = zeroOfMyType = fresh42 {
        val pattern = getPattern(v)
        if ((isPatternMatch(pattern, candidate) && (pattern.prefix.length > longestMatchPrefixLength))) {
          (longestMatchPrefixLength = pattern.prefix.length)
          (matchedValue = v)

        }

      }
    }
    return matchedValue

  }
  def isPatternMatch(tuple: Pattern, candidate: String) = {
    const fresh43 = tuple
    val prefix = fresh43.prefix
    val suffix = fresh43.suffix
    return (((candidate.length >= (prefix.length + suffix.length)) && startsWith(
      candidate,
      prefix)) && endsWith(candidate, suffix))

  }
  def tryParsePattern(pattern: String): (Pattern | undefined) = {
    Debug.assert(hasZeroOrOneAsteriskCharacter(pattern))
    val indexOfStar = pattern.indexOf("*")
    return (if ((indexOfStar === (-1))) undefined
            else
              Map(
                "prefix" -> pattern.substr(0, indexOfStar),
                "suffix" -> pattern.substr((indexOfStar + 1))))

  }
  def positionIsSynthesized(pos: Int): Boolean = {
    return (!((pos >= 0)))

  }
}
