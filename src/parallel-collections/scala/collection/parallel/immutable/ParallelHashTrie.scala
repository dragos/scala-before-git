package scala.collection.parallel.immutable







import scala.collection.parallel.ParallelMap
import scala.collection.parallel.ParallelMapLike
import scala.collection.parallel.Combiner
import scala.collection.parallel.EnvironmentPassingCombiner
import scala.collection.generic.ParallelMapFactory
import scala.collection.generic.CanBuildFromParallel
import scala.collection.immutable.HashMap






class ParallelHashTrie[K, +V] private[immutable] (private[this] val trie: HashMap[K, V])
extends ParallelMap[K, V]
   with ParallelMapLike[K, V, ParallelHashTrie[K, V], HashMap[K, V]]
{ self =>
  
  def this() = this(HashMap.empty[K, V])
  
  override def empty: ParallelHashTrie[K, V] = new ParallelHashTrie[K, V]
  
  def parallelIterator = new ParallelHashTrieIterator(trie) with SCPI
  
  def seq = trie
  
  def -(k: K) = new ParallelHashTrie(trie - k)
  
  def +[U >: V](kv: (K, U)) = new ParallelHashTrie(trie + kv)
  
  def get(k: K) = trie.get(k)
  
  override def size = trie.size
  
  type SCPI = SignalContextPassingIterator[ParallelHashTrieIterator]
  
  class ParallelHashTrieIterator(val ht: HashMap[K, V])
  extends super.ParallelIterator {
  self: SignalContextPassingIterator[ParallelHashTrieIterator] =>
    // println("created iterator " + ht)
    var i = 0
    lazy val triter = ht.iterator
    def split: Seq[ParallelIterator] = {
      // println("splitting " + ht + " into " + ht.split.map(new ParallelHashTrieIterator(_) with SCPI).map(_.toList))
      ht.split.map(new ParallelHashTrieIterator(_) with SCPI)
    }
    def next: (K, V) = {
      // println("taking next after " + i + ", in " + ht)
      i += 1
      triter.next
    }
    def hasNext: Boolean = {
      // println("hasNext: " + i + ", " + ht.size + ", " + ht)
      i < ht.size
    }
    def remaining = ht.size - i
  }
  
}


object ParallelHashTrie extends ParallelMapFactory[ParallelHashTrie] {
  def empty[K, V]: ParallelHashTrie[K, V] = new ParallelHashTrie[K, V]
  
  implicit def canBuildFrom[K, V]: CanBuildFromParallel[Coll, (K, V), ParallelHashTrie[K, V]] = new ParallelMapCanBuildFrom[K, V]
}


trait HashTrieCombiner[K, V]
extends Combiner[(K, V), ParallelHashTrie[K, V]] {
self: EnvironmentPassingCombiner[(K, V), ParallelHashTrie[K, V]] =>
  private var trie: HashMap[K, V] = HashMap.empty[K, V]
  
  def size: Int = trie.size
  
  def clear = trie = HashMap.empty[K, V]
  
  def +=(elem: (K, V)) = { trie += elem; this }
  
  def result = new ParallelHashTrie[K, V](trie)
  
  def combine[N <: (K, V), NewTo >: ParallelHashTrie[K, V]](other: Combiner[N, NewTo]): Combiner[N, NewTo] = {
    if (other.isInstanceOf[HashTrieCombiner[_, _]]) {
      val that = other.asInstanceOf[HashTrieCombiner[K, V]]
      val ncombiner = HashTrieCombiner[K, V]
      ncombiner.trie = this.trie combine that.trie
      ncombiner
    } else error("Unexpected combiner type.")
  }
  
}


object HashTrieCombiner {
  def apply[K, V] = new HashTrieCombiner[K, V] with EnvironmentPassingCombiner[(K, V), ParallelHashTrie[K, V]] {}
}















