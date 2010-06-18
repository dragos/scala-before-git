package scala.collection.parallel.immutable


import scala.collection.generic.GenericParallelTemplate
import scala.collection.generic.GenericCompanion
import scala.collection.generic.GenericParallelCompanion
import scala.collection.generic.CanCombineFrom
import scala.collection.generic.ParallelFactory
import scala.collection.parallel.ParallelSeqLike
import scala.collection.parallel.Combiner






// TODO uncomment when we add parallel vectors

///** An immutable variant of `ParallelSeq`.
// *  
// *  @define Coll mutable.ParallelSeq
// *  @define coll mutable parallel sequence
// */
//trait ParallelSeq[A] extends collection.immutable.IndexedSeq[A]
//                        with ParallelIterable[A]
//                        with collection.parallel.ParallelSeq[A]
//                        with GenericParallelTemplate[A, ParallelSeq]
//                        with ParallelSeqLike[A, ParallelSeq[A], Seq[A]] {
//  override def companion: GenericCompanion[ParallelSeq] with GenericParallelCompanion[ParallelSeq] = ParallelSeq
//  
//}
//
//
///** $factoryInfo
// *  @define Coll mutable.ParallelSeq
// *  @define coll mutable parallel sequence
// */
//object ParallelSeq extends ParallelFactory[ParallelSeq] {
//  implicit def canBuildFrom[T]: CanBuildFromParallel[Coll, T, ParallelSeq[T]] = new GenericCanBuildFromParallel[T]
//  
//  def newBuilder[A]: Combiner[A, ParallelSeq[A]] = null // TODO
//  
//  def newCombiner[A]: Combiner[A, ParallelSeq[A]] = null // TODO
//}



