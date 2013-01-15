package streams

import scala.collection.immutable.BitSet

trait NewTerrain extends BridgeStateTerrain {

  lazy override val terrain1: Terrain1 = (block: Block, state: State) =>
  {
    val bitSet = state.state
    val r = for (i <- 0 to switches.length - 1; sw = switches.apply(i) if sw.isStepOn(block) ) yield
    {
      (i, sw)
    }

    var q: BitSet = state.state
    
    r.foreach(f => {
      val z = f._2
      z match
      {
        case p:ChangeStateFunc => q = p.change(q)
        case _ => q
      }
    })
    
    new State(q)
  }
}
