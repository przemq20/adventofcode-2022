package day5

import day5.SupplyStacks.Stack

case class Order(quantity: Int, beginStack: Int, endStack: Int) {
  def executeOrder(stacks: List[Stack], reverse: Boolean): List[Stack] = {

    val itemsToRemove =
      if (reverse) stacks(beginStack - 1).items.take(quantity).reverse else stacks(beginStack - 1).items.take(quantity)
    val oldStack = stacks(beginStack - 1).copy(items = stacks(beginStack - 1).items.drop(quantity))
    val newStack = stacks(endStack - 1).copy(items   = itemsToRemove ::: stacks(endStack - 1).items)

    val stack = stacks.take(beginStack - 1) ::: oldStack :: stacks.drop(beginStack)
    stack.take(endStack - 1) ::: newStack :: stack.drop(endStack)
  }
}
