package funpats

/**
  * Created by walter
  */
case class Warehouse(id: String, name: String)
case class Supplier(id: String, name: String)
case class Product(id: String, name: String)

sealed trait Tree[A]
case class Branch[A](left: Tree[A], value: A, right: Tree[A]) extends Tree[A]
case class Leaf[A](value: A) extends Tree[A]