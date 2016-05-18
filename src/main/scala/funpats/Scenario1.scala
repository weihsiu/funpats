package funpats

import scala.util.control.NonFatal

/**
  * Created by walter
  */
object Scenario1 {
  trait InternalApi {
    def getWarehouse(id: String): Option[Warehouse]
    def getSupplier(warehouse: Warehouse, id: String): Option[Supplier]
    def getProduct(supplier: Supplier, id: String): Option[Product]
  }
  val internalApi: InternalApi = ???
  import internalApi._
  trait PublicApi {
    def getProductName(warehouseId: String, supplierId: String, productId: String): Option[String]
  }
  class NaivePublicApi extends PublicApi {
    def getProductName(warehouseId: String, supplierId: String, productId: String): Option[String] =
      try {
        val warehouse = getWarehouse(warehouseId).get
        val supplier = getSupplier(warehouse, supplierId).get
        val product = getProduct(supplier, productId).get
        Some(product.name)
      }
      catch {
        case NonFatal(e) => None
      }
  }
  class FlatMapPublicApi extends PublicApi {
    def getProductName(warehouseId: String, supplierId: String, productId: String): Option[String] =
      getWarehouse(warehouseId).flatMap(warehouse =>
        getSupplier(warehouse, supplierId).flatMap(supplier =>
          getProduct(supplier, productId).map(product =>
            product.name
          )
        )
      )
  }
  class ForComprehensionPublicApi extends PublicApi {
    def getProductName(warehouseId: String, supplierId: String, productId: String): Option[String] =
      for {
        warehouse <- getWarehouse(warehouseId)
        supplier <- getSupplier(warehouse, supplierId)
        product <- getProduct(supplier, productId)
      } yield product.name
  }
}
