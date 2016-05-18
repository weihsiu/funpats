package funpats

import Monad.ops._
import scala.language.postfixOps
import scala.util.control.NonFatal

/**
  * Created by walter
  */
object Scenario3 {
  trait InternalApi {
    case class Config()
    def getWarehouse(id: String): Config => Warehouse
    def getSupplier(warehouse: Warehouse, id: String): Config => Supplier
    def getProduct(supplier: Supplier, id: String): Config => Product
  }
  val internalApi: InternalApi = ???
  import internalApi._
  trait PublicApi {
    def getProductName(warehouseId: String, supplierId: String, productId: String): Config => String
  }
  class NaivePublicApi extends PublicApi {
    def getProductName(warehouseId: String, supplierId: String, productId: String): Config => String = config => {
      val warehouse = getWarehouse(warehouseId)(config)
      val supplier = getSupplier(warehouse, supplierId)(config)
      val product = getProduct(supplier, productId)(config)
      product.name
    }
  }
  class FlatMapPublicApi extends PublicApi {
    def getProductName(warehouseId: String, supplierId: String, productId: String): Config => String =
      getWarehouse(warehouseId).flatMap(warehouse =>
        getSupplier(warehouse, supplierId).flatMap(supplier =>
          getProduct(supplier, productId).map(product =>
            product.name
          )
        )
      )
  }
  class ForComprehensionPublicApi extends PublicApi {
    def getProductName(warehouseId: String, supplierId: String, productId: String): Config => String =
      for {
        warehouse <- getWarehouse(warehouseId)
        supplier <- getSupplier(warehouse, supplierId)
        product <- getProduct(supplier, productId)
      } yield product.name
  }
}
