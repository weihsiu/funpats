package funpats

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.control.NonFatal

/**
  * Created by walter
  */
object Scenario2 {
  trait InternalApi {
    def getWarehouse(id: String): Future[Warehouse]
    def getSupplier(warehouse: Warehouse, id: String): Future[Supplier]
    def getProduct(supplier: Supplier, id: String): Future[Product]
  }
  val internalApi: InternalApi = ???
  import internalApi._
  trait PublicApi {
    def getProductName(warehouseId: String, supplierId: String, productId: String): Future[String]
  }
  class NaivePublicApi extends PublicApi {
    def getProductName(warehouseId: String, supplierId: String, productId: String): Future[String] =
      try {
        val warehouse = Await.result(getWarehouse(warehouseId), 5 seconds)
        val supplier = Await.result(getSupplier(warehouse, supplierId), 5 seconds)
        val product = Await.result(getProduct(supplier, productId), 5 seconds)
        Future.successful(product.name)
      }
      catch {
        case NonFatal(e) => Future.failed(e)
      }
  }
  class FlatMapPublicApi extends PublicApi {
    def getProductName(warehouseId: String, supplierId: String, productId: String): Future[String] =
      getWarehouse(warehouseId).flatMap(warehouse =>
        getSupplier(warehouse, supplierId).flatMap(supplier =>
          getProduct(supplier, productId).map(product =>
            product.name
          )
        )
      )
  }
  class ForComprehensionPublicApi extends PublicApi {
    def getProductName(warehouseId: String, supplierId: String, productId: String): Future[String] =
      for {
        warehouse <- getWarehouse(warehouseId)
        supplier <- getSupplier(warehouse, supplierId)
        product <- getProduct(supplier, productId)
      } yield product.name
  }
}
