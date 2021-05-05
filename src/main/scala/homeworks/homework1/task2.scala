package homeworks.homework1

sealed trait PaymentSystem

case object MasterCard extends PaymentSystem

case object Visa extends PaymentSystem

case object Maestro extends PaymentSystem

case object Mir extends PaymentSystem

/**
 * Размер платежа
 *
 * @param amount   сумма
 * @param currency валюта
 */
final case class Price(amount: Int, currency: String)

sealed trait PaymentInfo {
  def price: Price
}

/**
 * Оплата через дебетовую карту
 *
 * @param price         размер платежа
 * @param paymentSystem платежная система
 */
final case class DebitCard(price: Price, paymentSystem: PaymentSystem) extends PaymentInfo

/**
 * Оплата наличными
 *
 * @param price                размер платежа
 * @param isLargeDenominations осуществляется ли оплата большими номиналами
 */
final case class Cash(price: Price, isLargeDenominations: Boolean) extends PaymentInfo

/**
 * Оплата в кредит
 *
 * @param price  размер платежа
 * @param rating кредитный рейтинг
 */
final case class Loan(price: Price, rating: Int) extends PaymentInfo


object task2 extends App {
  /**
   * С помощью *pattern matching* реализуйте функцию isSafePayment, определяющиую, является ли платеж безопасным.
   *
   * Платеж считается безопасным, если:
   *  - Оплата через дебетовую карту
   *    платежная система - MasterCard или Visa
   *  - Оплата наличными
   *    валюта - "RUB", сумма - не более 5000, оплата - не большими номиналами
   *  - Оплата в кредит
   *    кредитный рейтинг - выше 5
   *
   * @param paymentInfo информация о платеже
   * @return true, если платеж безопасен, иначе false
   */
  def isSafePayment(paymentInfo: PaymentInfo): Boolean =
    paymentInfo match {
      case DebitCard(_, paymentSystem) =>
        paymentSystem match {
          case MasterCard | Visa => true
          case _ => false
        }
      case Cash(price, isLargeDenominations) =>
        price.amount <= 5000 && price.currency == "RUB" && !isLargeDenominations
      case Loan(_, rating) =>
        rating > 5
      case _ => false
    }

  println(isSafePayment(DebitCard(Price(5, "EUR"), MasterCard)))
  // true
  println(isSafePayment(Cash(Price(10000, "EUR"), isLargeDenominations = true)))
  // false
}
