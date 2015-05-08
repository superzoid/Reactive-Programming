package calculator

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._

import TweetLength.MaxTweetLength

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  /******************
   ** TWEET LENGTH **
   ******************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))

    val mini = " \uD83D\uDCA9"
    val result3 = TweetLength.tweetRemainingCharsCount(Var(mini))
    assert(result3() == MaxTweetLength - tweetLength(mini))
  }

  test("should be red less than zero") {
    val red = TweetLength.colorForRemainingCharsCount(Signal(-1))()
    assert(red == "red")
  }

  test("should be green between 15-140"){
    val maxCount = TweetLength.colorForRemainingCharsCount(Signal(140))()
    assert(maxCount == "green")
    val minCount = TweetLength.colorForRemainingCharsCount(Signal(15))
    assert(maxCount == "green")
  }

  test("should be orange between 0-14"){
    val maxCount = TweetLength.colorForRemainingCharsCount(Signal(14))()
    assert(maxCount == "orange")
    val minCount = TweetLength.colorForRemainingCharsCount(Signal(0))
    assert(maxCount == "orange")
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }

  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }

  test(" polynomial of x^2 + 1") {
    val d = Polynomial.computeDelta(Var(1), Var(0), Var(1))
    assert(d() == -4)
    val result = Polynomial.computeSolutions(Var(1), Var(0), Var(1), d)
    assert(result() == Set())
  }

  test(" polynomial of x^2 - 1") {
    val delta = Polynomial.computeDelta(Var(1), Var(0), Var(-1))
    assert(delta() == 4)
    val result = Polynomial.computeSolutions(Var(1), Var(0), Var(1), delta)
    assert(result() == Set(1, -1))
  }

  test(" test 1-3-1") {
    val delta = Polynomial.computeDelta(Var(1), Var(3), Var(1))
    assert(delta() == 5)
    val result = Polynomial.computeSolutions(Var(1), Var(3), Var(1), delta)
    assert(result() == Set(-0.3819660112501051, -2.618033988749895))
  }

}
