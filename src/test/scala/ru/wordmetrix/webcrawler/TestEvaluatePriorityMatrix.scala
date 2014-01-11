package ru.wordmetrix.webcrawler

import java.net.URI
import scala.concurrent.duration.DurationInt
import org.scalatest.{ BeforeAndAfterAll, Matchers, WordSpecLike }
import Gather.{ GatherIntel, GatherLinkContext, GatherPage, GatherSeeds }
import akka.actor.ActorSystem
import akka.testkit.{ DefaultTimeout, ImplicitSender, TestKit, TestProbe }
import ru.wordmetrix.utils.CFG
import ru.wordmetrix.vector.Vector
import ru.wordmetrix.webcrawler.LinkContext.FeatureName
import akka.actor.Props
import akka.actor.Actor

class TestEvaluatePriorityMatrix extends TestKit(ActorSystem("TestEvalutatePriorityMatrix"))
        with DefaultTimeout with ImplicitSender
        with WordSpecLike with Matchers with BeforeAndAfterAll {

    override def afterAll(): Unit = {
        system.shutdown()
    }

    import EvaluatePriorityMatrix._
    import SeedQueue._
    import Gather._
    import Storage._
    import SampleHierarchy2Priority._

    val cfg = CFG(List("-d","-ts","2","-tl","0.005"))
    def uri(n: Int) = new URI(s"http://example.org/${n}")

    def xml(n: Int) = <html><body>
                                <a href="http://en.wikipedia.org/${n+1}">
                                    Test Test Test Test Test Test
                                </a>
                            </body></html>

    def TestActor() = {
        val actor = TestProbe()
        (actor, Props(new Actor {
            def receive = { case msg => actor.ref forward msg }
        }))
    }

    "A queue" should {
        "init a process" in {
            val (storage, storageprop) = TestActor()
            val (gather, gatherprop) = TestActor()
            val (seedqueue, seedqueueprop) = TestActor()
            val (sample, sampleprop) = TestActor()

            val queue = system.actorOf(
                EvaluatePriorityMatrix.props(storageprop, gatherprop, seedqueueprop, sampleprop, cfg),
                "TestEvaluatePriority_1")

            // Initial seed is sent     
            queue ! EvaluatePriorityMatrixSeed(uri(1))

            // Initial phase
            seedqueue.expectMsg(SeedQueueRequest(uri(1)))

            // Targeting phase
            gather.send(queue, GatherSeeds(uri(1), Set(uri(2), uri(3), uri(4), uri(5), uri(6), uri(7)), Vector("test" -> 2.0)))

            seedqueue.expectMsg(SeedQueueRequest(uri(2)))

            seedqueue.expectMsg(SeedQueueRequest(uri(3)))

            seedqueue.expectMsg(SeedQueueRequest(uri(4)))

            seedqueue.expectMsg(SeedQueueRequest(uri(5)))

            seedqueue.expectMsg(SeedQueueRequest(uri(6)))
            
            seedqueue.expectMsg(SeedQueueRequest(uri(7)))

            storage.expectMsg(StorageSign(uri(1)))

            gather.send(queue, GatherSeeds(uri(2), Set(uri(4), uri(5)), Vector("test" -> 2.0, "test2" -> 4.0)))

            storage.expectMsg(StorageSign(uri(2)))

            gather.send(queue, GatherSeeds(uri(3), Set(uri(6), uri(7)), Vector("test" -> 2.0, "test3" -> 3.0)))

            storage.expectMsg(StorageSign(uri(3)))

            gather.send(queue, GatherSeeds(uri(4), Set(uri(4), uri(5)), Vector("test" -> 2.0, "test4" -> 2.0)))

            gather.send(queue, GatherSeeds(uri(5), Set(uri(6), uri(7)), Vector("test" -> 2.0, "test5" -> 1.0)))

            storage.expectMsg(StorageSign(uri(5)))

            // Estimation phase
            gather.send(queue, GatherSeeds(uri(6), Set(uri(6), uri(7)), Vector("test" -> 2.0, "test6" -> 0.5)))

            gather.send(queue, GatherSeeds(uri(7), Set(uri(6), uri(7)), Vector("test" -> 2.0, "test7" -> 0.25)))
            
            //sample.expectMsg(1)
        }
    }
}