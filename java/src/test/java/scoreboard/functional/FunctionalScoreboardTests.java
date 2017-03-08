package scoreboard.functional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CountDownLatch;

import org.junit.jupiter.api.Test;

import io.reactivex.Observable;

public class FunctionalScoreboardTests {

	@Test
	void initialScoreShouldBe000to000() throws InterruptedException {
		Observable<String> lines = Observable.empty();
		Observable<String> observable = FScoreboardApp.process(new FScoreboard(), lines);
		List<String> messages = new ArrayList<>();
		observable.subscribe(messages::add);
		assertEquals(1, messages.size());
		assertEquals("000:000", messages.get(0));
	}
}
