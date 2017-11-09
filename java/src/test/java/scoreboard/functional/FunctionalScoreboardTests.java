package scoreboard.functional;

import java.util.*;

import io.reactivex.Observable;
import org.junit.jupiter.api.*;

import static org.junit.jupiter.api.Assertions.*;

public class FunctionalScoreboardTests {

	@Test
	void initialScoreShouldBe000to000() throws InterruptedException {
		Observable<String> lines = Observable.empty();
		Observable<String> observable = FScoreboardApp.process(new FScoreboard(), lines);
		List<String> messages = new ArrayList<>();
		observable.subscribe(messages::add);
		assertMessages(messages, "000:000");
	}

	@Test @Disabled
	void scoreTeamA() throws InterruptedException {
		Observable<String> lines = Observable.just("a", "+");
		Observable<String> observable = FScoreboardApp.process(new FScoreboard(), lines);
		List<String> messages = new ArrayList<>();
		observable.subscribe(messages::add);
		assertMessages(messages, "000:000", "Team A selected", "001:000");
	}

	private void assertMessages(List<String> messages, String... expectedMessages) {
		assertEquals(expectedMessages.length, messages.size(), "# of messages");
		for (int i = 0; i < expectedMessages.length; i++) {
			assertEquals(expectedMessages[i], messages.get(0));
		}
	}

}
